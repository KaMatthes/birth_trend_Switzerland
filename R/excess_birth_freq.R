function_freq_total <- function(Sex) {
load("data/data_total.RData")


data_total <- data_total %>%
  filter(Geschlecht==Sex) %>%
  select(birth, Year, Month, count,pop.monthly) %>%
  mutate(Year=as.numeric(as.character(Year))) %>%
  mutate(si_one = sin(2*pi*Month/12),
         si_two = sin(4*pi*Month/12),
         co_one = cos(2*pi*Month/12),
         co_two = cos(4*pi*Month/12),
         Month2 = as.factor(Month))


year_smooth <- 5 # Anzahl der Jahre vor dem Jahr, was geschätzt werden soll
year_from <- min(data_total$Year)
year_reg <- year_from + year_smooth
year_max <- max(data_total$Year)

expected_birth <- list()

for (YEAR in year_reg:year_max) {
  print(YEAR)


reg_data <-  data_total %>% 
  filter(Year >= YEAR+1 - year_smooth & Year < YEAR+1)

Model_year <- glm(count ~ Year +si_one + co_one,
                  family=poisson,offset=log(pop.monthly), data=  reg_data )

pred_data <-  data_total %>% 
  filter(Year==YEAR) 

boot_pi <- function(model, pdata, n, p) {
  set.seed(20231023)
  odata <- model$data
  lp <- (1 - p) / 2
  up <- 1 - lp
  boot_y <- foreach(i = 1:n, .combine = rbind) %dopar% {
    bdata <- odata[sample(seq(nrow(odata)), size = nrow(odata), replace = TRUE), ]
    bpred <- predict(update(model, data = bdata), type = "response", newdata = pdata)
    rpois(length(bpred), lambda = bpred)
  }
  boot_ci <- t(apply(boot_y, 2, quantile, c(lp, up)))
  return(data.frame(pred = predict(model, newdata = pdata, type = "response"), 
                    lower = boot_ci[, 1], upper = boot_ci[, 2]))
}

predict <- boot_pi( Model_year , pred_data, 1000, 0.975)

pred_results <- pred_data %>%
  mutate(
    pred = predict$pred,
    lower = predict$lower,
    upper = predict$upper) %>%
  select(Year, Month, birth,pop.monthly, count, pred, lower, upper) %>%
  filter(Year==YEAR) %>%
  arrange(Year, Month) 


expected_birth[[YEAR]] <-  pred_results
expected_birth <- expected_birth[-which(sapply(expected_birth, is.null))] 

}

expected_birth <- expected_birth %>%
  bind_rows(., .id = "column_label")

write.xlsx( expected_birth,paste0("data/expected_birth_freg_month_",Sex,".xlsx"), rowNames=FALSE, overwrite = TRUE)
save( expected_birth,file=paste0("data/expected_birth_freq_month_",Sex,".RData"))

}

function_freq_total(Sex="Geschlecht - Total")
function_freq_total(Sex="Frau")


