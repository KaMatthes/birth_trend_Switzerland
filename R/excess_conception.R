
function_inla_total <- function(Year_max, Year_min) {
  
  load("data/data_conception.RData")
  
  dat.excess <- data_conception %>%
    mutate(Year = as.numeric(Year)) %>%
    filter(Year >=Year_min & Year <=Year_max ) %>%
    rename(birth_var = total_birth,
           denominator = population)
  
  
  year_smooth <- 6
  year_from <- min(dat.excess$Year)
  year_reg <- year_from + year_smooth
  
  
  control.family <- inla.set.control.family.default()
  
  hyper.iid <- list(theta = list(prior="pc.prec", param=c(1, 0.01)))

  formula <- birth_var~ 1 + offset(log(denominator))  +
    timeID +
    as.factor(MonthID2) +
    f(timeID2, model='rw1',scale.model = T,cyclic = TRUE, hyper=hyper.iid)
  
  expected_birth <- list()
  
  for (YEAR in year_reg:Year_max){
    
    print(YEAR)
    
    
      reg_data <-  dat.excess %>% 
        filter(Year >= YEAR+1 - year_smooth & Year < YEAR+1) %>%
        mutate(birth_var =ifelse (Year ==YEAR, NA,birth_var)) %>% 
        arrange(Year, Month) %>%
        group_by(Year,Month) %>%
        mutate(timeID = cur_group_id(),
               seasID = timeID) %>%
        arrange(timeID) %>%
        ungroup() %>%
        mutate(MonthID = Month,
               MonthID2 = Month,
               YearID = Year,
               timeID2 = timeID)
  
    
    set.seed(20231020)
    
    inla.mod <- inla(formula,
                     data=reg_data,
                     family="nbinomial",
                     control.family = control.family,
                     control.compute = list(config = TRUE,dic=TRUE),
                     control.mode = list(restart = TRUE),
                     control.predictor = list(compute = TRUE, link = 1))

    post.samples <- inla.posterior.sample(n = 1000, result = inla.mod, seed=20220421)
    predlist <- do.call(cbind, lapply(post.samples, function(X)
      exp(X$latent[startsWith(rownames(X$latent), "Pred")])))
    
    rate.drawsMed <-array(unlist( predlist), dim=c(dim(reg_data)[1], 1000)); dim(rate.drawsMed) 
    dM = as.data.frame(rate.drawsMed)
    # Add to the data and save
    Data= cbind(reg_data,dM)
    
    mean.samples <- Data %>%
      select(starts_with("V"), "Month", "Year") %>%
      rowwise(Month) %>%
      mutate(fit = median(c_across(V1:V1000)),
             LL = quantile(c_across(V1:V1000), probs= 0.025),
             UL = quantile(c_across(V1:V1000), probs= 0.975)) %>%
      select(Month, fit, LL, UL, Year) %>%
      filter(Year==YEAR) %>%
      arrange(Year, Month) %>%
      left_join(dat.excess, by=c("Year", "Month"))
    
    expected_birth[[YEAR]] <-  mean.samples
    expected_birth <- expected_birth[-which(sapply( expected_birth, is.null))] 
    
  }
  
  expected_birth <- expected_birth %>%
    bind_rows(., .id = "column_label")
  
  write.xlsx( expected_birth,paste0("data/expected_conception_inla_month.xlsx"), rowNames=FALSE, overwrite = TRUE)
  save( expected_birth,file=paste0("data/expected_conception_inla_month.RData"))
}

function_inla_total(Year_max=2022, Year_min=2010)
