
function_inla_total <- function(Year_max, Year_min,Sex) {
  
  load("data/data_total.RData")
  
  dat.excess <- data_total %>%
    filter(Geschlecht==Sex) %>%
    select(birth, Year, Month, count,pop.monthly) %>%
    mutate(Year=as.numeric(as.character(Year)))%>%
    filter(Year >=Year_min & Year <=Year_max )
  
  
  year_smooth <- 5
  year_from <- min(dat.excess$Year)
  year_reg <- year_from + year_smooth
  
  
  control.family <- inla.set.control.family.default()
  
  hyper.iid <- list(theta = list(prior="pc.prec", param=c(1, 0.01)))
  # 
  # formula <- death ~ 1 + offset(log(pop.monthly))  +  as.factor(Month) +
  #   f(YearID, model='iid',hyper=hyper.iid) +
  #   # f(MonthID, model='iid',hyper=hyper.iid) +
  #   f(timeID, model='rw1',scale.model = T,cyclic = TRUE, hyper=hyper.iid)
  # # f(timeID, model='seasonal',season.length=12)
  

  formula <- count ~ 1 + offset(log(pop.monthly))  +
    timeID +
    as.factor(MonthID2) +
    # YearID+
     f(YearID, model='ar1',hyper=hyper.iid) +
    # f(MonthID, model='ar1',hyper=hyper.iid) +
    # f(timeID2, model='ar1',hyper=hyper.iid, constr=T) 
    # f(seasID, model='seasonal', season.length =12)
    f(timeID2, model='rw1',scale.model = T,cyclic = TRUE, hyper=hyper.iid)
  # f(timeID, model='seasonal',season.length=12)
  
  
  # formula <- count ~ 1 + offset(log(pop.monthly))  + 
  #   f(MonthID, model='iid',hyper=hyper.iid) +
  #   f(seasID, model='seasonal', season.length =12) +
  #   f(timeID, model='rw1',scale.model = T,cyclic = TRUE, hyper=hyper.iid) 
  
  expected_birth <- list()
  
  for (YEAR in year_reg:Year_max){
    
    print(YEAR)
    
    # if (YEAR==Year_Pan) {
    #   reg_data <-  dat.excess %>%
    #     filter(Year >= YEAR+1 - year_smooth & Year < YEAR+1)%>%
    #     mutate(death=ifelse (Year ==YEAR, NA, death))  %>%
    #     filter(!Year==1918) %>%
    #     arrange(Year, Month) %>%
    #     group_by(Year,Month) %>%
    #     mutate(timeID = cur_group_id(),
    #            seasID = timeID) %>%
    #     arrange(timeID) %>%
    #     ungroup() %>%
    #     mutate(MonthID = Month,
    #            YearID = Year)
    # }
    # 
   
      reg_data <-  dat.excess %>% 
        filter(Year >= YEAR+1 - year_smooth & Year < YEAR+1)%>%
        mutate(count=ifelse (Year ==YEAR, NA, count)) %>% 
        # filter(!Year == Year_Pan)  %>% 
        # filter(!Year==1918) %>%
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
                     # family="nbinomial",
                     family="Poisson",
                     # family = "zeroinflatednbinomial1",
                     #verbose = TRUE,
                     control.family = control.family,
                     control.compute = list(config = TRUE,dic=TRUE),
                     control.mode = list(restart = TRUE),
                     # num.threads = round(parallel::detectCores() * .2),
                     # verbose=TRUE,
                     control.predictor = list(compute = TRUE, link = 1))
    
    
    # inla.mod$summary.random$t %>% 
    #   ggplot() +
    #   geom_line(aes(ID, mean)) +
    #   geom_ribbon(aes(ID, ymin = `0.025quant`, ymax = `0.975quant`), alpha = 0.3)
    
    post.samples <- inla.posterior.sample(n = 1000, result = inla.mod, seed=20220421)
    predlist <- do.call(cbind, lapply(post.samples, function(X)
      exp(X$latent[startsWith(rownames(X$latent), "Pred")])))
    
    rate.drawsMed <-array(unlist( predlist), dim=c(dim(reg_data)[1], 1000)); dim(rate.drawsMed) 
    dM = as.data.frame(rate.drawsMed)
    # Add to the data and save
    Data= cbind(reg_data,dM)
    
    mean.samples <- Data %>%
      select(starts_with("V"), "Month", "Year", "count", "pop.monthly") %>%
      rowwise(Month) %>%
      mutate(fit = median(c_across(V1:V1000)),
             LL = quantile(c_across(V1:V1000), probs= 0.025),
             UL = quantile(c_across(V1:V1000), probs= 0.975)) %>%
      select(Month, fit, LL, UL, Year, count,pop.monthly) %>%
      filter(Year==YEAR) %>%
      arrange(Year, Month) %>%
      left_join(dat.excess, by=c("Year", "Month"))
    # 
    
    
    # write.xlsx( expected_birth,paste0("data/expected_death_inla_month",Year_Pan,".xlsx"), rowNames=FALSE, overwrite = TRUE)
    # save( expected_birth,file=paste0("data/expected_death_inla_month",Year_Pan,".RData"))
    
    expected_birth[[YEAR]] <-  mean.samples
    expected_birth <- expected_birth[-which(sapply( expected_birth, is.null))] 
    
  }
  
  expected_birth <- expected_birth %>%
    bind_rows(., .id = "column_label")
  
  write.xlsx( expected_birth,paste0("data/expected_birth_inla_month_",Sex,".xlsx"), rowNames=FALSE, overwrite = TRUE)
  save( expected_birth,file=paste0("data/expected_birth_inla_month_",Sex,".RData"))
}





function_inla_total(Year_max=2022, Year_min=1987,Sex="Geschlecht - Total")


function_inla_total(Year_max=2022, Year_min=1987, Sex="Frau")




