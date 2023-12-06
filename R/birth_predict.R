
function_inla_total <- function(varBirth,varPop,Year_max, Year_pre, Year_min,pop_group, CitGroup, CanGroup, AgeGroup) {
  
  load("data/data_total.RData")
  
  dat.excess <- data_total %>%
    filter(Geschlecht==pop_group) %>%
    filter(Citizenship==CitGroup) %>%
    filter(Canton== CanGroup) %>%
    filter(Alter==  AgeGroup) %>%
    select(eval(substitute(varBirth)),Year, Month,eval(substitute(varPop))) %>%
    mutate(Year=as.numeric(as.character(Year)),
           birth = ymd(paste0(Year,"-", Month,"-01")))%>%
    filter(Year >=Year_min & Year <=Year_max ) %>%
    rename(birth_var = eval(substitute(varBirth)),
           denominator = eval(substitute(varPop))) %>%
    mutate(birth_pre =birth_var,
           birth_pre = ifelse(Year > Year_pre, NA, birth_pre))
  

  
  control.family <- inla.set.control.family.default()
  
  hyper.iid <- list(theta = list(prior="pc.prec", param=c(1, 0.01)))

  formula <- birth_pre~ 1 + offset(log(denominator))  +
    timeID +
    as.factor(MonthID2) +
    f(timeID2, model='rw1',scale.model = T,cyclic = TRUE, hyper=hyper.iid)

      reg_data <-  dat.excess %>% 
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
    
    fitted_birth <- Data %>%
      select(starts_with("V"), "Month", "Year") %>%
      rowwise(Month) %>%
      mutate(fit = median(c_across(V1:V1000)),
             LL = quantile(c_across(V1:V1000), probs= 0.025),
             UL = quantile(c_across(V1:V1000), probs= 0.975)) %>%
      select(Month, fit, LL, UL, Year) %>%
      arrange(Year, Month) %>%
      left_join(dat.excess, by=c("Year", "Month"))

  
  write.xlsx( fitted_birth,paste0("data/predicted_birth_inla_month",varBirth,"_",pop_group,".xlsx"), rowNames=FALSE, overwrite = TRUE)
  save( fitted_birth,file=paste0("data/predicted_birth_inla_month",varBirth,"_",pop_group,".RData"))
}


function_inla_total(varBirth="total_birth",varPop="population",Year_max=2023, Year_min=2016,Year_pre=2020,pop_group="total",CitGroup="total", CanGroup="Switzerland",AgeGroup = "total")
function_inla_total(varBirth="total_birth",varPop="population",Year_max=2023, Year_min=2016,Year_pre=2020, pop_group="female",CitGroup="total", CanGroup="Switzerland",AgeGroup = "15-49")

