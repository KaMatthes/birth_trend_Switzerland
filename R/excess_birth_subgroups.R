
varBirth="total_birth"
varPop="population"
Year_max=1895
Year_min=1880
pop_group="female"
CitGroup="total"
CanGroup="Switzerland"
AgeGroup = "15-49"
Year_pan = 1890


quantiles <- c(0.025,0.1,0.5,0.9,0.975)

function_inla_total <- function(varBirth,varPop,Year_Pan,Year_max, Year_min,pop_group, CitGroup, CanGroup, AgeGroup) {
  
  load("data/data_total.RData")
  
  dat.excess <- data_total %>%
    filter(Sex==pop_group) %>%
    filter(Citizenship==CitGroup) %>%
    filter(Canton== CanGroup) %>%
    filter(Age==  AgeGroup) %>%
    select(eval(substitute(varBirth)),Year, Month,eval(substitute(varPop))) %>%
    mutate(Year=as.numeric(as.character(Year)),
           birth = ymd(paste0(Year,"-", Month,"-01")))%>%
    filter(Year >=Year_min & Year <=Year_max ) %>%
    rename(birth_var = eval(substitute(varBirth)),
           denominator = eval(substitute(varPop)))
  
  
  year_smooth <- 3
  year_from <- min(dat.excess$Year)
  year_reg <- year_from + year_smooth
  
  
  control.family <- inla.set.control.family.default()
  
  hyper.iid <- list(theta = list(prior="pc.prec", param=c(1, 0.01)))
  
  
  formula <- birth_var ~ 1 + offset(log(denominator))  + as.factor(MonthID2) +
    f(timeID2, model='rw1',scale.model = T,cyclic = TRUE, hyper=hyper.iid) +
    f(factor(obs), model = "iid",hyper= hyper.iid)
  
  expected_birth <- list()
  
  for (YEAR in Year_min:Year_max){
    
    print(YEAR)
    
    
    if (YEAR==Year_Pan) {
      reg_data <-  dat.excess %>% 
        
        filter(Year >= YEAR - year_smooth & Year <= YEAR + year_smooth ) %>%
        # filter(Year >= YEAR - year_smooth & Year <= YEAR) %>%
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
               timeID2 = timeID,
               obs = timeID)
    }
    
    else{
      
      reg_data <-  dat.excess %>%
        filter(Year >= YEAR - year_smooth & Year <= YEAR + year_smooth ) %>%
        # filter(Year >= YEAR - year_smooth & Year <= YEAR) %>%
        mutate(birth_var =ifelse (Year ==YEAR, NA,birth_var)) %>%
        filter(!Year == Year_Pan) %>%
        arrange(Year, Month) %>%
        group_by(Year,Month) %>%
        mutate(timeID = cur_group_id(),
               seasID = timeID) %>%
        arrange(timeID) %>%
        ungroup() %>%
        mutate(MonthID = Month,
               MonthID2 = Month,
               YearID = Year,
               timeID2 = timeID,
               obs = timeID)
    }
    
    set.seed(20231020)
    
    inla.mod <- inla(formula,
                     data=reg_data,
                     family="poisson",
                     control.family = control.family,
                     control.compute = list(config = TRUE,dic=TRUE),
                     control.mode = list(restart = TRUE),
                     quantiles = quantiles,
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
  
  write.xlsx( expected_birth,paste0("data/expected_birth_inla_month_",varBirth,"_",pop_group,"_",Year_Pan,".xlsx"), rowNames=FALSE, overwrite = TRUE)
  write_rds( expected_birth,file=paste0("data/expected_birth_inla_month_",varBirth,"_",pop_group,"_",Year_Pan,".rds"))
}


function_inla_total(varBirth="total_birth",varPop="population",Year_Pan=1890,Year_max=1900, Year_min=1880,pop_group="female",CitGroup="total", CanGroup="Switzerland",AgeGroup = "15-49")
function_inla_total(varBirth="total_birth",varPop="population",Year_Pan=1915,Year_max=1927, Year_min=1907,pop_group="female",CitGroup="total", CanGroup="Switzerland",AgeGroup = "15-49")
function_inla_total(varBirth="total_birth",varPop="population",Year_Pan=1958,Year_max=1967, Year_min=1947,pop_group="female",CitGroup="total", CanGroup="Switzerland",AgeGroup = "15-49")
function_inla_total(varBirth="total_birth",varPop="population",Year_Pan=1970,Year_max=1979, Year_min=1959,pop_group="female",CitGroup="total", CanGroup="Switzerland",AgeGroup = "15-49")
function_inla_total(varBirth="total_birth",varPop="population",Year_Pan=2010,Year_max=2019, Year_min=1999,pop_group="female",CitGroup="total", CanGroup="Switzerland",AgeGroup = "15-49")
function_inla_total(varBirth="total_birth",varPop="population",Year_Pan=2021,Year_max=2022, Year_min=2007,pop_group="female",CitGroup="total", CanGroup="Switzerland",AgeGroup = "15-49")


function_inla_total(varBirth="parity_1",varPop="population",Year_Pan=2021,Year_max=2022, Year_min=2013,pop_group="female", CitGroup="total", CanGroup="Switzerland", AgeGroup = "15-49")
function_inla_total(varBirth="parity_2",varPop="population",Year_Pan=2021,Year_max=2022, Year_min=2013,pop_group="female", CitGroup="total", CanGroup="Switzerland", AgeGroup = "15-49")
function_inla_total(varBirth="parity_sup2",varPop="population",Year_Pan=2021, Year_max=2022, Year_min=2013,pop_group="female",CitGroup="total", CanGroup="Switzerland",AgeGroup = "15-49")

# function_inla_total(varBirth="total_birth",varPop="population",Year_max=2023, Year_min=2010,pop_group="total",CitGroup="total", CanGroup="Switzerland",AgeGroup = "total")


function_inla_total(varBirth="mat_age_below_30",varPop="population",Year_Pan=2021,Year_max=2022, Year_min=2013,pop_group="female", CitGroup="total", CanGroup="Switzerland",AgeGroup = "15-49")
function_inla_total(varBirth="mat_age_above_or_eq_30",varPop="population",Year_Pan=2021,Year_max=2022, Year_min=2013,
                    pop_group="female",CitGroup="total", CanGroup="Switzerland",AgeGroup = "15-49")


function_inla_total(varBirth="german_romansh",varPop="population",Year_Pan=2021,Year_max=2022, Year_min=2013,pop_group="female",CitGroup="total", CanGroup="German",AgeGroup = "15-49")
function_inla_total(varBirth="french",varPop="population",Year_Pan=2021,Year_max=2022, Year_min=2013,pop_group="female",CitGroup="total", CanGroup="French",AgeGroup = "15-49")
function_inla_total(varBirth="italy",varPop="population",Year_Pan=2021,Year_max=2022, Year_min=2013,pop_group="female",CitGroup="total", CanGroup="Italian",AgeGroup = "15-49")

function_inla_total(varBirth="swiss",varPop="population",Year_Pan=2021,Year_max=2022, Year_min=2013,pop_group="female",CitGroup="Switzerland", CanGroup="Switzerland",AgeGroup = "15-49")
function_inla_total(varBirth="non_swiss",varPop="population",Year_Pan=2021,Year_max=2022, Year_min=2013,pop_group="female",CitGroup="Foreign country", CanGroup="Switzerland",AgeGroup = "15-49")
