data_birth_g <- read.xlsx("data/birth_count_per_subgroup_1987_2022.xlsx",detectDates = TRUE) %>%
  mutate(birth = paste0(birth_Y_M,"-01"),
         birth =ymd(birth),
         Year = year(birth),
         Year = as.factor(Year),
         Month = month(birth)) %>%
  rename(total_birth=n) %>%
  select(-birth_Y_M)


data_birth_1871 <- read.xlsx("data/FSO_published_livebriths_per_months_1871_2023.xlsx",detectDates = TRUE) %>%
  mutate(birth = ymd(paste0(Year,"-", Month,"-01")),
         Year = as.factor(Year)) %>%
  select(-Count_IEM)


data_birth <- bind_rows(data_birth_g, data_birth_1871)

data_pop_total <- read.xlsx("data/population.xlsx") %>%
  gather(.,Year, population, `1860`:`2022`) %>%
  filter(Year > 1870) %>%
  filter(Alter == "Alter - Total") %>%
  filter(Geschlecht=="Geschlecht - Total")


data_pop_citizen <- read.xlsx("data/Population_1987_2022.xlsx") %>%
  rename(population.citizen = `Population.on.31.December`) %>%
  rename(Geschlecht = Sex) %>%
  rename(Alter = Age) %>%
  mutate(Year=as.character(Year)) %>%
  filter(Canton =="Switzerland") %>%
  filter(Alter == "Age - total") %>%
  filter(Geschlecht=="Sex - total") %>%
  filter(!`Citizenship.(category)` == "Citizenship (category) - total")


data_pop_language <- read.xlsx("data/Population_1987_2022.xlsx") %>%
  rename(population = `Population.on.31.December`) %>%
  rename(Geschlecht = Sex) %>%
  rename(Alter = Age) %>%
  mutate(Year=as.character(Year)) %>%
  filter(!Canton =="Switzerland") %>%
  filter(Alter == "Age - total") %>%
  filter(Geschlecht=="Sex - total") %>%
  filter(`Citizenship.(category)` == "Citizenship (category) - total") %>%
  mutate(Language = Canton,
         Language = recode(Language,
                           "Zürich" = "German",
                           "Bern / Berne" = "German",
                           "Luzern" ="German",
                           "Uri" = "German",
                           "Schwyz" = "German",
                           "Obwalden" = "German",
                           "Nidwalden" = "German",
                           "Glarus" = "German",
                           "Zug" = "German",
                           "Solothurn" = "German",
                           "Basel-Stadt" = "German",
                           "Basel-Landschaft" = "German",
                           "Schaffhausen" = "German",
                           "Appenzell Ausserrhoden" = "German",
                           "Appenzell Innerrhoden" = "German",
                           "St. Gallen" = "German",
                           "Graubünden / Grigioni / Grischun" = "German",
                           "Aargau" = "German",
                           "Thurgau" = "German",
                           "Ticino" = "Italian",
                           "Fribourg / Freiburg" ="French",
                           "Valais / Wallis" ="French",
                           "Vaud" ="French",
                           "Neuchâtel" ="French",
                           "Genève" ="French",
                           "Jura" ="French")) %>%
  filter(!Language == "No indication") %>%
  group_by(Year,Language) %>%
  mutate(pop.language = sum(population)) %>%
  ungroup()


data_pop_rep_women <- read.xlsx("data/population.xlsx") %>%
  gather(.,Year, population, `1860`:`2022`) %>%
  filter(Year > 1870) %>%
  filter(!Alter == "Alter - Total") %>%
  filter(!Alter == "99 Jahre und mehr") %>%
  filter(Geschlecht=="Frau") %>%
  mutate(Alter = substr(Alter,1, 2),
         Alter = as.numeric(Alter)) %>%
  filter(Alter >14 & Alter < 50) %>%
  group_by(Year) %>%
  mutate(population = sum(population)) %>%
  ungroup() %>%
  distinct(Year, .keep_all=TRUE) %>%
  select(Year, Geschlecht,population) %>%
  mutate(Alter="15-49") 

# data_tmp_t <- data.frame(Year=rep(seq(1871,2022,1),11), population=rep(NA,1672),Month=rep(seq(2,12,1),152),
#                          Alter=rep("Alter - Total", 1672), Geschlecht=rep("Geschlecht - Total", 1672)) %>%
#   mutate(Year = as.factor(Year))
# 
# 
# data_tmp_w <- data.frame(Year=rep(seq(1871,2022,1),11), population=rep(NA,1672),Month=rep(seq(2,12,1),152),
#                          Alter=rep("15-49", 1672), Geschlecht=rep("Frau", 1672)) %>%
#   mutate(Year = as.factor(Year))

pop_month_total <- data_pop_total 
# %>%
#   mutate(Month=1) 
  # full_join(data_tmp_t) %>%
  # arrange(Year, Month) %>%
  # mutate(pop.monthly = round(zoo::na.approx(population, na.rm=FALSE),0),
  #        pop.monthly = ifelse(Year==2022 ,8815385, pop.monthly))


pop_month_women <- data_pop_rep_women 
# %>%
#   mutate(Month=1) 
  # full_join(data_tmp_w) %>%
  # arrange(Year, Month) %>%
  # mutate(pop.monthly = round(zoo::na.approx(population, na.rm=FALSE),0),
  #        pop.monthly = ifelse(Year==2022 ,1936043, pop.monthly))

data_pop <- rbind(pop_month_total, pop_month_women) %>%
  mutate(Year=as.factor(Year)) %>%
  full_join(data_pop_citizen) %>%
  full_join(data_pop_language)
       
data_total <- data_birth %>%
  full_join(data_pop)

write.xlsx( data_total,"data/data_total.xlsx")

save(data_total,file="data/data_total.RData")