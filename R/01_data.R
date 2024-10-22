data_birth_g <- read.xlsx("data/birth_count_per_subgroup_1987_2022.xlsx",detectDates = TRUE) %>%
  mutate(birth = paste0(birth_Y_M,"-01"),
         birth =ymd(birth),
         Year = year(birth),
         Year = as.factor(Year),
         Month = month(birth)) %>%
  rename(total_birth=n) %>%
  select(-birth_Y_M)


data_birth_1871 <- read.xlsx("data/FSO_published_livebriths_per_months_1871_2023.xlsx",detectDates = TRUE) %>%
  filter(Year <1987 | Year==2023) %>%
  mutate(birth = ymd(paste0(Year,"-", Month,"-01")),
         Year = as.factor(Year)) %>%
  select(-Count_IEM) %>%
  mutate(Canton ="Switzerland",
         Citizenship="total")


data_birth <- bind_rows(data_birth_g, data_birth_1871) %>%
  select(-Canton, -Citizenship)

data_pop_total <- read.xlsx("data/population.xlsx") %>%
  gather(.,Year, population, `1860`:`2023`) %>%
  filter(Year > 1870) %>%
  filter(Alter == "Alter - Total") %>%
  filter(Geschlecht=="Geschlecht - Total") %>%
  mutate(Canton ="Switzerland",
         Citizenship="total") %>%
  rename(Sex=  Geschlecht,
         Age = Alter) %>%
  mutate(Sex="total",
         Age ="total")

data_pop_female <- read.xlsx("data/population.xlsx") %>%
  gather(.,Year, population, `1860`:`2023`) %>%
  filter(Year > 1870) %>%
  filter(Geschlecht=="Frau") %>%
  mutate(Alter = substr(Alter,1, 2),
         Alter = as.numeric(Alter)) %>%
  filter(Alter >14 & Alter < 50) %>%
  group_by(Year, Geschlecht) %>%
  summarise(population = sum(population)) %>%
  ungroup() %>%
  rename(Sex=Geschlecht) %>%
  mutate(Age="15-49",
         Sex ="female",
         Canton ="Switzerland",
         Citizenship="total") 

data_pop_total <- data_pop_total %>%
  rbind(data_pop_female)


data_pop2 <- read.xlsx("data/Population_1987_2022.xlsx") %>%
  rename(Citizenship = `Citizenship.(category)`) %>%
  mutate(Year=as.character(Year),
         Citizenship= recode(Citizenship, "Citizenship (category) - total" = "total")) %>%
  filter(!Age== "Age - total") %>%
  filter(!Age == "99 years or older") %>%
  filter(Sex=="Female") %>%
  mutate(Age = substr(Age,1, 2),
         Age = as.numeric(Age)) %>%
  filter(Age %in% 15:49) %>%
  group_by(Year, Canton,  Citizenship) %>%
  summarise(population = sum(Population.on.31.December)) %>%
  ungroup() %>%
  mutate(Age="15-49",
         Sex ="female") %>%
  filter(!(Canton =="Switzerland" & Citizenship=="total" & Sex=="female"))




# data_pop_citizen <- data_pop2 %>%
#   rename(population= `Population.on.31.December`) %>%
#   filter(Canton =="Switzerland") 


data_pop_language <- data_pop2 %>%
  # filter(!Canton =="Switzerland") %>%
  # filter(Citizenship == "total") %>%
  mutate(
         Canton = recode(Canton,
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
  # filter(!Language == "No indication") %>%
  group_by(Year,Canton,Citizenship, Age, Sex) %>%
  summarise(population = sum(population)) %>%
  ungroup() %>%
  filter(!(Canton =="Switzerland" & Citizenship=="total" & Sex=="female"))

data_pop <- data_pop2 %>%
  filter(Canton %in% "Switzerland") %>%
  full_join(data_pop_total) %>%
  full_join(data_pop_language)

       
data_total <- data_birth %>%
  full_join(data_pop, by="Year") %>%
  select(-birth) %>%
  mutate(birth_ratio = males/females) %>%
  select(Year, Month, total_birth, Sex, Age, population,	Canton,	Citizenship,	females,	males,
         parity_1,	parity_2,	parity_sup2,	swiss,	non_swiss,	german_romansh,	french,	italy,	mat_age_below_30,	mat_age_above_or_eq_30,	birth_ratio) %>%
arrange(Year)

write.xlsx( data_total,"data/data_total.xlsx")

save(data_total,file="data/data_total.RData")