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
  filter(Geschlecht=="Geschlecht - Total")


data_pop2 <- read.xlsx("data/Population_1987_2022.xlsx") %>%
  rename(Geschlecht = Sex) %>%
  rename(Alter = Age) %>%
  rename(Citizenship = `Citizenship.(category)`) %>%
  mutate(Year=as.character(Year),
         Citizenship= recode(Citizenship, "Citizenship (category) - total" = "total")) %>%
  filter(Alter == "Age - total") %>%
  filter(Geschlecht=="Sex - total")  %>%
  mutate(Alter=recode(Alter, "Age - total" = "Alter - Total"),
         Geschlecht = recode(Geschlecht, "Sex - total" = "Geschlecht - Total")) 
  
data_pop_citizen <- data_pop2 %>%
  rename(population= `Population.on.31.December`) %>%
  filter(Canton =="Switzerland") 


data_pop_language <- data_pop2 %>%
  rename(population = `Population.on.31.December`) %>%
  # filter(!Canton =="Switzerland") %>%
  filter(Citizenship == "total") %>%
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
  group_by(Year,Canton) %>%
  mutate(population = sum(population)) %>%
  ungroup() %>%
  distinct(Year, Canton, .keep_all = TRUE)


data_pop_rep_women <- read.xlsx("data/population.xlsx") %>%
  gather(.,Year, population, `1860`:`2023`) %>%
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


data_pop_rep_women30 <- read.xlsx("data/population.xlsx") %>%
  gather(.,Year, population, `1860`:`2023`) %>%
  filter(Year > 1870) %>%
  filter(!Alter == "Alter - Total") %>%
  filter(!Alter == "99 Jahre und mehr") %>%
  filter(Geschlecht=="Frau") %>%
  mutate(Alter = substr(Alter,1, 2),
         Alter = as.numeric(Alter)) %>%
  filter(Alter >14 & Alter < 30) %>%
  group_by(Year) %>%
  mutate(population = sum(population)) %>%
  ungroup() %>%
  distinct(Year, .keep_all=TRUE) %>%
  select(Year, Geschlecht,population) %>%
  mutate(Alter="15-29") 


data_pop_rep_women30_49 <- read.xlsx("data/population.xlsx") %>%
  gather(.,Year, population, `1860`:`2023`) %>%
  filter(Year > 1870) %>%
  filter(!Alter == "Alter - Total") %>%
  filter(!Alter == "99 Jahre und mehr") %>%
  filter(Geschlecht=="Frau") %>%
  mutate(Alter = substr(Alter,1, 2),
         Alter = as.numeric(Alter)) %>%
  filter(Alter >29 & Alter < 50) %>%
  group_by(Year) %>%
  mutate(population = sum(population)) %>%
  ungroup() %>%
  distinct(Year, .keep_all=TRUE) %>%
  select(Year, Geschlecht,population) %>%
  mutate(Alter="30-49") 

pop_month_total <- data_pop_total %>%
  mutate(Canton ="Switzerland",
         Citizenship="total")

pop_month_women <- rbind(data_pop_rep_women,data_pop_rep_women30, data_pop_rep_women30_49) %>%
  mutate(Canton ="Switzerland",
         Citizenship="total")


data_pop <- rbind(pop_month_total, pop_month_women) %>%
  mutate(Year=as.factor(Year)) %>%
  full_join(data_pop_citizen) %>%
  full_join(data_pop_language)
       
data_total <- data_birth %>%
  full_join(data_pop) %>%
  select(-birth) %>%
  mutate(birth_ratio = males/females) %>%
  select(Year, Month, total_birth, Sex=Geschlecht, Age=Alter, population,	Canton,	Citizenship,	females,	males,
         parity_1,	parity_2,	parity_sup2,	swiss,	non_swiss,	german_romansh,	french,	italy,	mat_age_below_30,	mat_age_above_or_eq_30,	birth_ratio) %>%
  mutate(Sex=recode(Sex, "Geschlecht - Total" = "total",
                    "Frau" = "female"),
         Ag=recode(Age, "Alter - Total" = "total"))

write.xlsx( data_total,"data/data_total.xlsx")

save(data_total,file="data/data_total.RData")