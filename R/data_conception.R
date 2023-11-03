data_con <- read.xlsx("data/birth_count_per_conception_date_subgroup_2007_2022.xlsx",detectDates = TRUE) %>%
  select(conception_month,n) %>%
  mutate(birth = paste0(conception_month,"-01"),
         birth =ymd(birth),
         Year = year(birth),
         Month = month(birth)) %>%
  rename(total_birth=n) %>%
  select(-conception_month) %>%
  filter(Year > 2009 &  Year < 2022) %>%
  mutate(Year = as.factor(Year))

data_pop_total <- read.xlsx("data/population.xlsx") %>%
  gather(.,Year, population, `1860`:`2022`) %>%
  filter(Year > 2009) %>%
  filter(Alter == "Alter - Total") %>%
  filter(Geschlecht=="Geschlecht - Total") %>%
  select(Year, population)
  

data_conception <- data_con %>%
  full_join(data_pop_total) %>%
  filter(!is.na(birth))

write.xlsx(data_conception,"data/data_conception.xlsx")

save(data_conception,file="data/data_conception.RData")