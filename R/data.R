data_birth <- read.xlsx("data/birth_num.xlsx",detectDates = TRUE) %>%
  mutate(Year = year(birth),
         Year = as.factor(Year),
         Month = month(birth))

data_pop_total <- read.xlsx("data/population.xlsx") %>%
  gather(.,Year, population, `1860`:`2022`) %>%
  filter(Year > 1986) %>%
  filter(Alter == "Alter - Total") %>%
  filter(Geschlecht=="Geschlecht - Total")


data_pop_rep_women <- read.xlsx("data/population.xlsx") %>%
  gather(.,Year, population, `1860`:`2022`) %>%
  filter(Year > 1986) %>%
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

data_tmp_t <- data.frame(Year=rep(seq(1987,2022,1),11), population=rep(NA,396),Month=rep(seq(2,12,1),36),
                         Alter=rep("Alter - Total", 396), Geschlecht=rep("Geschlecht - Total", 396)) %>%
  mutate(Year = as.factor(Year))


data_tmp_w <- data.frame(Year=rep(seq(1987,2022,1),11), population=rep(NA,396),Month=rep(seq(2,12,1),36),
                         Alter=rep("15-49", 396), Geschlecht=rep("Frau", 396)) %>%
  mutate(Year = as.factor(Year))

pop_month_total <- data_pop_total %>%
  mutate(Month=1) %>%
  full_join(data_tmp_t) %>%
  arrange(Year, Month) %>%
  mutate(pop.monthly = round(zoo::na.approx(population, na.rm=FALSE),0),
         pop.monthly = ifelse(Year==2022 ,8815385, pop.monthly))


pop_month_women <- data_pop_rep_women %>%
  mutate(Month=1) %>%
  full_join(data_tmp_w) %>%
  arrange(Year, Month) %>%
  mutate(pop.monthly = round(zoo::na.approx(population, na.rm=FALSE),0),
         pop.monthly = ifelse(Year==2022 ,1936043, pop.monthly))

data_pop <- rbind(pop_month_total, pop_month_women) %>%
  mutate(Year=as.factor(Year))
       


data_total <- data_birth %>%
  left_join(data_pop)

write.xlsx( data_total,"data/data_total.xlsx")

save(data_total,file="data/data_total.RData")