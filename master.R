library(tidyverse)
library(lubridate)
library(openxlsx)
library(INLA)
library(zoo)
library(scales)
library(ggsci)
library(conflicted)

conflict_prefer("rename", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("arrange", "dplyr")
conflict_prefer("group_by", "dplyr")
conflict_prefer("summarise", "dplyr")

col_pal <- pal_jco()(8)


