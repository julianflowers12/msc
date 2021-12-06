require(here)
require(tidyverse)
require(janitor)

# set path
path <- here::here("data")

# list files
files <- list.files(path, pattern = "rda", full.names = T)

# read

load(files[[1]]) 
load(files[[2]])
load(files[[3]])

d1 <- d1 %>% clean_names %>% dplyr::select(-c(starts_with("individual"), start_date_day, start_date_month, contains("end"), contains("organism")))
d2 <- d2 %>% clean_names %>% dplyr::select(-c(starts_with("individual"), start_date_day, start_date_month, contains("end"), contains("organism")))
d3 <- d3 %>% clean_names %>% dplyr::select(-c(starts_with("individual"), start_date_day, start_date_month, contains("end"), contains("organism")))

df <- bind_rows(d1, d2, d3)
