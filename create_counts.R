### simplify data - counts by site by year - and 
require(tidyverse); require(sf)
source("R/data_prep.R")


## select fields
df1 <- df %>%
  dplyr::select(hectad = osgr_10km, 
         transect = osgr_1km, 
         year =  start_date_year, 
         x = longitude_wgs84, 
         y = latitude_wgs84, 
         scientific_name, 
         common_name, 
         error = coordinate_uncertainty_m, 
         occ = occurrence_status, 
         status = identification_verification_status) %>%
  drop_na(transect, year, x, y) %>%
  filter(occ == "present", 
         str_detect(status, "Accepted"), 
         nchar(hectad) > 3, 
         nchar(transect) > 0)

range(df1$year)

## create spatial data fraome
df_sf_4326 <- df1 %>%
  st_as_sf(., coords = c("x", "y"), crs = 4326)  ## in degrees

df_sf_27700 <- df_sf_4326 %>%
  st_transform(., 27700)                         ## in metres


### hectad counts by year

hectad_count <- df1 %>% 
  count(hectad, year)

### transect counts by year

transect_count <- df1 %>%
  count(transect, year, sort = TRUE) 

transect_count %>%
  arrange(year) %>%
  pivot_wider(names_from = "year", values_from = "n", values_fill = 0) %>%
  arrange(-`1990`)

View(transect_count)
  