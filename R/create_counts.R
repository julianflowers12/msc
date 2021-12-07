### simplify data - counts by site by year - and 
require(tidyverse); require(sf);require(gam);require(gamm4);require(poptrend)
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

## global count

year_count <- df1 %>%
  count(year)


### hectad counts by year

hectad_count <- df1 %>% 
  count(hectad, year)

### transect counts by year

transect_count <- df1 %>%
  count(transect, year, sort = TRUE) 

transect_wider <- transect_count %>%
  arrange(year) %>%
  pivot_wider(names_from = "year", values_from = "n", values_fill = 0) %>%
  arrange(-`1990`)

transect_wider %>%
  rowwise(transect) %>%
  mutate(total = sum(c_across(2:30)),
         mean = mean(c_across(2:30))) %>%
  select(total, mean) %>%
  arrange(-total)

transect_count_filt <- transect_count %>%   ## sampled in at least 10 years
  add_count(transect) %>%
  filter(nn > 9) %>%
  drop_na()

transect_count_filt_20 <- transect_count %>%   ## sampled in at least 20 years
  add_count(transect) %>%
  filter(nn > 19) %>%
  drop_na()

mod_transect_count <- transect_count_filt_20 %>%
  nest_by(transect)

mods <- mod_transect_count %>%
  mutate(mod = list(bam(n ~ s(year), family = poisson, data = data, method = 'fREML', nthreads = 8, discrete = T)))

mods <- mods %>% mutate(pred = list(predict(mod, data)))
mods <- mods %>% mutate(est = list(exp(pred)))
fit_sig <- mods %>%
  summarise(broom::tidy(mod)) %>%
  filter(p.value < 0.05)

x <- 10
title <- mods$transect[x]

mods_filt <- mods %>%
  filter(transect %in% fit_sig$transect) ## 671 transects with at least 20 years observations and significant trend (up or down)

mods_plots <- mods_filt %>% 
  mutate(plot = list(plot(mod)))


pt_mods <- mods_filt %>%
  unnest("data")

library(poptrend)
tictoc::tic()
poptfit <- ptrend(n ~ trend(year, type = "smooth", tempRE = TRUE, fx = TRUE) + transect, data = pt_mods)
tictoc::toc()


summary(poptfit) 
                    

