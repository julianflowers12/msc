### zonal stats

library(raster); library(sf); library(exactextractr);library(tmap)
rs <- read_rds("raster_stack.rds")
load("~/Dropbox/Mac (2)/Desktop/msc/shapes.RData")

s1 <- st_cast(density7090, "MULTIPOLYGON")
s2 <- st_cast(density2010, "MULTIPOLYGON")

density(log2(rs[[7]]))

names(rs) <- c("lc1990", "lc2007", "lc2015", "Blue tit", "GS Woodpecker", "Mean temp", "Precipitation")

pairs(rs)


lc_classes <- data.frame(
  id = 0:10, 
  class = c(NA, "Broadleaf woodland", "Coniferous woodland", "Arable", "Improved grassland", 
            "Semi-natural grassland", "Mountain, heath, bog", "Saltwater", "Freshwater", 
            "Coastal", "Built-up areas and gardens")
  
  
)
colours()
lc_class_palette <- c()

units::set_units(sum(st_area(s1)), km^2)
s1[[1]]

lc1990x <- rs[[1]]
lc2000x <- rs[[2]]
lc2010x <- rs[[3]]

raster::extract(lc1990x, s1, fun = "mean", na.rm = TRUE)

as(s1, "Spatial")
class(density7090)

modal_land_cover_7090 <- exactextractr::exact_extract(lc2010x, density7090) %>%
  map_dfr(., bind_rows)

modal_land_cover_2000 <- exactextractr::exact_extract(lc2000x, density2000, "mode")


modal_land_cover_2010 <- exactextractr::exact_extract(lc2010x, density2010 ) %>%
  map_dfr(., bind_rows)

landcover_dist_2010 <- modal_land_cover_2010 %>%
  mutate(fracTot = coverage_fraction/ sum(coverage_fraction)) %>%
  group_by(value) %>%
  summarise(freq = 100 * sum(fracTot))

landcover_dist_7090 <- modal_land_cover_7090 %>%
  mutate(fracTot = coverage_fraction/ sum(coverage_fraction)) %>%
  group_by(value) %>%
  summarise(freq = 100 *sum(fracTot)) 

landcover_dist_2010 %>%
  left_join(landcover_dist_7090, by = "value") %>%
  left_join(lc_classes, by = c("value" = "id")) %>%
  rename(now = freq.x, past = freq.y) %>%
  mutate(diff = now - past,
         change = ifelse(diff > 0, "up", "down")) %>%
  drop_na() %>%
  ggplot(aes(reorder(class, diff), diff, fill = change)) + 
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("red", "blue")) +
  coord_flip() +
  theme(panel.background = element_blank(), 
        panel.border = element_rect(fill = NA), 
        axis.text = element_text(size = rel(1.5)),
        axis.title = element_text(size = rel(1.6)),
        plot.title.position = "plot", 
        plot.title = element_text(size = rel(2))) +
  labs(y = "% change in land cover", 
       x = '', 
       caption = "Based on 2015 land cover domnant classes per km2", 
       title = "Land cover change in core Willow Tit territories 1970-1999 and 2010-19")

  

exactextractr::exact_extract(lc2010x, s2, function(df){
  df %>%
    mutate(frac_total = coverage_fraction / sum(coverage_fraction)) %>%
             group_by(name, value) %>%
             summarize(freq = sum(frac_total))
},
summarise_df = TRUE)


stable <- st_intersection(density7090, density2010)
loss <- st_difference(density7090, density2010)
gain <- st_difference(density2010, density7090)
gain == loss

plot(overlaps)


tm_shape(lc1990x) +
  tm_raster(n = 10, palette = viridis::turbo(10, direction = -1), title = "Land cover 1990") +
  tm_shape(gb) +
  tm_polygons(alpha = 0) +
  tm_shape(density2010) +
  tm_polygons(alpha = 0.3, col = "black", lwd = 2) +
  tm_layout(frame = F) +
  tm_compass(position = c("left", "top")) +
  tm_scale_bar(position = c("right", "bottom" )) +
  tm_legend(legend.outside = TRUE, legend.position = c("left", "centre"))

p7090 <- tm_shape(density7090) +
  tm_fill() +
  tm_shape(gb) +
  tm_polygons(alpha = 0)

p2010 <- tm_shape(density2010) +
  tm_fill() +
  tm_shape(gb) +
  tm_polygons(alpha = 0)


p_gain <- tm_shape(gain, st_bbox = gb) +
  tm_fill("blue") +
  tm_shape(gb) +
  tm_polygons(alpha = 0)

p_stable <- tm_shape(stable) +
  tm_fill("green") +
  tm_shape(gb) +
  tm_polygons(alpha = 0)

tmap_arrange(p_gain, p2010, p_stable, p7090)
