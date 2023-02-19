library(tidyverse)
library(sf)
Sys.setenv(TZ = 'GMT')

#### INPUT ####
file_pr_series <- "data/meteorological_data//df_pr_m.csv"
file_stas_pr__shp <- "data/meteorological_data/shp/sf_stas_pr.shp"
file_wshed__shp <- "data/watersheds/shp/pativilca_watershed.shp"
buffer_distance <- 25000 # in meters, buffer to extend watershed boundary

#### OUTPUT ####
file_pr_series_selected__rds <- "output/00_processed_data//df_pr_m.rds"
file_pr_series_selected__csv <- "output/00_processed_data//df_pr_m.csv"
file_stas_pr_selected__shp <- "output/00_processed_data/shp/sf_stas_pr.shp"


#### reading input ####
df_pr_m__global <- read_csv(file_pr_series) %>%
  mutate(dates = as.POSIXct(dates))
sf_stas_pr__global <- st_read(file_stas_pr__shp)
sf_wshed <- st_read(file_wshed__shp)


#### processing ####
# get extended area
sf_wshed_buf <- sf_wshed %>% st_buffer(buffer_distance)

# plots
plot(sf_stas_pr__global$geometry)
plot(sf_wshed$geometry, border = "blue", add = T)
plot(sf_wshed_buf$geometry, border = "red", add = T)

# select stas and data
sf_stas_pr <- sf_stas_pr__global[sf_wshed_buf, ]
df_pr_m <- df_pr_m__global %>% select(dates, sf_stas_pr$namcod)

#### writing output ####
saveRDS(df_pr_m, file_pr_series_selected__rds)
write.csv(
  df_pr_m,
  file_pr_series_selected__csv,
  row.names = F,
  quote = F
)
st_write(sf_stas_pr,
         file_stas_pr_selected__shp,
         delete_dsn = T)


#### EXTRA ####

df_pr_m %>% gather(sta, pr,-1) %>%
  ggplot(aes(dates, pr)) + geom_line() +
  facet_wrap( ~ sta) +
  theme_bw() +
  labs(x = NULL, y = "Precipitation (mm/month)")


library(tmap)
tm_shape(sf_wshed_buf) + tm_borders() +
  tm_shape(sf_wshed) + tm_borders() +
  tm_shape(sf_stas_pr) + tm_dots(col = "red",
                                 shape = 20,
                                 size = 0.5) +
  tm_text("name", ymod = 0.5, col = "blue")

