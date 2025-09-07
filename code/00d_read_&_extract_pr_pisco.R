library(tidyverse)
library(terra)
library(sf)
Sys.setenv(TZ = 'GMT')

#### INPUT ####
file_pisco_pr_m <- "~/projects/global_data/PISCO/pr_monthly/data.nc"
file_wshed__shp <- "data/watersheds/shp/pativilca_watershed.shp"

#### OUTPUT ####
file_df_pr_m__pisco_rds <-
  "output/00_processed_data/df_pr_m__pisco.rds"
file_df_pr_m__pisco_csv <-
  "output/00_processed_data/df_pr_m__pisco.csv"
file_ras_avg_prannual__pisco <- 
  "output/00_processed_data/raster/ras_avg_prannual__pisco.tiff"

#### reading input ####
ras_pisco_pr_m <- rast(file_pisco_pr_m)
sf_wshed <- st_read(file_wshed__shp)

#### extraction time series ####
values_pr <- 
  extract(ras_pisco_pr_m, sf_wshed, fun = "mean",weights=FALSE)[,-1] %>% 
  as.numeric()

df_pr_pisco <- 
  tibble(
    dates = seq.POSIXt(as.POSIXct("1981-01-01"), as.POSIXct("2016-12-01"),
                       by = "months"),
    PISCO = values_pr
  )

#### extraction average spatial annual precip ####
ras_avg_prannual <-  terra::crop(ras_pisco_pr_m,buffer(vect(sf_wshed), 30000)) %>% mean
ras_avg_prannual <-  12 *ras_avg_prannual
plot(ras_avg_prannual)
sf_wshed$geometry %>% plot(add=T)

#### wriyinh output ####
saveRDS(df_pr_pisco, file_df_pr_m__pisco_rds)
write.csv(df_pr_pisco, file_df_pr_m__pisco_csv, 
          quote = FALSE, row.names = FALSE)

ras_avg_prannual %>% terra::writeRaster(file_ras_avg_prannual__pisco, overwrite=TRUE)
