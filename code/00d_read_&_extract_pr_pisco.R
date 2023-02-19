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

#### reading input ####
ras_pisco_pr_m <- rast(file_pisco_pr_m)
sf_wshed <- st_read(file_wshed__shp)

#### extraction ####
values_pr <- 
  extract(ras_pisco_pr_m, sf_wshed, fun = "mean",weights=FALSE)[,-1] %>% 
  as.numeric()

df_pr_pisco <- 
  tibble(
    dates = seq.POSIXt(as.POSIXct("1981-01-01"), as.POSIXct("2016-12-01"),
                       by = "months"),
    PISCO = values_pr
  )

#### wriyinh output ####
saveRDS(df_pr_pisco, file_df_pr_m__pisco_rds)
write.csv(df_pr_pisco, file_df_pr_m__pisco_csv, 
          quote = FALSE, row.names = FALSE)
