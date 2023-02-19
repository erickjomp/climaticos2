library(tidyverse)
library(terra)
library(sf)
Sys.setenv(TZ = 'GMT')
devtools::load_all("~/projects/my_packages/basic_tools/")

#### INPUT ####
# data netCDF (.nc) downloaded from  
# https://iridl.ldeo.columbia.edu/SOURCES/.SENAMHI/.HSR/.PISCO/index.html
file_ETP_nc <- 
  "~/projects/global_data/PISCO/PET_monthly__hargreaves-samani/data.nc"
file_wshed_shp <- "data/watersheds/shp/pativilca_watershed.shp"

#### OUTPUT ####
file_df_etp_m_csv <- "output/00_processed_data/df_ETP_m.csv"
file_df_etp_m_rds <- "output/00_processed_data/df_ETP_m.rds"


#### reading INPUT ####
sf_wshed <- st_read(file_wshed_shp)
sv_wshed <- vect(sf_wshed)

ras_ETP <- rast(file_ETP_nc)

#### Extracting ####
values_ETP <- terra::extract(ras_ETP, sf_wshed, 
                             fun = "mean", weights = TRUE)[,-1] %>% 
  as.numeric()
df_etp_m <- tibble(dates =seq.POSIXt(as.POSIXct("1981-01-01"),
                                     as.POSIXct("2016-12-01"), by = "months"),
                   etp = values_ETP)


#### explotratory plots ####
plot(df_etp_m, type = "b")
df_etp_m %>% to_years() %>% plot(type = "b")
df_etp_m %>% to_years() %>% summarise_all(mean)

#### writing output ####
write.csv(df_etp_m,
          file_df_etp_m_csv,
          row.names = FALSE,
          quote = FALSE)
saveRDS(df_etp_m, file_df_etp_m_rds)