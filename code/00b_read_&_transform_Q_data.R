library(tidyverse)
devtools::load_all("~/projects/my_packages/basic_tools/")
Sys.setenv(TZ = 'GMT')

#### INPUT ####
path_files <- "data/streamflow_data/"

file_Q_d_series__rds <- "output/00_processed_data/df_Q_d.rds"
file_Q_d_series__csv <- "output/00_processed_data/df_Q_d.csv"

file_Q_m_series__rds <- "output/00_processed_data/df_Q_m.rds"
file_Q_m_series__csv <- "output/00_processed_data/df_Q_m.csv"

file_stas_Q__shp <- "output/00_processed_data/shp/sf_stas_Q.shp"


#### reading input  and processing ####
files <- list.files(path_files, full.names = TRUE)

list_df_Q_d <- list()
list_df_Q_m <- list()
list_df_stas <- list()

for (file in files) {
  # file <- "temp/data/SerieTiempo24012023000739.xlsx"
  df_Q_d <- readxl::read_excel(file, skip = 17) %>%
    mutate(dates = as.POSIXct(FECHA, format = "%d/%m/%Y"),
           Q = `VALOR (m³/s)`) %>%
    select(dates, Q)
  
  df_Q_m <- df_Q_d %>%
    complete_dates(fill_strip = "months") %>%
    to_months(mean_c, na_max = 6)
  
  df_sta_raw <- readxl::read_excel(file, n_max = 8, col_names = NA)
  
  name_sta <-
    df_sta_raw[[1]][1] %>% sub("Estación ", "", .) %>% strsplit(" ") %>%
    .[[1]] %>% .[1]
  
  coords_sta <-
    df_sta_raw[[2]][5] %>% strsplit(" ") %>% .[[1]] %>% .[c(2, 5, 8)]
  
  variable <- df_sta_raw[[1]][2]
  operador <- df_sta_raw[[2]][4]
  
  names(df_Q_d)[2] <- name_sta
  names(df_Q_m)[2] <- name_sta
  
  df_sta <- tibble::tibble(
    name = name_sta,
    variable,
    lat = coords_sta[1],
    lon = coords_sta[2],
    elevation = coords_sta[3],
    operator = operador
  )
  
  list_df_Q_d[[name_sta]] <-  df_Q_d
  list_df_Q_m[[name_sta]] <-  df_Q_m
  list_df_stas[[name_sta]] <-  df_sta
}

df_Q_d <- list_df_Q_d %>% Reduce(full_join, .) %>% arrange(dates)
df_Q_m <- list_df_Q_m %>% Reduce(full_join, .) %>% arrange(dates)
df_stas_Q <- list_df_stas %>% do.call(rbind, .)

sf_stas_Q <-
  df_stas_Q %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)

#### writing output ####
saveRDS(df_Q_d, file_Q_d_series__rds)
write.csv(df_Q_d,
          quote = F,
          row.names = F,
          file = file_Q_d_series__csv)
saveRDS(df_Q_m, file_Q_m_series__rds )
write.csv(df_Q_m,
          quote = F,
          row.names = F,
          file = file_Q_m_series__csv )

sf_stas_Q %>% st_write(file_stas_Q__shp,
                       delete_dsn = TRUE)
