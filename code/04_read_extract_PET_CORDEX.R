library(tidyverse)
library(terra)
library(sf)
devtools::load_all("~/projects/my_packages/basic_tools/")
Sys.setenv(TZ = 'GMT')

#### INPUT ####
file_wshed__shp <- "data/watersheds/shp/pativilca_watershed.shp"
paths_PET_CORDEX <-
  c(
    historical = 
      "/run/media/oem/TOSHIBA-EXT/global_data/CORDEX_projected_peru/SAM44i_peru/daily/historical/evspsblpot/",
    rcp45 = 
      "/run/media/oem/TOSHIBA-EXT/global_data/CORDEX_projected_peru/SAM44i_peru/daily/rcp45/evspsblpot/",
    rcp85 = 
      "/run/media/oem/TOSHIBA-EXT/global_data/CORDEX_projected_peru/SAM44i_peru/daily/rcp85/evspsblpot/"
  )
  
#### OUTPUT ####
file_series0_PET_CORDEX_monthly__rds <- 
  "output/04_extraction_CORDEX_series/df_PET_CORDEX_m0.rds"
file_series0_PET_CORDEX_monthly__csv <- 
  "output/04_extraction_CORDEX_series/df_PET_CORDEX_m0.csv"
file_series0_PET_CORDEX_yearly__rds <- 
  "output/04_extraction_CORDEX_series/df_PET_CORDEX_y0.rds"
file_series0_PET_CORDEX_yearly__csv <- 
  "output/04_extraction_CORDEX_series/df_PET_CORDEX_y0.csv"

file_plot_PET_series_yearly <- 
  "output/04_extraction_CORDEX_series/plots/plot_series_PET_CORDEX_y.pdf"


#### processing ####

# reading watershed shape
sf_wshed <- st_read(file_wshed__shp)

# reading & extracting CORDEX 
files_PET_CORDEX <-
  lapply(paths_PET_CORDEX,list.files, full.names = TRUE) %>% 
  do.call(c,.)

df_PET_CORDEX_d <-
  lapply(files_PET_CORDEX, function(file_pet){
    message(which(file_pet == files_PET_CORDEX)," / ",
            length(files_PET_CORDEX),"\r", appendLF = FALSE)
    
    ras_pet_cordex <- rast(file_pet)
    values_PET <-
      terra::extract(ras_pet_cordex,
                     sf_wshed,
                     fun = "mean",
                     weights = TRUE)[, -1] %>%
      as.numeric()
    df_PET <- tibble(dates = time(ras_pet_cordex),
                     PET = values_PET * 24 * 60 * 60)
    
    # extracting meta information
    basefile_pet <- basename(file_pet)
    basefile_pet_splitted <- strsplit(basefile_pet, split = "_")
    rcm <- basefile_pet_splitted[[1]][6]
    gcm <- basefile_pet_splitted[[1]][3]
    scenario <- basefile_pet_splitted[[1]][4]
    
    df_PET <- df_PET %>%
      mutate(
        RCM = rcm,
        GCM = gcm,
        name = paste(RCM, GCM, sep = "_"),
        scenario = scenario,
        .before = 1
      )
  }) %>% do.call(rbind,.)


df_PET_CORDEX_m <- df_PET_CORDEX_d %>% 
  mutate(dates = trunc(dates, "months")) %>% 
  group_by(RCM,GCM,name,scenario, dates) %>% summarise(PET = sum(PET)) %>% 
  ungroup()

df_PET_CORDEX_y <- df_PET_CORDEX_m %>% 
  mutate(dates = trunc(dates, "years")) %>% 
  group_by(RCM,GCM,name, scenario, dates) %>% summarise(PET = sum(PET)) %>% 
  ungroup()


#### writing output ####
saveRDS(df_PET_CORDEX_m, file_series0_PET_CORDEX_monthly__rds)
write.csv(df_PET_CORDEX_m, file_series0_PET_CORDEX_monthly__csv,
          quote = FALSE, row.names = FALSE)
saveRDS(df_PET_CORDEX_y, file_series0_PET_CORDEX_yearly__rds)
write.csv(df_PET_CORDEX_y, file_series0_PET_CORDEX_yearly__csv,
          quote = FALSE, row.names = FALSE)


#### extra exploratory plot ####
df_PET_CORDEX_y %>% 
  ggplot(aes(dates, PET, col = scenario)) +
  geom_line() + 
  facet_wrap(~name) +
  ggthemes::theme_few(base_size = 8) + 
  labs(x = NULL, y = "PET (mm/year)") +
  labs(color = "Scenario") +
  scale_color_discrete(
    labels = c("Historical","RCP 4.5","RCP 8.5")) +
  theme(legend.position = c(0.9,0.15 ))
  
ggsave(filename = file_plot_PET_series_yearly,
       width = 190, height = 90, units = "mm")


