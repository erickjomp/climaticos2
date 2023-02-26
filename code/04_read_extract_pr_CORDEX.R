library(tidyverse)
library(terra)
library(sf)
Sys.setenv(TZ = 'GMT')

#### INPUT ####
file_wshed__shp <- "data/watersheds/shp/pativilca_watershed.shp"
paths_pr_CORDEX <-
  c(
    historical = 
      "/run/media/oem/TOSHIBA-EXT/global_data/CORDEX_projected_peru/SAM44i_peru/daily/historical/pr/",
    rcp45 = 
      "/run/media/oem/TOSHIBA-EXT/global_data/CORDEX_projected_peru/SAM44i_peru/daily/rcp45/pr/",
    rcp85 = 
      "/run/media/oem/TOSHIBA-EXT/global_data/CORDEX_projected_peru/SAM44i_peru/daily/rcp85/pr/"
  )

#### OUTPUT ####
file_series0_pr_CORDEX_monthly__rds <- 
  "output/04_extraction_CORDEX_series/df_pr_CORDEX_m0.rds"
file_series0_pr_CORDEX_monthly__csv <- 
  "output/04_extraction_CORDEX_series/df_pr_CORDEX_m0.csv"
file_series0_pr_CORDEX_yearly__rds <- 
  "output/04_extraction_CORDEX_series/df_pr_CORDEX_y0.rds"
file_series0_pr_CORDEX_yearly__csv <- 
  "output/04_extraction_CORDEX_series/df_pr_CORDEX_y0.csv"

file_plot_pr_series_yearly <- 
  "output/04_extraction_CORDEX_series/plots/plot_series_pr_CORDEX_y.pdf"


#### processing ####

# reading watershed shape
sf_wshed <- st_read(file_wshed__shp)

# reading & extracting CORDEX 
files_pr_CORDEX <-
  lapply(paths_pr_CORDEX,list.files, full.names = TRUE) %>% 
  do.call(c,.)

df_pr_CORDEX_d <-
  lapply(files_pr_CORDEX, function(file_pr){
    message(which(file_pr == files_pr_CORDEX)," / ",
            length(files_pr_CORDEX),"\r", appendLF = FALSE)
    
    ras_pr_cordex <- rast(file_pr)
    values_pr <-
      terra::extract(ras_pr_cordex,
                     sf_wshed,
                     fun = "mean",
                     weights = TRUE)[, -1] %>%
      as.numeric()
    df_pr <- tibble(dates = time(ras_pr_cordex),
                     pr = values_pr * 24 * 60 * 60)
    
    # extracting meta information
    basefile_pr <- basename(file_pr)
    basefile_pr_splitted <- strsplit(basefile_pr, split = "_")
    rcm <- basefile_pr_splitted[[1]][6]
    gcm <- basefile_pr_splitted[[1]][3]
    scenario <- basefile_pr_splitted[[1]][4]
    
    df_pr <- df_pr %>%
      mutate(
        RCM = rcm,
        GCM = gcm,
        name = paste(RCM, GCM, sep = "_"),
        scenario = scenario,
        .before = 1
      )
  }) %>% do.call(rbind,.)


df_pr_CORDEX_m <- df_pr_CORDEX_d %>% 
  mutate(dates = trunc(dates, "months")) %>% 
  group_by(RCM,GCM,name,scenario, dates) %>% summarise(pr = sum(pr)) %>% 
  ungroup()

df_pr_CORDEX_y <- df_pr_CORDEX_m %>% 
  mutate(dates = trunc(dates, "years")) %>% 
  group_by(RCM,GCM,name, scenario, dates) %>% summarise(pr = sum(pr)) %>% 
  ungroup()


#### writing output ####
saveRDS(df_pr_CORDEX_m, file_series0_pr_CORDEX_monthly__rds)
write.csv(df_pr_CORDEX_m, file_series0_pr_CORDEX_monthly__csv,
          quote = FALSE, row.names = FALSE)
saveRDS(df_pr_CORDEX_y, file_series0_pr_CORDEX_yearly__rds)
write.csv(df_pr_CORDEX_y, file_series0_pr_CORDEX_yearly__csv,
          quote = FALSE, row.names = FALSE)


#### extra exploratory plot ####
df_pr_CORDEX_y %>% 
  ggplot(aes(dates, pr, col = scenario)) +
  geom_line() + 
  facet_wrap(~name) +
  ggthemes::theme_few(base_size = 8) + 
  labs(x = NULL, y = "Precipitation (mm/year)") +
  labs(color = "Scenario") +
  scale_color_discrete(
    labels = c("Historical","RCP 4.5","RCP 8.5")) +
  theme(legend.position = c(0.9,0.10 ),
        panel.grid.major = element_line(color = "grey80"))

ggsave(filename = file_plot_pr_series_yearly,
       width = 190, height = 120, units = "mm")


