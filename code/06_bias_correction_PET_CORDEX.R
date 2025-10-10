library(tidyverse)
library(hyfo)
library(climQMBC)
Sys.setenv(TZ = 'GMT')

#### INPUT ####
file_series_PET_CORDEX <-
  "output/05_selection_CORDEX_series/df_PET_CORDEX_m.rds"

file_periods_analysis <-
  "output/06_bias_correction_CORDEX_series/input/periods_analysis_start_end.csv"

file_serie_PET_pisco <-
  "output/00_processed_data/df_ETP_m.rds"
 

#### OUTPUT ####
file_series_PET_CORDEX_BC__rds <-
  "output/06_bias_correction_CORDEX_series//df_PET_CORDEX_m_BC.rds"
file_series_PET_CORDEX_BC__csv <-
  "output/06_bias_correction_CORDEX_series//df_PET_CORDEX_m_BC.csv"

#### reading INPUT ####
df_PET_CORDEX <- readRDS(file_series_PET_CORDEX)

df_periods_analysis <- read.csv(file_periods_analysis,
                                header = FALSE,
                                row.names = 1) %>% t() %>% 
  as_tibble() %>%   mutate_all(as.POSIXct) 
print(t(df_periods_analysis))

df_PET <- readRDS(file_serie_PET_pisco) # PISCO PET will be observed PET

#### Bias Correction PET ####
df_PET__obs <- df_PET %>% 
  filter(dates >= df_periods_analysis$date_begin_historic_PET,
         dates < df_periods_analysis$date_end_historic_PET)

df_PET_CORDEX <- df_PET_CORDEX %>% 
  filter(dates >= df_periods_analysis$date_begin_historic_PET,
         dates < df_periods_analysis$date_end_ref_future)

# loop 
df_PET_CORDEX_BC <- 
df_PET_CORDEX %>% group_by(RCM, GCM) %>% 
  group_split() %>% 
  lapply(function(df_PET_CORDEX_model){
    message(" | ", appendLF = FALSE)
    ## arrange done to be able to use SDM function of climQMBC package
    
    list_BC <- list()
    for (sce_rcp in c("rcp45", "rcp85")){
      df_PET_CORDEX_model_sce <- 
        df_PET_CORDEX_model %>% 
        filter(scenario  %in% c("historical", sce_rcp))
      
      # climQMBC
      values_BC <- climQMBC::QDM(obs = df_PET__obs$etp,
                                 mod = df_PET_CORDEX_model_sce$PET,
                                 mult_change = 1,
                                 frq = "m")
      
      df_PET_CORDEX_model_sce_BC <- df_PET_CORDEX_model_sce
      df_PET_CORDEX_model_sce_BC$PET <- values_BC[,1]

      list_BC[[sce_rcp]] <- df_PET_CORDEX_model_sce_BC
    }
    # since both are the same hist are same we join#
    df_PET_CORDEX_model_BC <- list_BC %>% do.call(rbind,.) %>% 
      filter(!duplicated(dates) | (scenario != "historical"))
    return(df_PET_CORDEX_model_BC)
  }) %>% do.call(rbind,.)

#### writing output ####
saveRDS(df_PET_CORDEX_BC ,file_series_PET_CORDEX_BC__rds)
write.csv(df_PET_CORDEX_BC, file_series_PET_CORDEX_BC__csv, 
          quote = FALSE, row.names = FALSE)


#### extra plots ####

df_PET_CORDEX_BC %>% 
  ggplot(aes(dates, PET, col = scenario)) + 
  geom_line() +
  facet_wrap(~name) +
  ggthemes::theme_few() + 
  labs(x = NULL, y = "PET (mm/month)") +
  geom_line(data = df_PET__obs %>% rename(PET= etp) %>% 
              mutate(dates = as.Date(dates)), 
            col = "black")
  
