library(tidyverse)
library(hyfo)
library(climQMBC)
# library(MBC)
# library(qmap)
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
  "output/06_bias_correction_CORDEX_series/df_PET_CORDEX_m_BC.rds"
file_series_PET_CORDEX_BC__csv <-
  "output/06_bias_correction_CORDEX_series/df_PET_CORDEX_m_BC.csv"

#### reading INPUT ####
df_PET_CORDEX <- readRDS(file_series_PET_CORDEX)

df_PET <- readRDS(file_serie_PET_pisco) 

df_periods_analysis <- read.csv(file_periods_analysis,
                                header = FALSE,
                                row.names = 1) %>% t() %>% as_tibble()

H_inicio <- as.POSIXct(df_periods_analysis$date_begin_historic,format="%Y-%m-%d")
H_final <- as.POSIXct(df_periods_analysis$date_end_historic,format="%Y-%m-%d")
F_inicio <- as.POSIXct(df_periods_analysis$date_begin_ref_future,format="%Y-%m-%d")
F_final <- as.POSIXct(df_periods_analysis$date_end_ref_future,format="%Y-%m-%d")

#### Bias Correction PET ####

df_PET_obs <- df_PET %>% 
  filter(dates >= H_inicio,
         dates < H_final)

df_PET_H_CORDEX <- df_PET_CORDEX %>% 
  filter(dates >= H_inicio,
         dates < H_final)

df_PET_F_CORDEX <- df_PET_CORDEX %>% 
  filter(dates >= F_inicio,
         dates < F_final)

df_PET_M_CORDEX <- rbind(df_PET_H_CORDEX,df_PET_F_CORDEX)

# loop 
df_PET_CORDEX_BC <- 
  df_PET_M_CORDEX %>% group_by(RCM, GCM) %>% 
  group_split() %>% 
  lapply(function(df_PET_CORDEX_model){
    # message(" | ", appendLF = FALSE)
    ## arrange done to be able to use SDM function of climQMBC package
    
    list_BC <- list()
    for (sce_rcp in c("rcp45", "rcp85")){
      df_PET_CORDEX_model_sce <- 
        df_PET_CORDEX_model %>% 
        filter(scenario  %in% c("historical", sce_rcp))
      
      # climQMBC
      values_BC <- climQMBC::DQM(obs=df_PET_obs$etp,mod=df_PET_CORDEX_model_sce$PET,mult_change=1,allow_negatives=0,frq = "m")
      
      df_PET_CORDEX_model_sce_BC <- df_PET_CORDEX_model_sce
      df_PET_CORDEX_model_sce_BC$PET <- values_BC
      
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