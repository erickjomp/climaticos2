library(tidyverse)
library(hyfo)
library(climQMBC)

Sys.setenv(TZ = 'GMT')

#### INPUT ####
file_series_pr_CORDEX <-
  "output/05_selection_CORDEX_series/df_pr_CORDEX_m.rds"

file_periods_analysis <-
  "output/06_bias_correction_CORDEX_series/input/periods_analysis_start_end.csv"

file_series_pr_inters <-
  "output/02_interpolation_pr/df_pr_m__inter.rds"

file_serie_pr_pisco <-
  "output/00_processed_data/df_pr_m__pisco.rds"


#### OUTPUT ####
file_series_pr_CORDEX_BC__rds <-
  "output/06_bias_correction_CORDEX_series/df_pr_all_CORDEX_m_BC.rds"
file_series_pr_CORDEX_BC__csv <-
  "output/06_bias_correction_CORDEX_series/df_pr_all_CORDEX_m_BC.csv"

#### reading INPUT ####
df_pr_CORDEX <- readRDS(file_series_pr_CORDEX)

df_periods_analysis <- read.csv(file_periods_analysis,
                                header = FALSE,
                                row.names = 1) %>% t() %>% as_tibble()

H_inicio <- as.POSIXct(df_periods_analysis$date_begin_historic,format="%Y-%m-%d")
H_final <- as.POSIXct(df_periods_analysis$date_end_historic,format="%Y-%m-%d")
F_inicio <- as.POSIXct(df_periods_analysis$date_begin_ref_future,format="%Y-%m-%d")
F_final <- as.POSIXct(df_periods_analysis$date_end_ref_future,format="%Y-%m-%d")

# %>%   mutate_all(as.POSIXct)
print(t(df_periods_analysis))

df_pr_inters <- readRDS(file_series_pr_inters)
df_pr_pisco <- readRDS(file_serie_pr_pisco)


#### processing ####
df_pr_all <- left_join(df_pr_inters, df_pr_pisco)


#### Bias Correction pr ####
list_results_BC <- list()

for (source_pr in names(df_pr_all)[-1]){
  message(which(source_pr == names(df_pr_all)[-1]), " / ",
          ncol(df_pr_all) -1, "\r", appendLF = FALSE ) 

  df_pr_obs <- df_pr_all %>% dplyr::select(dates, pr = !!source_pr) %>%
    filter(dates >= H_inicio,
           dates < H_final)
  
  df_pr_H_CORDEX <- df_pr_CORDEX %>% 
    filter(dates >= H_inicio,
           dates < H_final)
  
  df_pr_F_CORDEX <- df_pr_CORDEX %>% 
    filter(dates >= F_inicio,
           dates < F_final)
  
  df_pr_M_CORDEX <- rbind(df_pr_H_CORDEX,df_pr_F_CORDEX)
  
  # loop 
  df_pr_CORDEX_BC <- 
    df_pr_M_CORDEX %>% group_by(RCM, GCM) %>% 
    group_split() %>% 
    lapply(function(df_pr_CORDEX_model){
      list_BC <- list()
      for (sce_rcp in c("rcp45", "rcp85")){
        df_pr_CORDEX_model_sce <- 
          df_pr_CORDEX_model %>% 
          filter(scenario  %in% c("historical", sce_rcp))
        
        # ClimQMBC
        values_BC <- climQMBC::DQM(obs=df_pr_obs$pr,mod=df_pr_CORDEX_model_sce$pr,mult_change=1,allow_negatives=0,frq = "m")
        
        df_pr_CORDEX_model_sce_BC <- df_pr_CORDEX_model_sce
        df_pr_CORDEX_model_sce_BC$pr <- values_BC
        
        list_BC[[sce_rcp]] <- df_pr_CORDEX_model_sce_BC
      }
      
      # since both are the same hist are same we join#
      df_pr_CORDEX_model_BC <- list_BC %>% do.call(rbind,.) %>% 
        filter(!duplicated(dates) | (scenario != "historical"))
      
      return(df_pr_CORDEX_model_BC)
      
    }) %>% do.call(rbind,.)
  
  df_pr_CORDEX_BC <- df_pr_CORDEX_BC %>% mutate(source_pr = source_pr, 
                                                .before = dates)
  
  list_results_BC[[source_pr]] <- df_pr_CORDEX_BC
}

df_pr_all_CORDEX_BC <- list_results_BC %>% do.call(rbind,.) 
  
#### writing output ####
saveRDS(df_pr_all_CORDEX_BC ,file_series_pr_CORDEX_BC__rds)
write.csv(df_pr_all_CORDEX_BC, file_series_pr_CORDEX_BC__csv,
          quote = FALSE, row.names = FALSE)