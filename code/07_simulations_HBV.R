library(tidyverse)
library(HBV.IANIGLA)
library(sf)
Sys.setenv(TZ = 'GMT')
source("code/functions_hbv_model.R")


#### INPUT ####
file_series_pr_all_CORDEX_BC <- 
  "output/06_bias_correction_CORDEX_series/df_pr_all_CORDEX_m_BC.rds"
file_series_PET_CORDEX_BC <- 
  "output/06_bias_correction_CORDEX_series/df_PET_CORDEX_m_BC.rds"
df_cal_parameters <- 
  "output/03_hydrological_model_cal_val/df_parameters__HBV.csv"

file_wshed_shp <- "data/watersheds/shp/pativilca_watershed.shp"


#### OUTPUT ####
file_series_Q_HBV_proj__rds <- 
  "output/07_Q_projections/df_Q_HBV_proj.rds"
file_series_Q_HBV_proj__csv <- 
  "output/07_Q_projections/df_Q_HBV_proj.csv"

#### reading input ####
df_pr_all_CORDEX_BC <- 
  readRDS(file_series_pr_all_CORDEX_BC)
df_PET_CORDEX_BC <- 
  readRDS(file_series_PET_CORDEX_BC)
df_parameters <- 
  read.csv(df_cal_parameters)

sf_wshed <- st_read(file_wshed_shp)


#### processing ####
area_km2 <- sf_wshed %>% st_area() %>% units::set_units( "km^2") %>% 
  as.numeric() 

#### running models ####
names_source_pr <- unique(df_pr_all_CORDEX_BC$source_pr)
names_models <- unique(df_pr_all_CORDEX_BC$name)

df_Qm3s_CORDEX_BC__HBV <- 
  lapply(names_source_pr, function(name_source_pr){
    list_results_models <- list()
    
    for (model in names_models){
      list_results_scens <- list()
      for (sce_rcp in c("rcp45","rcp85")){
        df_pr_CORDEX_BC_model_sce <- df_pr_all_CORDEX_BC %>% 
          filter(source_pr == name_source_pr, 
                 scenario %in%  c("historical", sce_rcp), 
                 name ==model)
        df_PET_CORDEX_BC_model_sce <- df_PET_CORDEX_BC %>% 
          filter(scenario  %in%  c("historical",sce_rcp),name == model)
        df_series_BC <- tibble(dates = df_pr_CORDEX_BC_model_sce$dates,
                               pr = df_pr_CORDEX_BC_model_sce$pr,
                               PET = df_PET_CORDEX_BC_model_sce$PET)
        ind_run <- 1:nrow(df_series_BC)
        
        # externsion for warmup
        n_years_ext <- 3
        ind_ext_warmup <- 1:(n_years_ext * 12)
        min_date <- min(df_series_BC$dates) %>% as.POSIXlt()
        min_ext_date <- min_date
        min_ext_date$year <- min_ext_date$year - n_years_ext
        dates_ext <- seq.POSIXt(as.POSIXct(min_ext_date), 
                                by = "months", 
                                length.out = 12 * n_years_ext)
        df_series_BC <- rbind(df_series_BC[ind_ext_warmup,],
                              df_series_BC)
        df_series_BC$dates[ind_ext_warmup] <- dates_ext
        ind_run <- (n_years_ext * 12 + 1):nrow(df_series_BC)
        
        
        ## running model
        qsim <- 
          hbv_model(pr = df_series_BC$pr,
                    pet = df_series_BC$PET,
                    param_soil_module = 
                      df_parameters[[name_source_pr]][1:3],
                    param_routing_module = 
                      df_parameters[[name_source_pr]][-(1:3)],
                    routing_model = 3,
                    init_cond_routing = c(0,0))
  
        
        df_Qm3s_sim <- df_pr_CORDEX_BC_model_sce %>% rename(Qm3s = pr)
        df_Qm3s_sim <- df_Qm3s_sim %>% 
          mutate(Qm3s = qsim[-ind_ext_warmup] / 
                   (lubridate::days_in_month(dates) * 24*60*60) * 
                   (area_km2 * 1000 * 1000) / 1000)
        list_results_scens[[sce_rcp]] <- df_Qm3s_sim
      }
      list_results_models[[model]] <- list_results_scens %>% 
        do.call(rbind, .) %>% 
        filter(!duplicated(dates) | scenario  %in% c("rcp45","rcp85"))
    }
    list_results_models %>% do.call(rbind,.)
  }) %>% do.call(rbind,.)


#### writing OUTPUT ####
saveRDS(df_Qm3s_CORDEX_BC__HBV, 
        file = file_series_Q_HBV_proj__rds)
write.csv(df_Qm3s_CORDEX_BC__HBV,
          file = file_series_Q_HBV_proj__csv,
          quote = FALSE,
          row.names = FALSE)

