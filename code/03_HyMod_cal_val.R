library(tidyverse)
library(terra)
library(sf)
# library(hymodbluecat)
devtools::load_all("~/projects/my_packages/modified/hymodbluecat/")
library(hydroGOF)
library(DEoptim)
Sys.setenv(TZ = 'GMT')

#### INPUT ####
file_pr_inter_series <- "output/02_interpolation_pr/df_pr_m__inter.rds"
file_pr_pisco_serie <- "output/00_processed_data/df_pr_m__pisco.rds"
filet_etp_series <- "output/00_processed_data/df_ETP_m.rds"
file_Q_series <- "output/00_processed_data/df_Q_m.rds"

file_wshed_shp <- "data/watersheds/shp/pativilca_watershed.shp"

# start end periods warmup calib
file_periods_def <- 
  "output/03_hydrological_model_cal_val/input/hmodel_start_end_periods.csv"



#### OUTPUT ####
file_df_parameters__HyMod_csv <- 
  "output/03_hydrological_model_cal_val/df_parameters__HyMod.csv"
file_df_simulations__HyMod_rds <- 
  "output/03_hydrological_model_cal_val/df_Qm3s_simulations__HyMod.rds"
file_df_simulations__HyMod_csv <- 
  "output/03_hydrological_model_cal_val/df_Qm3s_simulations__HyMod.csv"
file_df_performance__HyMod_csv <- 
  "output/03_hydrological_model_cal_val/df_performance__HyMod.csv"


#### reading input ####
df_pr_m__inters <-  readRDS(file_pr_inter_series)
df_pr_m__pisco <-  readRDS(file_pr_pisco_serie)
df_etp_m <- readRDS(filet_etp_series)
df_Qm3s_m <- readRDS(file_Q_series) %>% rename_at(2, ~"Qm3s")
sf_wshed <- st_read(file_wshed_shp)

# periods
df_periods_def <- read.csv(file_periods_def, header = F,
                           row.names = 1) %>% 
  mutate_at(1, as.POSIXct) %>% t() %>% as_tibble()
print(t(df_periods_def))

date_begin_warmup <- df_periods_def$date_begin_warmup
date_end_warmup <- df_periods_def$date_end_warmup
date_begin_calib <- df_periods_def$date_begin_calib
date_end_calib <- df_periods_def$date_end_calib


#### Processing ####

### transforming m3/s to mm
area_km2 <- sf_wshed %>% st_area() %>% units::set_units( "km^2") %>% 
  as.numeric() 

#### HERE !!!!!!!!!!!!!!!!!!!!!!

## joining pr series from interpolations and pisco in 1 df
df_pr_m__all <- left_join(df_pr_m__inters, df_pr_m__pisco)

## loop to test the pr series from interpolations and pisco
list_performance <- list()
list_parameters <- list()
list_simulations <- list()


for(pr_source in names(df_pr_m__all)[-1]){
  # selecting 
  df_pr_m <- df_pr_m__all %>% 
    select(dates, !!pr_source) %>% rename_at(2,~ "pr")
  df_series <- full_join(df_pr_m, df_etp_m) %>% drop_na() %>% 
    left_join(df_Qm3s_m)
  df_series <- df_series %>% filter(dates >= date_begin_warmup)
  
  ## calibration, validation, warmup period selection
  Ind_warmup <- which(df_series$dates >= date_begin_warmup &
                        df_series$dates < date_end_warmup)
  Ind_cal <- which(df_series$dates >= date_begin_calib &
                     df_series$dates < date_end_calib)
  Ind_val <- which((df_series$dates < date_begin_calib | 
                      df_series$dates >= date_end_calib) &
                     df_series$dates >= date_end_warmup)
  Ind_run = which(df_series$dates >= date_end_warmup) # cal & val
  
  
  ### calibration
  set.seed(1234)
  pr1=hymod.par(c(100,1,0.5,1,0.5),
                area=area_km2,
                tdelta=30*86400,
                e = df_series$etp,
                p = df_series$pr,
                qoss = df_series$Qm3s,
                qinitial=15,
                indexrun = Ind_cal,
                lower=c(0,0,0,3,0.001), # 5
                upper=c(2000,2,1,40,3),
                opt="DEoptim",plot = F)
  
  ## Running the whole period (warmup + calib  + valid) 
  pr2=hymod.sim(pr1$optim$bestmem,
                area=area_km2,
                tdelta=30*86400,
                e = df_series$etp,
                p = df_series$pr,
                qoss = df_series$Qm3s,
                qinitial=15,
                bluecat=F)
  
  
  
  
  #### efficiency criterions
  df_Qm3s_sim <- tibble(dates = df_series$dates,
                       Qm3s_sim = pr2$q_tot)
  df_series <- df_series %>% left_join(df_Qm3s_sim)
  
  df_performance <- 
    tibble(
      Index = c("NSE", "KGE", "pbias")
    )
  values_sim <- df_series$Qm3s_sim[Ind_cal]
  values_obs <- df_series$Qm3s[Ind_cal]
  df_performance$Calibration <- c(
    NSE(values_sim, values_obs),
    KGE(values_sim, values_obs),
    pbias(values_sim, values_obs)
  )
  
  values_sim <- df_series$Qm3s_sim[Ind_val]
  values_obs <- df_series$Qm3s[Ind_val]
  df_performance$Validation<- c(
    NSE(values_sim, values_obs),
    KGE(values_sim, values_obs),
    pbias(values_sim, values_obs)
  )
  
  list_performance[[pr_source]] <- 
    df_performance %>% mutate(pr_source = pr_source, .before = Index)
  
  list_parameters[[pr_source]] <-   
    tibble(Parameters = names(pr1$par),
           !!pr_source := pr1$par) 
  
  list_simulations[[pr_source]] <- 
    tibble(dates = df_series$dates[Ind_run],
           !!pr_source := df_series$Qm3s_sim[Ind_run])
  
}

df_performance <- list_performance %>% do.call(rbind, . )
print(df_performance)

df_parameters <- list_parameters %>% Reduce(left_join, . )
print(df_parameters)

df_Qm3s_simulations <- list_simulations %>% Reduce(left_join, . )
# print(df_Qm3s_simulations)


#### writing OUTPUT ####
write.csv(df_parameters, file_df_parameters__HyMod_csv,
          row.names = FALSE, quote = FALSE)
saveRDS(df_Qm3s_simulations, file_df_simulations__HyMod_rds)
write.csv(df_Qm3s_simulations, file_df_simulations__HyMod_csv,
          row.names = FALSE, quote = FALSE)
write.csv(df_performance, file_df_performance__HyMod_csv,
          row.names = FALSE, quote = FALSE)


#### EXTRA ####
df_Qm3s_simulations %>% 
  gather(pr_source, Qm3s, -1) %>% 
  ggplot(aes(dates, Qm3s, col = pr_source)) + 
  geom_line() + 
  theme_bw() + 
  geom_line(data = df_Qm3s_m, aes(dates, Qm3s), 
            col ="red", linewidth = 1)
