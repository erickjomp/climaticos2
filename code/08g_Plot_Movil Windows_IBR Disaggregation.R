library(tidyverse)
library(lubridate)
library(slider)

#### INPUTS ####

file_GR2M <- "output/07_Q_projections/df_Q_GR2M_proj.rds"
file_HyMod <- "output/07_Q_projections/df_Q_HyMod_proj.rds"
file_HBV <- "output/07_Q_projections/df_Q_HBV_proj.rds"
file_periods <- "output/06_bias_correction_CORDEX_series/input/periods_analysis_start_end.csv"



#### OUTPUTS ####

# ruta <- output/08_Plots/g. IBR Disaggregation/C3_RCP45_RB_MW.jpg" # Activar para RCP 4.5
ruta <- "output/08_Plots/g. IBR Disaggregation/C3_RCP85_RB_MW.jpg" # Activar para RCP 8.5



#### READING ####

df_GR2M <-  readRDS(file_GR2M) %>% mutate(hmodel = "GR2M")
df_GR2M <- df_GR2M %>% mutate(Qm3s=Qm3s*lubridate::days_in_month(dates)*24*60*60/(2906*10^6)*1000)

df_HyMod <- readRDS(file_HyMod) %>% mutate(hmodel = "HyMod")
df_HyMod <-df_HyMod %>% mutate(Qm3s=Qm3s*lubridate::days_in_month(dates)*24*60*60/(2906*10^6)*1000)

df_HBV <- readRDS(file_HBV) %>% mutate(hmodel = "HBV")
df_HBV <- df_HBV %>% mutate(Qm3s=Qm3s*lubridate::days_in_month(dates)*24*60*60/(2906*10^6)*1000)

df_all <- rbind(df_GR2M, df_HyMod, df_HBV) %>% 
  mutate(across(c(GCM, hmodel, source_pr,scenario ), ~ fct_inorder(.x)))

df_periods <- read.csv(file_periods,header = FALSE,row.names = 1) %>% t() %>% as_tibble()



#### AGRUPAMIENTO DE INFORMACION A NIVEL ANUAL / EPOCA HUMEDA / EPOCA SECA ####

df_all_anual <- df_all %>%
  # mutate(year = year(dates)) %>%                                      # Activar para analisis por epocas
  mutate(year = year(dates),month = month(dates)) %>%
  # filter(month %in% c(11, 12, 1, 2, 3, 4)) %>%                        # Activar para analisis por epoca humeda
  # mutate(year = if_else(month %in% c(11, 12), year + 1, year)) %>%    # Activar para analisis por epoca humeda
  filter(month %in% c(5,6,7,8,9,10)) %>%                                # Activar para analisis por epoca seca
  group_by(RCM, GCM, name, scenario, source_pr, hmodel, year) %>%
  summarise(Qm3s_mean = mean(Qm3s, na.rm = TRUE), .groups = "drop")




#### APLICACIÓN DE VENTANAS MOVILES ####

df_roll30 <- df_all_anual %>%
  arrange(RCM, GCM, name, scenario, source_pr, hmodel, year) %>%
  group_by(RCM, GCM, name, scenario, source_pr, hmodel) %>%
  mutate(
    Qm3s_MW = slide_dbl(Qm3s_mean, mean, .before = 29, .complete = TRUE)
  ) %>%
  ungroup()




#### DIFERENCIACIÓN DE INFORMACION POR ESCENARIO Y TIPO DE DATA ####

df_summary <- df_roll30  %>% mutate(Code=paste0(name," - ",source_pr))

df_H_45 <- df_summary %>% filter(scenario=="historical")
df_H_45$tipo <- "RCP45"
df_H_45$data <- "Historica"

df_H_85 <- df_summary %>% filter(scenario=="historical")
df_H_85$tipo <- "RCP85"
df_H_85$data <- "Historica"

df_45 <- df_summary %>% filter(scenario=="rcp45")
df_45$tipo <- "RCP45"
df_45$data <- "RCP45"

df_85 <- df_summary %>% filter(scenario=="rcp85")
df_85$tipo <- "RCP85"
df_85$data <- "RCP85"





#### SELECCION DEL ESCENARIO DE EVALUACIÓN 

# df_sum <- rbind(df_H_45,df_45)  # Activar para RCP 4.5
df_sum <- rbind(df_H_85,df_85)  # Activar para RCP 8.5





#### GENERACION DE ENVOLVENTE E IBR GCM-RCM ####

df_env1 <- df_sum%>%
  group_by(hmodel,source_pr, year) %>%
  summarise(
    Qm3s_min  = quantile(Qm3s_MW, probs = 0.25, na.rm = TRUE),
    Qm3s_max  = quantile(Qm3s_MW, probs = 0.75, na.rm = TRUE)
  ) %>%
  ungroup()

df_env1 <-df_env1 %>% mutate(IBR=Qm3s_max-Qm3s_min)

df_mediam1 <- df_sum%>%
  group_by(hmodel,source_pr, year) %>%
  summarise(
    Qm3s_mediam= quantile(Qm3s_MW, probs = 0.50, na.rm = TRUE)) %>%ungroup()





#### GENERACION DE ENVOLVENTE E IBR GCM-RCM + PR ####

df_env2 <- df_sum%>%
  group_by(hmodel, year) %>%
  summarise(
    Qm3s_min  = quantile(Qm3s_MW, probs = 0.25, na.rm = TRUE),
    Qm3s_max  = quantile(Qm3s_MW, probs = 0.75, na.rm = TRUE)
  ) %>%
  ungroup()

df_env2 <-df_env2 %>% mutate(IBR=Qm3s_max-Qm3s_min)

df_mediam2 <- df_sum%>%
  group_by(hmodel, year) %>%
  summarise(
    Qm3s_mediam= quantile(Qm3s_MW, probs = 0.50, na.rm = TRUE)) %>%ungroup()





#### GENERACION DE ENVOLVENTE E IBR GCM-RCM + Hmodels ####

df_env3 <- df_sum%>%
  group_by(source_pr, year) %>%
  summarise(
    Qm3s_min  = quantile(Qm3s_MW, probs = 0.25, na.rm = TRUE),
    Qm3s_max  = quantile(Qm3s_MW, probs = 0.75, na.rm = TRUE)
  ) %>%
  ungroup()

df_env3 <-df_env3 %>% mutate(IBR=Qm3s_max-Qm3s_min)

df_mediam3 <- df_sum%>%
  group_by(source_pr, year) %>%
  summarise(
    Qm3s_mediam= quantile(Qm3s_MW, probs = 0.50, na.rm = TRUE)) %>%ungroup()




#### GENERACION DE IBR DISGREGADO ####

# Para GCM-RCM
df_e1 <- df_env1

# Para GCM-RCM + Pr
df_e2 <- data.frame()

for (m in 1:nrow(df_e1)){
  
  h_pr <- df_env1[m,1]
  
  h_pr <- as.character(h_pr$hmodel)
  
  año_pr <- df_env1[m,3]
  
  año_pr <- as.numeric(año_pr$year)
  
  df_e2_pre <- df_env2 %>% filter(hmodel==h_pr & year==año_pr)
  
  df_e2 <- rbind(df_e2,df_e2_pre)
}

# Para GCM-RCM + Hmodels
df_e3 <- data.frame()

for (n in 1:nrow(df_e1)){
  
  name_pr <- df_env1[n,2]
  
  name_pr <- as.character(name_pr$source_pr)
  
  año_pr <- df_env1[n,3]
  
  año_pr <- as.numeric(año_pr$year)
  
  df_e3_pre <- df_env3 %>% filter(source_pr==name_pr & year==año_pr)
  
  df_e3 <- rbind(df_e3,df_e3_pre)
}


# Para incorporacion de informacion
df_env <- df_e1

df_env$IBR2 <- df_e2$IBR

df_env$IBR3 <- df_e3$IBR

df_env <- df_env %>% mutate(Var2=IBR2-IBR,Var3=IBR3-IBR)





#### ESTIMACION DE LOS RANGOS DE VARIACION DEL IBR DISGREGADO ####

df_labels <- df_env %>%
  group_by(hmodel, source_pr) %>%
  summarise(
    max_IBR = max(IBR, na.rm = TRUE),
    min_IBR = min(IBR, na.rm = TRUE),
    max_IBR2 = max(Var2, na.rm = TRUE),
    min_IBR2= min(Var2, na.rm = TRUE),
    max_IBR3 = max(Var3, na.rm = TRUE),
    min_IBR3= min(Var3, na.rm = TRUE),
    x_pos = max(year),   
    .groups = "drop")




#### GENERACION DE GRAFICAS #####

graph <- ggplot() +
  
  geom_line(data=df_env,aes(x=year,y = Var2, color="Pr"),size=0.25)+

  # Activar para RCP 4.5 
  
  # geom_text(
  #   data = df_labels,
  #   aes(x = 2070, y = 6, label = paste0("Max spread: ", round(max_IBR2,1)), hmodel = hmodel,
  #       source_pr = source_pr ),
  #   hjust = 0, vjust = 0, size = 3, color = "red"
  # ) +
 
  
  
  
  # Activar para RCP 8.5
  
  # geom_text(
  #   data = df_labels,
  #   aes(x = 2070, y =12, label = paste0("Max spread: ", round(max_IBR2,1)), hmodel = hmodel,
  #       source_pr = source_pr ),
  #   hjust = 0, vjust = 0, size = 3, color = "red"
  # ) +
  
  
  
  
  # Activar para RCP 8.5 - Wet
  
  # geom_text(
  #   data = df_labels,
  #   aes(x = 2070, y =18, label = paste0("Max spread: ", round(max_IBR2,1)), hmodel = hmodel,
  #       source_pr = source_pr ),
  #   hjust = 0, vjust = 0, size = 3, color = "red"
  # ) +
  
  
  
  
  # Activar para RCP 8.5 - Dry
  
  geom_text(
    data = df_labels,
    aes(x = 2070, y =7, label = paste0("Max spread: ", round(max_IBR2,1)), hmodel = hmodel,
        source_pr = source_pr ),
    hjust = 0, vjust = 0, size = 3, color = "red"
  ) +
  

  
  
  geom_line(data=df_env,aes(x=year,y = Var3,color="Hmodels"),size=0.25)+

  # Activar para RCP 4.5 
  
  # geom_text(
  #   data = df_labels,
  #   aes(x = 2070, y = 6.5, label = paste0("Max spread: ", round(max_IBR3,1)), hmodel = hmodel,
  #       source_pr = source_pr ),
  #   hjust = 0, vjust = 0, size = 3, color = "darkgreen"
  # ) +
  
  
  
  
  # Activar para RCP 8.5
  
    # geom_text(
  #   data = df_labels,
  #   aes(x = 2070, y = 13, label = paste0("Max spread: ", round(max_IBR3,1)), hmodel = hmodel,
  #       source_pr = source_pr ),
  #   hjust = 0, vjust = 0, size = 3, color = "darkgreen"
  # ) +
  
  
  
  
  # Activar para RCP 8.5 - Wet
  
  # geom_text(
  #   data = df_labels,
  #   aes(x = 2070, y = 19.5, label = paste0("Max spread: ", round(max_IBR3,1)), hmodel = hmodel,
  #       source_pr = source_pr ),
  #   hjust = 0, vjust = 0, size = 3, color = "darkgreen"
  # ) +
  
  
  
  
  # Activar para RCP 8.5 - Dry
  geom_text(
    data = df_labels,
    aes(x = 2070, y = 7.5, label = paste0("Max spread: ", round(max_IBR3,1)), hmodel = hmodel,
        source_pr = source_pr ),
    hjust = 0, vjust = 0, size = 3, color = "darkgreen"
  ) +

  
  
  
  
  geom_line(data=df_env,aes(x=year,y = IBR,color="GCM-RCM"),size=0.25)+
 
  # Activar para RCP 4.5
  
  # geom_text(
  #   data = df_labels,
  #   aes(x = 2070, y = 7, label = paste0("Max spread: ", round(max_IBR,1)), hmodel = hmodel,
  #       source_pr = source_pr ),
  #   hjust = 0, vjust = 0, size = 3, color = "blue"
  # ) +

  
  
  
  
  # Activar para RCP 8.5
  
  # geom_text(
  #   data = df_labels,
  #   aes(x = 2070, y = 14, label = paste0("Max spread: ", round(max_IBR,1)), hmodel = hmodel,
  #       source_pr = source_pr ),
  #   hjust = 0, vjust = 0, size = 3, color = "blue"
  # ) +
  
  
  
  
  
  # Activar para RCP 8.5 - Wet
  
  # geom_text(
  #   data = df_labels,
  #   aes(x = 2070, y = 21, label = paste0("Max spread: ", round(max_IBR,1)), hmodel = hmodel,
  #       source_pr = source_pr ),
  #   hjust = 0, vjust = 0, size = 3, color = "blue"
  # ) +
  
  
  
  
  
  # Activara para RCP 8.5 - Dry
  
  geom_text(
    data = df_labels,
    aes(x = 2070, y = 8, label = paste0("Max spread: ", round(max_IBR,1)), hmodel = hmodel,
        source_pr = source_pr ),
    hjust = 0, vjust = 0, size = 3, color = "blue"
  ) +
  

  
  
  geom_hline(yintercept = 0, color = "black", linetype = "solid", linewidth = 0.1) +
  
  facet_grid(hmodel ~ source_pr) +
  theme_bw() +
  scale_color_manual("Uncertainty source: ",
    values = c(
      "GCM-RCM" = "blue",
      "Pr" = "red",
      "Hmodels" = "darkgreen")) +
  
  labs(x = "Year", y = "Individual contribution to IQR (mm)") +
  scale_x_continuous(breaks=seq(2030,2100,20),limits = c(2030,2100))+
  # scale_y_continuous(breaks=seq(-50,50,2))+ # Activar para RCP 4.5
  scale_y_continuous(breaks=seq(-50,50,1))+ # Activar para RCP 8.5
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 12),  
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12, face = "bold"), 
    axis.title.y = element_text(size = 12, face = "bold"), 
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold",size=12))




#### EXPORTACION DE GRAFICA ####

# ggsave(ruta, plot = graph, width = 12, height = 12, dpi = 500) # Activar para RCP 4.5
ggsave(ruta, plot = graph, width = 12, height = 12, dpi = 500) # Activar para RCP 8.5
