library(tidyverse)
library(lubridate)
library(slider)

#### INPUTS ####

file_GR2M <- "output/07_Q_projections/df_Q_GR2M_proj.rds"
file_HyMod <- "output/07_Q_projections/df_Q_HyMod_proj.rds"
file_HBV <- "output/07_Q_projections/df_Q_HBV_proj.rds"
file_periods <- "output/06_bias_correction_CORDEX_series/input/periods_analysis_start_end.csv"


#### OUTPUTS ####

# ruta <- "output/08_Plots/f. IBR/C2_RCP45_RB_MW.jpg" # Activar para RCP 4.5
ruta <- "output/08_Plots/f. IBR/C2_RCP85_RB_MW.jpg" # Activar para RCP 8.5



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



#### AGRUPAMIENTO DE INFORMACION A NIVEL ANUAL####

df_all_anual <- df_all %>%
  mutate(year = year(dates)) %>%
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

df_summary <- df_roll30 %>% mutate(Code=paste0(name," - ",source_pr))

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

# df_sum <- rbind(df_H_45,df_45) # Activar para RCP 4.5
df_sum <- rbind(df_H_85,df_85) # Activar para RCP 8.5



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





#### GENERACION DE GRAFICAS #####

graph <- ggplot() +
  
  geom_line(data=df_env2,aes(x=year,y = IBR, color="GCM-RCM + Pr"),size=0.25)+

  geom_line(data=df_env3,aes(x=year,y = IBR,color="GCM-RCM + Hmodels"),size=0.25)+

  geom_line(data=df_env1,aes(x=year,y = IBR,color="GCM-RCM"),size=0.25)+

  facet_grid(hmodel ~ source_pr) +
  theme_bw() +
  scale_color_manual("Total uncertainty: ",
    values = c(
      "GCM-RCM" = "blue",
      "GCM-RCM + Pr" = "red",
      "GCM-RCM + Hmodels" = "darkgreen")) +
  
  labs(x = "Year", y = "Interquartile Range - IQR (mm)") +
  scale_x_continuous(breaks=seq(2030,2100,20),limits = c(2030,2100))+
  scale_y_continuous(breaks=seq(0,700,2))+
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 12),  
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12, face = "bold"),  # título eje X
    axis.title.y = element_text(size = 12, face = "bold"),  # título eje Y
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold",size=12))



#### EXPORTACION DE GRAFICA ####

# ggsave(ruta, plot = graph, width = 12, height = 12, dpi = 500) # Activar para RCP 4.5
ggsave(ruta, plot = graph, width = 12, height = 12, dpi = 500) # Activar para RCP 8.5