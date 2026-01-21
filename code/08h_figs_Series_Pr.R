library(tidyverse)
library(openxlsx)

Sys.setenv(TZ = 'GMT')

#### INPUT ####

file_series_pr_all_CORDEX_BC <- "output/06_bias_correction_CORDEX_series/df_pr_all_CORDEX_m_BC.rds"
map <- read.xlsx("output/07_Q_projections/0. Input/TABLA MODELOS CLIMÁTICOS.xlsx")
map <- map %>% rename(old = `OLD.NAME`, new = `NEW.NAME`)



#### OUTPUT ####

#ruta <- "output/08_Plots/h. Pr series/C0_RCP45_RB_PR.jpg" # Activar para RCP 4.5
ruta <- "output/08_Plots/h. Pr series/C0_RCP85_RB_PR.jpg" # Activar para RCP 8.5




#### READING ####

df_pr_all_CORDEX_BC <- readRDS(file_series_pr_all_CORDEX_BC)

df_pr_all_CORDEX_BC<- df_pr_all_CORDEX_BC%>%
  left_join(map, by = c("GCM" = "old")) %>% 
  mutate(name = if_else(!is.na(new), new, GCM)) %>%
  select(-new)




#### SELECCION DE ESCENARIO ####

# df_pr <- df_pr_all_CORDEX_BC %>% filter(scenario=="rcp45") # Activar para RCP 4.5
df_pr <- df_pr_all_CORDEX_BC %>% filter(scenario=="rcp85")  # Activar para RCP 8.5




#### GENERACION DE GRAFICA ####

graph <- ggplot() +
  geom_line(data=df_pr,aes(x=dates,y = pr,color=source_pr), size = 0.15) +
  facet_wrap(~ name, ncol=2) +
  theme_bw() +
  labs(x = "Year", y = "Monthly Total Precipitation (mm)") +
  scale_y_continuous(breaks=seq(0,700,100))+
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(size = 10),  
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 10, face = "bold"),  # título eje X
    axis.title.y = element_text(size = 10, face = "bold"),  # título eje Y
    strip.background = element_rect(fill = "gray90"),
    strip.text = element_text(face = "bold",size=10))




#### EXPORTACION DE GRAFICA ####

# ggsave(ruta, plot = graph, width = 12, height = 12, dpi = 500) # Activar para RCP 4.5
ggsave(ruta, plot = graph, width = 12, height = 12, dpi = 500) # Activar para RCP 8.5

