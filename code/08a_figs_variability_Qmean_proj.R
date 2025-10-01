library(tidyverse)
# remotes::install_github("https://github.com/erickjomp/Rthemes.git")
library(Rthemes)

#### INPUTS ####
file_GR2M <- "output/07_Q_projections/df_Q_GR2M_proj.rds"
file_HyMod <- "output/07_Q_projections/df_Q_HyMod_proj.rds"
file_HBV <- "output/07_Q_projections/df_Q_HBV_proj.rds"

file_periods <- "output/06_bias_correction_CORDEX_series/input/periods_analysis_start_end.csv"

base_size_figs <- 8 # 7 : recommended for normal text by elsevier

#### OUTPUTS ####
file_plot_precipmethod_1 <- "output/08_plots_results/fig_variability_dueto_precipestimation_1.pdf"
file_plot_precipmethod_2 <- "output/08_plots_results/fig_variability_dueto_precipestimation_2.pdf"

file_plot_hydromodel_1 <- "output/08_plots_results/fig_variability_dueto_hydromodel_1.pdf"
file_plot_hydromodel_2 <- "output/08_plots_results/fig_variability_dueto_hydromodel_2.pdf"


#### READING ####
df_GR2M <-  readRDS(file_GR2M) %>% mutate(hmodel = "GR2M")
df_HyMod <- readRDS(file_HyMod) %>% mutate(hmodel = "HyMod")
df_HBV <- readRDS(file_HBV) %>% mutate(hmodel = "HBV")

df_all <- rbind(df_GR2M, df_HyMod, df_HBV) %>% 
  mutate(across(c(GCM, hmodel, source_pr,scenario ), ~ fct_inorder(.x)))

df_periods <- read.csv(file_periods,header = FALSE,
                       row.names = 1) %>% t() %>% 
  as_tibble() %>%   mutate_all(as.POSIXct) 
print(t(df_periods))

#### PROCESS ####
df_Qy <- df_all %>%
  filter((scenario == "historical" & 
            dates>= df_periods$date_begin_historic_pr &
            dates < df_periods$date_end_historic_pr) |
           (scenario != "historical" & 
              dates>= df_periods$date_begin_ref_future &
              dates < df_periods$date_end_ref_future) ) %>% 
  # filter(dates <= "2005-12-01" | dates >= "2031-01-01") %>% 
  group_by(GCM, source_pr, hmodel,scenario) %>% 
  summarise(mean_Qy = mean(Qm3s)*12)

df_Qy_change45 <-
  df_Qy %>% spread(scenario, mean_Qy) %>% mutate(Qy_change = 100*((rcp45-historical)/historical)) %>% 
  select(-rcp45,-rcp85,-historical) %>% 
  mutate(scenario = "RCP 4.5")

df_Qy_change85 <-
  df_Qy %>% spread(scenario, mean_Qy) %>% mutate(Qy_change = 100*((rcp85- historical)/historical)) %>% 
  select(-rcp45,-rcp85,-historical) %>% 
  mutate(scenario = "RCP 8.5")




#### PLOTS PRECIP ESTIMATION METHOD VARIABILITY ####

## fixed hmodel1 ##
df_Qy_change45 %>% rbind(df_Qy_change85) %>% # this seems the BEST
  ggplot(aes(source_pr, Qy_change, color = GCM)) +   
  geom_boxplot(col = "grey50") +
  geom_point(size = 1) +
  facet_grid(scenario~hmodel,scales = "free_y") +
  labs(y = "Mean annual runoff change (%)", 
       x = "Precipitation estimation method",
       color = "Driving GCM for CORDEX RCM simulations") + 
  # theme_bw() +
  my_ggtheme(base_size = base_size_figs) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "top",
        legend.background = element_rect(color = "black", fill = "white", linewidth = 0.5),
        legend.key = element_rect(fill = "white", color = NA),
        legend.title = element_text(hjust = 0.5) ) + # cleaner keys inside) +
  guides(color = guide_legend(nrow = 2,title.position = "top", byrow = TRUE)) 

ggsave(filename = file_plot_precipmethod_1,
       width = 190,height = 120, #98,
       units = "mm")

## fixed hmodel2 ##

df_Qy_change45 %>% rbind(df_Qy_change85) %>% # this seems the BEST
  ggplot(aes(GCM, Qy_change, color = source_pr)) +   
  geom_point(size = 1) +
  facet_grid(scenario~hmodel,scales = "free_y") +
  labs(y = "Mean annual runoff change (%)", 
       x = "Driving GCM for CORDEX RCM simulations",
       color = "Precipitation estimation method") + 
  # theme_bw() +
  my_ggtheme(base_size = base_size_figs) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "top",
        legend.background = element_rect(color = "black", fill = "white", linewidth = 0.5),
        legend.key = element_rect(fill = "white", color = NA)) + # cleaner keys inside) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

ggsave(filename = file_plot_precipmethod_2,
       width = 190,height = 120, #98,
       units = "mm")




#### PLOTS HYDROLOGICAL MODEL VARIABILITY ####

## fixed source_pr2 ##
df_Qy_change45 %>% rbind(df_Qy_change85) %>% # this seems the BEST
  ggplot(aes(hmodel, Qy_change,color = GCM)) +   
  geom_boxplot(col = "grey50") +
  geom_point(size = 1) +
  facet_grid(scenario~source_pr,scales = "free_y") +
  labs(y = "Mean annual runoff change (%)", 
       x = "Hydrological model",
       color = "Driving GCM for CORDEX RCM simulations") + 
  # theme_bw() +
  my_ggtheme(base_size = base_size_figs) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "top",
        legend.background = element_rect(color = "black", fill = "white", linewidth = 0.5),
        legend.key = element_rect(fill = "white", color = NA),
        legend.title = element_text(hjust = 0.5) ) + # cleaner keys inside) +
  guides(color = guide_legend(nrow = 2,title.position = "top", byrow = TRUE))

ggsave(filename = file_plot_hydromodel_1,
       width = 190,height = 120, #98,
       units = "mm")

## fixed source_pr2 ##

df_Qy_change45 %>% rbind(df_Qy_change85) %>%  # this seems the BEST
  ggplot(aes(GCM, Qy_change, color = hmodel)) +   
  geom_point(size=1) + 
  facet_grid(scenario~source_pr,scales = "free_y") +
  labs(y = "Mean annual runoff change (%)", 
       x = "Driving GCM for CORDEX RCM simulations",
       color = "Hydrological Model") + 
  # theme_bw() +
  my_ggtheme(base_size = base_size_figs) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "top",
        legend.background = element_rect(color = "black", fill = "white", linewidth = 0.5),
        legend.key = element_rect(fill = "white", color = NA)) + # cleaner keys inside) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

ggsave(filename = file_plot_hydromodel_2,
       width = 190,height = 120, #98,
       units = "mm")



