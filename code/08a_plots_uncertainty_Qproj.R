library(tidyverse)

#### INPUTS ####
file_GR2M <- "output/07_Q_projections/df_Q_GR2M_proj.rds"
file_HyMod <- "output/07_Q_projections/df_Q_HyMod_proj.rds"
file_HBV <- "output/07_Q_projections/df_Q_HBV_proj.rds"

#### OUTPUTS ####



#### PROCESS ####
df_GR2M <-  readRDS(file_GR2M) %>% mutate(hmodel = "GR2M")
df_HyMod <- readRDS(file_HyMod) %>% mutate(hmodel = "HyMod")
df_HBV <- readRDS(file_HBV) %>% mutate(hmodel = "HBV")

df_all <- rbind(df_GR2M, df_HyMod, df_HBV) %>% 
  mutate(across(c(GCM, hmodel, source_pr,scenario ), ~ fct_inorder(.x)))


df_Qy <- df_all %>%
  group_by(GCM, source_pr, hmodel,scenario) %>% summarise(mean_Qy = mean(Qm3s)*12)

df_Qy_change45 <-
  df_Qy %>% spread(scenario, mean_Qy) %>% mutate(Qy_change = 100*((rcp45-historical)/historical)) %>% 
    select(-rcp45,-rcp85,-historical) %>% 
  mutate(scenario = "RCP 4.5")

df_Qy_change85 <-
  df_Qy %>% spread(scenario, mean_Qy) %>% mutate(Qy_change = 100*((rcp85- historical)/historical)) %>% 
  select(-rcp45,-rcp85,-historical) %>% 
  mutate(scenario = "RCP 8.5")


#### PLOTS ####
# fixed hmodel
df_Qy_change45 %>% 
  ggplot(aes(source_pr, Qy_change)) + 
  geom_boxplot() + 
  facet_wrap(~hmodel) + 
  geom_line() + 
  theme_bw()
  
df_Qy_change85 %>% 
  ggplot(aes(source_pr, Qy_change)) + 
  geom_boxplot() + 
  facet_wrap(~hmodel) + 
  geom_line() + 
  theme_bw()

# fixed source_pr
df_Qy_change45 %>% 
  ggplot(aes(hmodel, Qy_change)) + 
  geom_boxplot() + 
  facet_wrap(~source_pr) + 
  geom_line() + 
  theme_bw()

df_Qy_change85 %>% 
  ggplot(aes(hmodel, Qy_change)) + 
  geom_boxplot() + 
  facet_wrap(~source_pr) + 
  geom_line() + 
  theme_bw()


#### sane plot but models by colors
# fixed hmodel
# df_Qy_change45 %>% 
#   ggplot(aes(source_pr, Qy_change, color = GCM)) + 
#   geom_point() + 
#   facet_wrap(~hmodel) + 
#   geom_line() + 
#   theme_bw()
# 
# df_Qy_change85 %>% 
#   ggplot(aes(source_pr, Qy_change, color = GCM)) + 
#   geom_point() + 
#   facet_wrap(~hmodel)  +
#   theme_bw()

df_Qy_change45 %>% rbind(df_Qy_change85) %>% # this seems the BEST
  ggplot(aes(source_pr, Qy_change, color = GCM)) +   
  geom_boxplot(col = "grey50") +
  geom_point() + 
  facet_grid(scenario~hmodel,scales = "free_y") +
  labs(y = "Mean annual runoff change (%)", 
       x = "Precipitation estimation method",
       color = "Driving GCM for CORDEX RCM simulations") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "top",
        legend.background = element_rect(color = "black", fill = "white", linewidth = 0.5),
        legend.key = element_rect(fill = "white", color = NA),
        legend.title = element_text(hjust = 0.5) ) + # cleaner keys inside) +
  guides(color = guide_legend(nrow = 2,title.position = "top", byrow = TRUE))


## fixed hmodel2 ##

# df_Qy_change45 %>% 
#   ggplot(aes(GCM, Qy_change, color = source_pr)) + 
#   geom_point() + 
#   facet_wrap(~hmodel) + 
#   geom_line() + 
#   theme_bw()
# 
# df_Qy_change85 %>% 
#   ggplot(aes(GCM, Qy_change, color = source_pr)) + 
#   geom_point() + 
#   facet_wrap(~hmodel) +
#   theme_bw()

df_Qy_change45 %>% rbind(df_Qy_change85) %>% # this seems the BEST
  ggplot(aes(GCM, Qy_change, color = source_pr)) +   
  geom_point() + 
  facet_grid(scenario~hmodel,scales = "free_y") +
  labs(y = "Mean annual runoff change (%)", 
       x = "Driving GCM for CORDEX RCM simulations",
       color = "Precipitation estimation method") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "top",
        legend.background = element_rect(color = "black", fill = "white", linewidth = 0.5),
        legend.key = element_rect(fill = "white", color = NA)) + # cleaner keys inside) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))


## fixed source_pr ##
# df_Qy_change45 %>% 
#   ggplot(aes(hmodel, Qy_change, color = GCM)) + 
#   geom_point() + 
#   geom_line() +
#   facet_wrap(~source_pr) + 
#   theme_bw()
# 
# df_Qy_change85 %>% 
#   ggplot(aes(hmodel, Qy_change, color = GCM)) + 
#   geom_point() + 
#   facet_wrap(~source_pr) + 
#   theme_bw() 

df_Qy_change45 %>% rbind(df_Qy_change85) %>% # this seems the BEST
  ggplot(aes(hmodel, Qy_change,color = GCM)) +   
  geom_boxplot(col = "grey50") +
  geom_point() +
  facet_grid(scenario~source_pr,scales = "free_y") +
  labs(y = "Mean annual runoff change (%)", 
       x = "Hydrological model",
       color = "Driving GCM for CORDEX RCM simulations") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "top",
        legend.background = element_rect(color = "black", fill = "white", linewidth = 0.5),
        legend.key = element_rect(fill = "white", color = NA),
        legend.title = element_text(hjust = 0.5) ) + # cleaner keys inside) +
  guides(color = guide_legend(nrow = 2,title.position = "top", byrow = TRUE))



## fixed source_pr2 ##

# df_Qy_change45 %>% 
#   ggplot(aes(GCM, Qy_change, color = hmodel)) + 
#   geom_point() + 
#   geom_line() +
#   facet_wrap(~source_pr,nrow = 3) + 
#   theme_bw()
# 
# df_Qy_change85 %>% 
#   ggplot(aes(GCM, Qy_change, color = hmodel)) + 
#   geom_point() + 
#   geom_line() +
#   facet_wrap(~source_pr,nrow = 3) +
#   theme_bw()

df_Qy_change45 %>% rbind(df_Qy_change85) %>%  # this seems the BEST
  ggplot(aes(GCM, Qy_change, color = hmodel)) +   
  geom_point() + 
  facet_grid(scenario~source_pr,scales = "free_y") +
  labs(y = "Mean annual runoff change (%)", 
       x = "Driving GCM for CORDEX RCM simulations",
       color = "Hydrological Model") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "top",
        legend.background = element_rect(color = "black", fill = "white", linewidth = 0.5),
        legend.key = element_rect(fill = "white", color = NA)) + # cleaner keys inside) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))




