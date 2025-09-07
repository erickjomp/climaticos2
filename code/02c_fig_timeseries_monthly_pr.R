library(tidyverse)
library(terra)
library(sf)
library(metR)
# remotes::install_github("https://github.com/erickjomp/Rthemes.git")
library(Rthemes)

#### INPUTS ####
file_df_pr_m__inter_rds <- "output/02_interpolation_pr/df_pr_m__inter.rds"
file_df_pr_m__pisco_rds <- "output/00_processed_data/df_pr_m__pisco.rds"

#### OUTPUT ####
file_plot <- 
  "output/02_interpolation_pr/plots/fig_timeseries_prm.pdf"

#### reading data ####
df_prm_inters <- readRDS(file_df_pr_m__inter_rds)
df_prm_pisco <- readRDS(file_df_pr_m__pisco_rds)

#### joining and plotting ####
df_prm <- df_prm_inters %>% left_join(df_prm_pisco) 

base_size_figs <- 8 # 7 : recommended for normal text by elsevier

my_colors <- c("#00BFFF", "orange", "green", "blue", "red")

df_plot <- df_prm %>% 
  drop_na() %>% 
  gather("method","pr",-1) %>% 
  mutate(method = factor(method,levels = unique(method))) %>% #.$method
  filter(dates >= "2000-01-01") 

(
plot_timeseries <- 
  df_plot %>%  
  ggplot(aes(dates, pr,col = method)) +
  geom_line(linewidth = 0.2) + 
  scale_x_datetime(expand = c(0, 0),breaks = "2 years",date_labels = "%Y") +
  scale_y_continuous(breaks = seq(0,max(df_plot$pr),by=50)) + 
  # geom_point() +
  # facet_wrap(~method) +
  my_ggtheme(base_size = base_size_figs) +
  labs(x= NULL, y = "Precipitation (mm/month)") + 
  theme(
    legend.position = c(0.98, 0.98),#0.98),   # (x,y) in npc units (0–1), top center
    legend.justification = c("right","top"),#c("center", "top"),
    legend.direction = "horizontal", # horizontal layout
    legend.box.just = "center",
    legend.background = element_rect(           # rectangle around legend
      fill = "white", 
      colour = "black"
    )
  ) +
  scale_color_manual(values = my_colors)  +
  guides(color = guide_legend(title.position = "top", title.hjust=0.5,
                              title = "Precipitation estimation method"))
)


ggsave(plot_timeseries,
       filename = file_plot,
       width = 190,height = 80, #98,
       units = "mm")

  