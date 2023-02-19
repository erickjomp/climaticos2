library(tidyverse)
Sys.setenv(TZ = 'GMT')

#### INPUT ####
file_qsim_series__gen <-
  "output/03_hydrological_model_cal_val/df_Qm3s_simulations__%s.rds"
names_hmodels <- c("GR2M", "HBV", "HyMod")

file_qobs_serie <-
  "output/00_processed_data/df_Q_m.rds"

file_pr_series__inters <-
  "output/02_interpolation_pr/df_pr_m__inter.rds"
file_pr_serie__pisco <-
  "output/00_processed_data/df_pr_m__pisco.rds"

# for plot
exp_fac <- 0.33
base_size_figs <- 8

# start end periods warmup calib
file_periods_def <- 
  "output/03_hydrological_model_cal_val/input/hmodel_start_end_periods.csv"




#### OUTPUT ####
file_plot_series <-
  "output/03_hydrological_model_cal_val/plots/qsim_series.pdf"


#### reading input ####
list_df_qsim__hmodels <-
  lapply(names_hmodels, function(hmodel) {
    df_qsim__GR2M <- readRDS(sprintf(file_qsim_series__gen, hmodel))
  }) %>% setNames(names_hmodels)


df_qobs <- readRDS(file_qobs_serie) %>% rename_at(2, ~"Qobs")

df_pr__inters <- readRDS(file_pr_series__inters)
df_pr__pisco <- readRDS(file_pr_serie__pisco)
df_pr__all <- left_join(df_pr__inters, df_pr__pisco) 


df_periods_def <- read.csv(file_periods_def, header = F,
                           row.names = 1) %>% 
  mutate_at(1, as.POSIXct) %>% t() %>% as_tibble() %>% 
  mutate_all(as.POSIXct)
print(t(df_periods_def))

#### processing ####

df_pr__all_filt_gat <- df_pr__all %>% 
  filter(dates >= min(list_df_qsim__hmodels[[i]]$dates),
         dates <= max(list_df_qsim__hmodels[[i]]$dates)) %>% 
  gather("pr_source","pr",-1)

max_Q <- max(c(max(list_df_qsim__hmodels[[i]][-1]),
               df_qobs[[2]]), na.rm = T )#
max_pr <- max(df_pr__all_filt_gat$pr, na.rm = T)

maxRange <- (1 + exp_fac) * max_Q 
fac <- exp_fac * max_Q / max_pr   

breaks_Q <- seq(0, round(max_Q / 40 ) * 40, 
                by = round(max_Q / 40 )*10)
breaks_pr <- seq(0, ceiling(max_pr / 300 ) * 300, 
                 by = ceiling(max_pr / 300 )*100)

# positions text
x_annot_cal <- df_periods_def$date_begin_calib + 
  (df_periods_def$date_end_calib - df_periods_def$date_begin_calib ) / 2

x_annot_val <- df_periods_def$date_end_calib + 
  (max(list_df_qsim__hmodels[[i]]$dates) - 
     df_periods_def$date_end_calib ) / 2

list_df_qsim__hmodels <- 
  lapply( names(list_df_qsim__hmodels),function(name_hmodel){
  list_df_qsim__hmodels[[name_hmodel]] %>% 
    mutate(Q_type = name_hmodel, .after = dates)
    })

names(df_qobs)[2] <- "Observed"
df_plot <- 
  list_df_qsim__hmodels %>% do.call(rbind,.) %>% 
  gather('pr_source','Q',-c("dates","Q_type")) %>%
  mutate_at(vars(pr_source),funs(factor(., levels=unique(.)))) %>% 
  spread("Q_type","Q") %>% 
  left_join(df_qobs) %>% 
  select("dates","pr_source", "Observed",all_of(names_hmodels)) %>% 
  gather("Q_type", "Q", -c("dates", "pr_source")) %>% 
  left_join(df_pr__all_filt_gat)

df_plot <- df_plot %>%  
  mutate_at(vars(pr_source, Q_type), 
            funs(factor(., levels=unique(.))))
  
df_plot %>% 
  ggplot() + 
  geom_tile(aes(x = dates ,y = -pr*fac/2+maxRange,
                height = pr*fac,width= 20*24*60*60),
            color = "royalblue",
            fill = "royalblue"
  )  +
  geom_point(aes(dates, Q, 
                 color = Q_type), size = 0.4 * base_size_figs / 12) + 
  geom_line(aes(dates,Q,
                color=Q_type, 
                linetype = Q_type,
                linewidth = Q_type)
            ) + 
  scale_color_manual(values = c('black','red',"blue","green4"),
                     # labels = c('Observed','Simulated')
                     ) +
  scale_linetype_manual(
    values = c("solid",rep("dashed", length(names_hmodels)))
  ) +
  scale_discrete_manual(aesthetic = "linewidth",
                        values = c(1*base_size_figs/12,
                                   rep(0.6*base_size_figs/12,
                                       length(names_hmodels)))) +
  scale_x_datetime(breaks = '2 year',
                   date_labels = '%Y-%m',expand = c(0,0)) + 
  theme(axis.text.x = 
          element_text(angle = -45, vjust = 1, hjust=0)) +  
  scale_y_continuous(
    name = expression(paste("Flow (", m^{3}/s,")")),
    breaks = breaks_Q,
    # "Discharge (cfs)"  #
    limits = c(0,maxRange*1.008),
    expand = c(0, 0),n.breaks = 4,   
    sec.axis = sec_axis(trans = ~-(1/fac)*(.-maxRange),
                        breaks = breaks_pr,
                        name = 'Precipitation (mm)')) +
  facet_wrap(~pr_source,ncol=1) +
  geom_vline(xintercept = df_periods_def$date_end_calib,
             linetype = 'dashed',color = "grey40") +
  annotate("text", 
           x = x_annot_cal ,        #### PARA UNO SOLO
           y = maxRange * 1 / (1+ exp_fac * 1.5),
           label = "Calibration", angle=00,
           size = 6 * base_size_figs /12,   
           colour='grey40',  
           # fontface ="bold"
           ) + 
  annotate("text", x = x_annot_val, 
           y = maxRange * 1 / (1+ exp_fac * 1.5), 
           label = "Validation", angle=00, 
           size= 6 * base_size_figs /12,       
           colour='grey40', 
           # fontface ="bold"
           ) + 
  # theme(strip.background =element_rect(fill="grey85",
  #                                      color = 'black'))+  
  ggthemes::theme_few(base_size = base_size_figs) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, 
                                size = 1.1*base_size_figs/12),
    # line = element_line(colour = "black"),
    # panel.border = element_rect(colour = "black"),
    # legend.position = "bottom",
    # axis.text = element_blank(),
    # axis.ticks = element_blank(),
    panel.grid.major = element_line(colour = "grey85"), #"grey80"
    # panel.grid.minor.x = element_line(colour = "grey85"),
    # panel.grid.minor.y = element_line(colour = "grey85"),
    # panel.grid.minor.y = element_line(colour = "transparent"),
    strip.text.x = element_text(size = base_size_figs * 1.1),
    strip.text.y = element_text(size = base_size_figs * 1.1),
    legend.background = element_blank(),
    legend.box.background = element_rect(colour = "black"),
    legend.title = element_blank(),
    # legend.position = c(.95, .65),      #### PARA UNO SOLO
    # legend.position = c(.8, .2),
    legend.position = c("bottom"),
    # legend.justification = c("right", "top"),
    # legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    # line = element_line(linetype = 'dashed')
  ) + 
  labs(x = NULL)

ggsave(filename = file_plot_series,
       width = 140, height = 200, units = "mm" )
