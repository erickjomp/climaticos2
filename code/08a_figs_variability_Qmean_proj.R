library(tidyverse)
remotes::install_github("https://github.com/erickjomp/Rthemes.git")
library(Rthemes)

#### INPUTS ####
file_GR2M <- "output/07_Q_projections/df_Q_GR2M_proj.rds"
file_HyMod <- "output/07_Q_projections/df_Q_HyMod_proj.rds"
file_HBV <- "output/07_Q_projections/df_Q_HBV_proj.rds"

file_periods <- "output/06_bias_correction_CORDEX_series/input/periods_analysis_start_end.csv"

base_size_figs <- 8

#### OUTPUTS ####
file_plot_precipmethod_1 <- "output/08_plots_results/fig_variability_dueto_precipestimation_1.pdf"
file_plot_precipmethod_2 <- "output/08_plots_results/fig_variability_dueto_precipestimation_2.pdf"
file_plot_precipmethod_2_alt_rcp45 <- "output/08_plots_results/fig_variability_dueto_precipestimation_2_alternative_RCP45.pdf"
file_plot_precipmethod_2_alt_rcp85 <- "output/08_plots_results/fig_variability_dueto_precipestimation_2_alternative_RCP85.pdf"

file_plot_hydromodel_1 <- "output/08_plots_results/fig_variability_dueto_hydromodel_1.pdf"
file_plot_hydromodel_2 <- "output/08_plots_results/fig_variability_dueto_hydromodel_2.pdf"

#### READING ####
df_GR2M <-  readRDS(file_GR2M) %>% mutate(hmodel = "GR2M")
df_HyMod <- readRDS(file_HyMod) %>% mutate(hmodel = "HyMod")
df_HBV <- readRDS(file_HBV) %>% mutate(hmodel = "HBV")

df_all <- rbind(df_GR2M, df_HyMod, df_HBV) %>% 
  mutate(across(c(GCM, hmodel, source_pr, scenario), ~ fct_inorder(.x)))

df_periods <- read.csv(file_periods, header = FALSE, row.names = 1) %>% 
  t() %>% as_tibble() %>% mutate_all(as.POSIXct) 
print(t(df_periods))

#### PROCESS ####
df_Qy <- df_all %>%
  filter((scenario == "historical" & 
            dates >= df_periods$date_begin_historic &
            dates < df_periods$date_end_historic) |
           (scenario != "historical" & 
              dates >= df_periods$date_begin_analysis_nearfuture &
              dates < df_periods$date_end_analysis_nearfuture)) %>% 
  group_by(GCM, source_pr, hmodel, scenario) %>% 
  summarise(mean_Qy = mean(Qm3s) * 12)

df_Qy_change45 <- df_Qy %>% 
  spread(scenario, mean_Qy) %>% 
  mutate(Qy_change = 100 * ((rcp45 - historical) / historical)) %>% 
  select(-rcp45, -rcp85, -historical) %>% 
  mutate(scenario = "RCP 4.5")

df_Qy_change85 <- df_Qy %>% 
  spread(scenario, mean_Qy) %>% 
  mutate(Qy_change = 100 * ((rcp85 - historical) / historical)) %>% 
  select(-rcp45, -rcp85, -historical) %>% 
  mutate(scenario = "RCP 8.5")

#### FUNCTION TO CALCULATE STATISTICS PER FACET ####
get_facet_stats <- function(df, group_vars, x_var) {
  # Calculate range (max - min) for each column within each facet
  df %>%
    group_by(across(all_of(group_vars)), .data[[x_var]]) %>%
    summarise(
      range_val = max(Qy_change, na.rm = TRUE) - min(Qy_change, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Get min and max range per facet
    group_by(across(all_of(group_vars))) %>%
    summarise(
      min_range = min(range_val, na.rm = TRUE),
      max_range = max(range_val, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      label = sprintf("Min. Range: %.1f%%\nMax. Range: %.1f%%", min_range, max_range)
    )
}

#### FUNCTION TO CALCULATE MIN/MAX FOR LINES ####
get_minmax_lines <- function(df, group_vars, x_var) {
  df %>%
    group_by(across(all_of(group_vars)), .data[[x_var]]) %>%
    summarise(
      min_val = min(Qy_change, na.rm = TRUE),
      max_val = max(Qy_change, na.rm = TRUE),
      .groups = "drop"
    )
}

#### PLOTS PRECIP ESTIMATION METHOD VARIABILITY ####

## Plot 1: fig_variability_dueto_precipestimation_1 (LOWER RIGHT) ##
df_plot1 <- df_Qy_change45 %>% rbind(df_Qy_change85)
stats_plot1 <- get_facet_stats(df_plot1, c("scenario", "hmodel"), "source_pr")
minmax_plot1 <- get_minmax_lines(df_plot1, c("scenario", "hmodel"), "source_pr")

df_plot1 %>%
  ggplot(aes(source_pr, Qy_change, color = GCM)) +   
  # Vertical line connecting min to max (transparent)
  geom_segment(data = minmax_plot1, aes(x = source_pr, xend = source_pr, 
                                        y = min_val, yend = max_val),
               inherit.aes = FALSE, color = "grey30", linewidth = 0.5, alpha = 0.4) +
  # Horizontal line at minimum
  geom_segment(data = minmax_plot1, aes(x = as.numeric(source_pr) - 0.15, xend = as.numeric(source_pr) + 0.15, 
                                        y = min_val, yend = min_val),
               inherit.aes = FALSE, color = "grey70", linewidth = 0.5) +
  # Horizontal line at maximum
  geom_segment(data = minmax_plot1, aes(x = as.numeric(source_pr) - 0.15, xend = as.numeric(source_pr) + 0.15, 
                                        y = max_val, yend = max_val),
               inherit.aes = FALSE, color = "grey70", linewidth = 0.5) +
  geom_point(size = 1) +
  geom_text(data = stats_plot1, 
            aes(x = Inf, y = -Inf, label = label),
            inherit.aes = FALSE,
            hjust = 1.05, vjust = -0.3,
            size = 1.5, fontface = "bold",
            color = "black") +
  facet_grid(scenario ~ hmodel, scales = "free_y") +
  labs(y = "Mean annual runoff change (%)", 
       x = "Precipitation estimation method",
       color = "Driving GCM for CORDEX RCM simulations") + 
  my_ggtheme(base_size = base_size_figs) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "top",
        legend.background = element_rect(color = "black", fill = "white", linewidth = 0.5),
        legend.key = element_rect(fill = "white", color = NA),
        legend.title = element_text(hjust = 0.5)) +
  guides(color = guide_legend(nrow = 2, title.position = "top", byrow = TRUE)) 

ggsave(filename = file_plot_precipmethod_1,
       width = 190, height = 120, units = "mm")

## Plot 2: fig_variability_dueto_precipestimation_2 (UPPER RIGHT) ##
df_plot2 <- df_Qy_change45 %>% rbind(df_Qy_change85)
stats_plot2 <- get_facet_stats(df_plot2, c("scenario", "hmodel"), "GCM")
minmax_plot2 <- get_minmax_lines(df_plot2, c("scenario", "hmodel"), "GCM")

df_plot2 %>%
  ggplot(aes(GCM, Qy_change, color = source_pr)) +   
  # Vertical line connecting min to max (transparent)
  geom_segment(data = minmax_plot2, aes(x = GCM, xend = GCM, 
                                        y = min_val, yend = max_val),
               inherit.aes = FALSE, color = "grey30", linewidth = 0.5, alpha = 0.4) +
  # Horizontal line at minimum
  geom_segment(data = minmax_plot2, aes(x = as.numeric(GCM) - 0.25, xend = as.numeric(GCM) + 0.25, 
                                        y = min_val, yend = min_val),
               inherit.aes = FALSE, color = "grey70", linewidth = 0.5) +
  # Horizontal line at maximum
  geom_segment(data = minmax_plot2, aes(x = as.numeric(GCM) - 0.25, xend = as.numeric(GCM) + 0.25, 
                                        y = max_val, yend = max_val),
               inherit.aes = FALSE, color = "grey70", linewidth = 0.5) +
  geom_point(size = 1) +
  geom_text(data = stats_plot2, 
            aes(x = Inf, y = Inf, label = label),
            inherit.aes = FALSE,
            hjust = 1.05, vjust = 1.3,
            size = 1.5, fontface = "bold",
            color = "black") +
  facet_grid(scenario ~ hmodel, scales = "free_y") +
  labs(y = "Mean annual runoff change (%)", 
       x = "Driving GCM for CORDEX RCM simulations",
       color = "Precipitation estimation method") + 
  my_ggtheme(base_size = base_size_figs) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "top",
        legend.background = element_rect(color = "black", fill = "white", linewidth = 0.5),
        legend.key = element_rect(fill = "white", color = NA)) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

ggsave(filename = file_plot_precipmethod_2,
       width = 190, height = 120, units = "mm")


## Plot 2 ALTERNATIVE: fig_variability_dueto_precipestimation_2_alternative (UPPER RIGHT) ##
## X-axis: hmodel, Facets by GCM ##

## Plot 2 ALTERNATIVE RCP 4.5: Only RCP 4.5 scenario ##
df_plot2_alt_rcp45 <- df_Qy_change45
stats_plot2_alt_rcp45 <- get_facet_stats(df_plot2_alt_rcp45, c("GCM"), "hmodel")
minmax_plot2_alt_rcp45 <- get_minmax_lines(df_plot2_alt_rcp45, c("GCM"), "hmodel")

df_plot2_alt_rcp45 %>%
  ggplot(aes(hmodel, Qy_change, color = source_pr)) +   
  # Vertical line connecting min to max (transparent)
  geom_segment(data = minmax_plot2_alt_rcp45, aes(x = hmodel, xend = hmodel, 
                                                  y = min_val, yend = max_val),
               inherit.aes = FALSE, color = "grey30", linewidth = 0.5, alpha = 0.4) +
  # Horizontal line at minimum
  geom_segment(data = minmax_plot2_alt_rcp45, aes(x = as.numeric(hmodel) - 0.15, xend = as.numeric(hmodel) + 0.15, 
                                                  y = min_val, yend = min_val),
               inherit.aes = FALSE, color = "grey50", linewidth = 0.7) +
  # Horizontal line at maximum
  geom_segment(data = minmax_plot2_alt_rcp45, aes(x = as.numeric(hmodel) - 0.15, xend = as.numeric(hmodel) + 0.15, 
                                                  y = max_val, yend = max_val),
               inherit.aes = FALSE, color = "grey50", linewidth = 0.7) +
  geom_point(size = 0.8) +
  geom_text(data = stats_plot2_alt_rcp45, 
            aes(x = Inf, y = -Inf, label = label),
            inherit.aes = FALSE,
            hjust = 1.05, vjust = -0.3,
            size = 1.3, fontface = "bold",
            color = "black") +
  facet_wrap(~ GCM, nrow = 2, scales = "free_y") +
  labs(y = "Mean annual runoff change (%)", 
       x = "Hydrological model",
       color = "Precipitation estimation method",
       title = "RCP 4.5") + 
  my_ggtheme(base_size = base_size_figs) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 5.5),
        axis.text.y = element_text(size = 6),
        axis.title = element_text(size = 7),
        strip.text = element_text(size = 3.5, margin = margin(1, 0.5, 1, 0.5)),
        legend.position = "top",
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 6.5),
        legend.background = element_rect(color = "black", fill = "white", linewidth = 0.5),
        legend.key = element_rect(fill = "white", color = NA),
        legend.key.size = unit(0.35, "cm"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 8),
        panel.spacing.x = unit(0.05, "lines"),
        panel.spacing.y = unit(0.15, "lines"),
        plot.margin = margin(1, 1, 1, 1),
        aspect.ratio = 1.2) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))

ggsave(filename = file_plot_precipmethod_2_alt_rcp45,
       width = 190, height = 110, units = "mm")

## Plot 2 ALTERNATIVE RCP 8.5: Only RCP 8.5 scenario ##
df_plot2_alt_rcp85 <- df_Qy_change85
stats_plot2_alt_rcp85 <- get_facet_stats(df_plot2_alt_rcp85, c("GCM"), "hmodel")
minmax_plot2_alt_rcp85 <- get_minmax_lines(df_plot2_alt_rcp85, c("GCM"), "hmodel")

df_plot2_alt_rcp85 %>%
  ggplot(aes(hmodel, Qy_change, color = source_pr)) +   
  # Vertical line connecting min to max (transparent)
  geom_segment(data = minmax_plot2_alt_rcp85, aes(x = hmodel, xend = hmodel, 
                                                  y = min_val, yend = max_val),
               inherit.aes = FALSE, color = "grey30", linewidth = 0.5, alpha = 0.4) +
  # Horizontal line at minimum
  geom_segment(data = minmax_plot2_alt_rcp85, aes(x = as.numeric(hmodel) - 0.15, xend = as.numeric(hmodel) + 0.15, 
                                                  y = min_val, yend = min_val),
               inherit.aes = FALSE, color = "grey50", linewidth = 0.7) +
  # Horizontal line at maximum
  geom_segment(data = minmax_plot2_alt_rcp85, aes(x = as.numeric(hmodel) - 0.15, xend = as.numeric(hmodel) + 0.15, 
                                                  y = max_val, yend = max_val),
               inherit.aes = FALSE, color = "grey50", linewidth = 0.7) +
  geom_point(size = 0.8) +
  geom_text(data = stats_plot2_alt_rcp85, 
            aes(x = Inf, y = -Inf, label = label),
            inherit.aes = FALSE,
            hjust = 1.05, vjust = -0.3,
            size = 1.3, fontface = "bold",
            color = "black") +
  facet_wrap(~ GCM, nrow = 2, scales = "free_y") +
  labs(y = "Mean annual runoff change (%)", 
       x = "Hydrological model",
       color = "Precipitation estimation method",
       title = "RCP 8.5") + 
  my_ggtheme(base_size = base_size_figs) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 5.5),
        axis.text.y = element_text(size = 6),
        axis.title = element_text(size = 7),
        strip.text = element_text(size = 3.5, margin = margin(1, 0.5, 1, 0.5)),
        legend.position = "top",
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 6.5),
        legend.background = element_rect(color = "black", fill = "white", linewidth = 0.5),
        legend.key = element_rect(fill = "white", color = NA),
        legend.key.size = unit(0.35, "cm"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 8),
        panel.spacing.x = unit(0.05, "lines"),
        panel.spacing.y = unit(0.15, "lines"),
        plot.margin = margin(1, 1, 1, 1),
        aspect.ratio = 1.2) +
  guides(color = guide_legend(nrow = 1, byrow = TRUE))

ggsave(filename = file_plot_precipmethod_2_alt_rcp85,
       width = 190, height = 110, units = "mm")


#### PLOTS HYDROLOGICAL MODEL VARIABILITY ####

## Plot 3: fig_variability_dueto_hydromodel_1 (LOWER RIGHT) ##
df_plot3 <- df_Qy_change45 %>% rbind(df_Qy_change85)
stats_plot3 <- get_facet_stats(df_plot3, c("scenario", "source_pr"), "hmodel")
minmax_plot3 <- get_minmax_lines(df_plot3, c("scenario", "source_pr"), "hmodel")

df_plot3 %>%
  ggplot(aes(hmodel, Qy_change, color = GCM)) +   
  # Vertical line connecting min to max (transparent)
  geom_segment(data = minmax_plot3, aes(x = hmodel, xend = hmodel, 
                                        y = min_val, yend = max_val),
               inherit.aes = FALSE, color = "grey30", linewidth = 0.5, alpha = 0.4) +
  # Horizontal line at minimum
  geom_segment(data = minmax_plot3, aes(x = as.numeric(hmodel) - 0.15, xend = as.numeric(hmodel) + 0.15, 
                                        y = min_val, yend = min_val),
               inherit.aes = FALSE, color = "grey70", linewidth = 0.5) +
  # Horizontal line at maximum
  geom_segment(data = minmax_plot3, aes(x = as.numeric(hmodel) - 0.15, xend = as.numeric(hmodel) + 0.15, 
                                        y = max_val, yend = max_val),
               inherit.aes = FALSE, color = "grey70", linewidth = 0.5) +
  geom_point(size = 1) +
  geom_text(data = stats_plot3, 
            aes(x = Inf, y = -Inf, label = label),
            inherit.aes = FALSE,
            hjust = 1.05, vjust = -0.3,
            size = 1.5, fontface = "bold",
            color = "black") +
  facet_grid(scenario ~ source_pr, scales = "free_y") +
  labs(y = "Mean annual runoff change (%)", 
       x = "Hydrological model",
       color = "Driving GCM for CORDEX RCM simulations") + 
  my_ggtheme(base_size = base_size_figs) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "top",
        legend.background = element_rect(color = "black", fill = "white", linewidth = 0.5),
        legend.key = element_rect(fill = "white", color = NA),
        legend.title = element_text(hjust = 0.5)) +
  guides(color = guide_legend(nrow = 2, title.position = "top", byrow = TRUE))

ggsave(filename = file_plot_hydromodel_1,
       width = 190, height = 120, units = "mm")

## Plot 4: fig_variability_dueto_hydromodel_2 (UPPER RIGHT) ##
df_plot4 <- df_Qy_change45 %>% rbind(df_Qy_change85)
stats_plot4 <- get_facet_stats(df_plot4, c("scenario", "source_pr"), "GCM")
minmax_plot4 <- get_minmax_lines(df_plot4, c("scenario", "source_pr"), "GCM")

df_plot4 %>%
  ggplot(aes(GCM, Qy_change, color = hmodel)) +   
  # Vertical line connecting min to max (transparent)
  geom_segment(data = minmax_plot4, aes(x = GCM, xend = GCM, 
                                        y = min_val, yend = max_val),
               inherit.aes = FALSE, color = "grey30", linewidth = 0.5, alpha = 0.4) +
  # Horizontal line at minimum
  geom_segment(data = minmax_plot4, aes(x = as.numeric(GCM) - 0.38, xend = as.numeric(GCM) + 0.38, 
                                        y = min_val, yend = min_val),
               inherit.aes = FALSE, color = "grey70", linewidth = 0.5) +
  # Horizontal line at maximum
  geom_segment(data = minmax_plot4, aes(x = as.numeric(GCM) - 0.38, xend = as.numeric(GCM) + 0.38, 
                                        y = max_val, yend = max_val),
               inherit.aes = FALSE, color = "grey70", linewidth = 0.5) +
  geom_point(size = 1) + 
  geom_text(data = stats_plot4, 
            aes(x = Inf, y = Inf, label = label),
            inherit.aes = FALSE,
            hjust = 1.05, vjust = 1.3,
            size = 1.5, fontface = "bold",
            color = "black") +
  facet_grid(scenario ~ source_pr, scales = "free_y") +
  labs(y = "Mean annual runoff change (%)", 
       x = "Driving GCM for CORDEX RCM simulations",
       color = "Hydrological Model") + 
  my_ggtheme(base_size = base_size_figs) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "top",
        legend.background = element_rect(color = "black", fill = "white", linewidth = 0.5),
        legend.key = element_rect(fill = "white", color = NA)) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

ggsave(filename = file_plot_hydromodel_2,
       width = 190, height = 120, units = "mm")

