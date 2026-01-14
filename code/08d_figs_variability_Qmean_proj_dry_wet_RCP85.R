library(tidyverse)
library(lubridate)
remotes::install_github("https://github.com/erickjomp/Rthemes.git")
library(Rthemes)

#### INPUTS ####
file_GR2M <- "output/07_Q_projections/df_Q_GR2M_proj.rds"
file_HyMod <- "output/07_Q_projections/df_Q_HyMod_proj.rds"
file_HBV <- "output/07_Q_projections/df_Q_HBV_proj.rds"

file_periods <- "output/06_bias_correction_CORDEX_series/input/periods_analysis_start_end.csv"

base_size_figs <- 8

#### OUTPUTS ####
# Wet season outputs
file_plot_precipmethod_1_wet <- "output/08_plots_results/fig_variability_dueto_precipestimation_1_RCP85_wet.pdf"
file_plot_precipmethod_2_wet <- "output/08_plots_results/fig_variability_dueto_precipestimation_2_RCP85_wet.pdf"
file_plot_precipmethod_2_alt_rcp85_wet <- "output/08_plots_results/fig_variability_dueto_precipestimation_2_alternative_RCP85_wet.pdf"
file_plot_hydromodel_1_wet <- "output/08_plots_results/fig_variability_dueto_hydromodel_1_RCP85_wet.pdf"
file_plot_hydromodel_2_wet <- "output/08_plots_results/fig_variability_dueto_hydromodel_2_RCP85_wet.pdf"

# Dry season outputs
file_plot_precipmethod_1_dry <- "output/08_plots_results/fig_variability_dueto_precipestimation_1_RCP85_dry.pdf"
file_plot_precipmethod_2_dry <- "output/08_plots_results/fig_variability_dueto_precipestimation_2_RCP85_dry.pdf"
file_plot_precipmethod_2_alt_rcp85_dry <- "output/08_plots_results/fig_variability_dueto_precipestimation_2_alternative_RCP85_dry.pdf"
file_plot_hydromodel_1_dry <- "output/08_plots_results/fig_variability_dueto_hydromodel_1_RCP85_dry.pdf"
file_plot_hydromodel_2_dry <- "output/08_plots_results/fig_variability_dueto_hydromodel_2_RCP85_dry.pdf"

#### READING ####
df_GR2M <-  readRDS(file_GR2M) %>% mutate(hmodel = "GR2M")
df_HyMod <- readRDS(file_HyMod) %>% mutate(hmodel = "HyMod")
df_HBV <- readRDS(file_HBV) %>% mutate(hmodel = "HBV")

df_all <- rbind(df_GR2M, df_HyMod, df_HBV) %>% 
  mutate(across(c(GCM, hmodel, source_pr, scenario), ~ fct_inorder(.x)))

df_periods <- read.csv(file_periods, header = FALSE, row.names = 1) %>% 
  t() %>% as_tibble() %>% mutate_all(as.POSIXct) 
print(t(df_periods))

#### FUNCTION TO CLASSIFY SEASON ####
classify_season <- function(date) {
  month_val <- month(date)
  # Wet season: November (11) to April (4)
  # Dry season: May (5) to October (10)
  ifelse(month_val >= 11 | month_val <= 4, "wet", "dry")
}

#### PROCESS ####
# Add season column to the data
df_all <- df_all %>%
  mutate(season = classify_season(dates))

#### FUNCTION TO CALCULATE STATISTICS PER FACET ####
get_facet_stats <- function(df, group_vars, x_var) {
  df %>%
    group_by(across(all_of(group_vars)), .data[[x_var]]) %>%
    summarise(
      range_val = max(Qy_change, na.rm = TRUE) - min(Qy_change, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      min_range = min(range_val, na.rm = TRUE),
      max_range = max(range_val, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      label = sprintf("Min. Spread: %.1f%%\nMax. Spread: %.1f%%", min_range, max_range)
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

#### FUNCTION TO PROCESS DATA BY SEASON ####
process_season_data <- function(df_input, season_name) {
  # Filter by season and periods
  df_filtered <- df_input %>%
    filter(season == season_name) %>%
    filter((scenario == "historical" & 
              dates >= df_periods$date_begin_historic &
              dates < df_periods$date_end_historic) |
             (scenario != "historical" & 
                dates >= df_periods$date_begin_analysis_nearfuture &
                dates < df_periods$date_end_analysis_nearfuture))
  
  # Calculate mean Q for the season (multiply by number of months in season)
  # Wet season: 6 months, Dry season: 6 months
  df_Qy <- df_filtered %>%
    group_by(GCM, source_pr, hmodel, scenario) %>% 
    summarise(mean_Qy = mean(Qm3s) * 6, .groups = "drop")
  
  # Calculate changes for RCP 8.5 only
  df_Qy_change85 <- df_Qy %>% 
    spread(scenario, mean_Qy) %>% 
    mutate(Qy_change = 100 * ((rcp85 - historical) / historical)) %>% 
    select(-rcp45, -rcp85, -historical) %>% 
    mutate(scenario = "RCP 8.5")
  
  return(df_Qy_change85)
}

#### FUNCTION TO CREATE PLOTS ####
create_plot_1 <- function(df_plot, season_label, file_output) {
  stats_plot <- get_facet_stats(df_plot, c("hmodel"), "source_pr")
  minmax_plot <- get_minmax_lines(df_plot, c("hmodel"), "source_pr")
  
  p <- df_plot %>%
    ggplot(aes(source_pr, Qy_change, color = GCM)) +   
    geom_segment(data = minmax_plot, aes(x = source_pr, xend = source_pr, 
                                         y = min_val, yend = max_val),
                 inherit.aes = FALSE, color = "grey30", linewidth = 0.5, alpha = 0.4) +
    geom_segment(data = minmax_plot, aes(x = as.numeric(source_pr) - 0.15, xend = as.numeric(source_pr) + 0.15, 
                                         y = min_val, yend = min_val),
                 inherit.aes = FALSE, color = "grey70", linewidth = 0.5) +
    geom_segment(data = minmax_plot, aes(x = as.numeric(source_pr) - 0.15, xend = as.numeric(source_pr) + 0.15, 
                                         y = max_val, yend = max_val),
                 inherit.aes = FALSE, color = "grey70", linewidth = 0.5) +
    geom_point(size = 1) +
    geom_text(data = stats_plot, 
              aes(x = Inf, y = -Inf, label = label),
              inherit.aes = FALSE,
              hjust = 1.05, vjust = -0.3,
              size = 1.5, fontface = "bold",
              color = "black") +
    facet_wrap(~ hmodel, scales = "free_y") +
    labs(y = paste0("Mean seasonal runoff change (%) - ", season_label), 
         x = "Precipitation estimation method",
         color = "Driving GCM for CORDEX RCM simulations",
         title = "RCP 8.5") + 
    my_ggtheme(base_size = base_size_figs) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          legend.position = "top",
          legend.background = element_rect(color = "black", fill = "white", linewidth = 0.5),
          legend.key = element_rect(fill = "white", color = NA),
          legend.title = element_text(hjust = 0.5),
          plot.title = element_text(hjust = 0.5, face = "bold")) +
    guides(color = guide_legend(nrow = 2, title.position = "top", byrow = TRUE))
  
  ggsave(filename = file_output, plot = p, width = 190, height = 110, units = "mm")
  return(p)
}

create_plot_2 <- function(df_plot, season_label, file_output) {
  stats_plot <- get_facet_stats(df_plot, c("hmodel"), "GCM")
  minmax_plot <- get_minmax_lines(df_plot, c("hmodel"), "GCM")
  
  p <- df_plot %>%
    ggplot(aes(GCM, Qy_change, color = source_pr)) +   
    geom_segment(data = minmax_plot, aes(x = GCM, xend = GCM, 
                                         y = min_val, yend = max_val),
                 inherit.aes = FALSE, color = "grey30", linewidth = 0.5, alpha = 0.4) +
    geom_segment(data = minmax_plot, aes(x = as.numeric(GCM) - 0.25, xend = as.numeric(GCM) + 0.25, 
                                         y = min_val, yend = min_val),
                 inherit.aes = FALSE, color = "grey70", linewidth = 0.5) +
    geom_segment(data = minmax_plot, aes(x = as.numeric(GCM) - 0.25, xend = as.numeric(GCM) + 0.25, 
                                         y = max_val, yend = max_val),
                 inherit.aes = FALSE, color = "grey70", linewidth = 0.5) +
    geom_point(size = 1) +
    geom_text(data = stats_plot, 
              aes(x = Inf, y = Inf, label = label),
              inherit.aes = FALSE,
              hjust = 1.05, vjust = 1.3,
              size = 1.6, fontface = "bold",
              color = "black") +
    facet_wrap(~ hmodel, scales = "free_y") +
    labs(y = paste0("Mean seasonal runoff change (%) - ", season_label), 
         x = "Driving GCM for CORDEX RCM simulations",
         color = "Precipitation estimation method",
         title = "RCP 8.5") + 
    my_ggtheme(base_size = base_size_figs) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          legend.position = "top",
          legend.background = element_rect(color = "black", fill = "white", linewidth = 0.5),
          legend.key = element_rect(fill = "white", color = NA),
          plot.title = element_text(hjust = 0.5, face = "bold")) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE))
  
  ggsave(filename = file_output, plot = p, width = 190, height = 110, units = "mm")
  return(p)
}

create_plot_2_alt <- function(df_plot, season_label, file_output) {
  stats_plot <- get_facet_stats(df_plot, c("GCM"), "hmodel")
  minmax_plot <- get_minmax_lines(df_plot, c("GCM"), "hmodel")
  
  p <- df_plot %>%
    ggplot(aes(hmodel, Qy_change, color = source_pr)) +   
    geom_segment(data = minmax_plot, aes(x = hmodel, xend = hmodel, 
                                         y = min_val, yend = max_val),
                 inherit.aes = FALSE, color = "grey30", linewidth = 0.5, alpha = 0.4) +
    geom_segment(data = minmax_plot, aes(x = as.numeric(hmodel) - 0.15, xend = as.numeric(hmodel) + 0.15, 
                                         y = min_val, yend = min_val),
                 inherit.aes = FALSE, color = "grey50", linewidth = 0.7) +
    geom_segment(data = minmax_plot, aes(x = as.numeric(hmodel) - 0.15, xend = as.numeric(hmodel) + 0.15, 
                                         y = max_val, yend = max_val),
                 inherit.aes = FALSE, color = "grey50", linewidth = 0.7) +
    geom_point(size = 0.8) +
    geom_text(data = stats_plot, 
              aes(x = Inf, y = Inf, label = label),
              inherit.aes = FALSE,
              hjust = 1.05, vjust = 1.3,
              size = 1.4, fontface = "bold",
              color = "black") +
    facet_wrap(~ GCM, nrow = 2, scales = "free_y") +
    labs(y = paste0("Mean seasonal runoff change (%) - ", season_label), 
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
  
  ggsave(filename = file_output, plot = p, width = 190, height = 120, units = "mm")
  return(p)
}

create_plot_3 <- function(df_plot, season_label, file_output) {
  stats_plot <- get_facet_stats(df_plot, c("source_pr"), "hmodel")
  minmax_plot <- get_minmax_lines(df_plot, c("source_pr"), "hmodel")
  
  p <- df_plot %>%
    ggplot(aes(hmodel, Qy_change, color = GCM)) +   
    geom_segment(data = minmax_plot, aes(x = hmodel, xend = hmodel, 
                                         y = min_val, yend = max_val),
                 inherit.aes = FALSE, color = "grey30", linewidth = 0.5, alpha = 0.4) +
    geom_segment(data = minmax_plot, aes(x = as.numeric(hmodel) - 0.1, xend = as.numeric(hmodel) + 0.1, 
                                         y = min_val, yend = min_val),
                 inherit.aes = FALSE, color = "grey70", linewidth = 0.5) +
    geom_segment(data = minmax_plot, aes(x = as.numeric(hmodel) - 0.1, xend = as.numeric(hmodel) + 0.1, 
                                         y = max_val, yend = max_val),
                 inherit.aes = FALSE, color = "grey70", linewidth = 0.5) +
    geom_point(size = 1) +
    geom_text(data = stats_plot, 
              aes(x = Inf, y = -Inf, label = label),
              inherit.aes = FALSE,
              hjust = 1.05, vjust = -0.3,
              size = 1.6, fontface = "bold",
              color = "black") +
    facet_wrap(~ source_pr, scales = "free_y") +
    labs(y = paste0("Mean seasonal runoff change (%) - ", season_label), 
         x = "Hydrological model",
         color = "Driving GCM for CORDEX RCM simulations",
         title = "RCP 8.5") + 
    my_ggtheme(base_size = base_size_figs) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          legend.position = "top",
          legend.background = element_rect(color = "black", fill = "white", linewidth = 0.5),
          legend.key = element_rect(fill = "white", color = NA),
          legend.title = element_text(hjust = 0.5),
          plot.title = element_text(hjust = 0.5, face = "bold")) +
    guides(color = guide_legend(nrow = 2, title.position = "top", byrow = TRUE))
  
  ggsave(filename = file_output, plot = p, width = 190, height = 120, units = "mm")
  return(p)
}

create_plot_4 <- function(df_plot, season_label, file_output) {
  stats_plot <- get_facet_stats(df_plot, c("source_pr"), "GCM")
  minmax_plot <- get_minmax_lines(df_plot, c("source_pr"), "GCM")
  
  p <- df_plot %>%
    ggplot(aes(GCM, Qy_change, color = hmodel)) +   
    geom_segment(data = minmax_plot, aes(x = GCM, xend = GCM, 
                                         y = min_val, yend = max_val),
                 inherit.aes = FALSE, color = "grey30", linewidth = 0.5, alpha = 0.4) +
    geom_segment(data = minmax_plot, aes(x = as.numeric(GCM) - 0.3, xend = as.numeric(GCM) + 0.3, 
                                         y = min_val, yend = min_val),
                 inherit.aes = FALSE, color = "grey70", linewidth = 0.5) +
    geom_segment(data = minmax_plot, aes(x = as.numeric(GCM) - 0.3, xend = as.numeric(GCM) + 0.3, 
                                         y = max_val, yend = max_val),
                 inherit.aes = FALSE, color = "grey70", linewidth = 0.5) +
    geom_point(size = 1) + 
    geom_text(data = stats_plot, 
              aes(x = Inf, y = Inf, label = label),
              inherit.aes = FALSE,
              hjust = 1.05, vjust = 1.3,
              size = 1.6, fontface = "bold",
              color = "black") +
    facet_wrap(~ source_pr, scales = "free_y") +
    labs(y = paste0("Mean seasonal runoff change (%) - ", season_label), 
         x = "Driving GCM for CORDEX RCM simulations",
         color = "Hydrological Model",
         title = "RCP 8.5") + 
    my_ggtheme(base_size = base_size_figs) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          legend.position = "top",
          legend.background = element_rect(color = "black", fill = "white", linewidth = 0.5),
          legend.key = element_rect(fill = "white", color = NA),
          plot.title = element_text(hjust = 0.5, face = "bold")) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE))
  
  ggsave(filename = file_output, plot = p, width = 190, height = 120, units = "mm")
  return(p)
}

#### GENERATE ALL PLOTS ####

## WET SEASON PLOTS ##
cat("\n=== Processing WET SEASON (November - April) ===\n")
wet_data <- process_season_data(df_all, "wet")

# Plot 1 - Wet
cat("Creating Plot 1 - Wet Season...\n")
create_plot_1(wet_data, "Wet Season", file_plot_precipmethod_1_wet)

# Plot 2 - Wet
cat("Creating Plot 2 - Wet Season...\n")
create_plot_2(wet_data, "Wet Season", file_plot_precipmethod_2_wet)

# Plot 2 Alt RCP 8.5 - Wet
cat("Creating Plot 2 Alternative RCP 8.5 - Wet Season...\n")
create_plot_2_alt(wet_data, "Wet Season", file_plot_precipmethod_2_alt_rcp85_wet)

# Plot 3 - Wet
cat("Creating Plot 3 - Wet Season...\n")
create_plot_3(wet_data, "Wet Season", file_plot_hydromodel_1_wet)

# Plot 4 - Wet
cat("Creating Plot 4 - Wet Season...\n")
create_plot_4(wet_data, "Wet Season", file_plot_hydromodel_2_wet)

## DRY SEASON PLOTS ##
cat("\n=== Processing DRY SEASON (May - October) ===\n")
dry_data <- process_season_data(df_all, "dry")

# Plot 1 - Dry
cat("Creating Plot 1 - Dry Season...\n")
create_plot_1(dry_data, "Dry Season", file_plot_precipmethod_1_dry)

# Plot 2 - Dry
cat("Creating Plot 2 - Dry Season...\n")
create_plot_2(dry_data, "Dry Season", file_plot_precipmethod_2_dry)

# Plot 2 Alt RCP 8.5 - Dry
cat("Creating Plot 2 Alternative RCP 8.5 - Dry Season...\n")
create_plot_2_alt(dry_data, "Dry Season", file_plot_precipmethod_2_alt_rcp85_dry)

# Plot 3 - Dry
cat("Creating Plot 3 - Dry Season...\n")
create_plot_3(dry_data, "Dry Season", file_plot_hydromodel_1_dry)

# Plot 4 - Dry
cat("Creating Plot 4 - Dry Season...\n")
create_plot_4(dry_data, "Dry Season", file_plot_hydromodel_2_dry)

cat("\n=== All 10 plots completed successfully! ===\n")