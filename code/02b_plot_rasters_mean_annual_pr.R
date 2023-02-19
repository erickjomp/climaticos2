library(tidyverse)
library(terra)
library(sf)
library(metR)

#### INPUT ####
file_rasters_mean_annual_pr <-
  "output/02_interpolation_pr/raster/ras_mean_annual_pr__inter.tiff"
file_wshed_shp <- "data/watersheds/shp/pativilca_watershed.shp"

#### OUTPUT ####
file_plot <- 
  "output/02_interpolation_pr/plots/rasters_mean_annual_pr.pdf"


#### reading INPUT ####
ras_inters <- rast(file_rasters_mean_annual_pr)
sf_wshed <- st_read(file_wshed_shp)

#### processing data ####
df_ras_methods <- as.data.frame(ras_inters, xy = TRUE)
df_ras_methods <- df_ras_methods %>%
  gather("method", "pry", -c(1:2)) %>%
  mutate(method = factor(method, levels = names(ras_inters)))



base_size_figs <- 8 # 7 : recommended for normal text by elsevier
breaks = c(-Inf, seq(200, 1000, 100),Inf)

(
  plot_ras_methods <-
    ggplot(data = df_ras_methods) +
    geom_contour_fill(aes(
      x = x,
      y = y,
      z = pry,
      fill = stat(level)
    ),binwidth = 150,
    breaks = breaks,
    color = NA) +
    guides(fill = guide_colorsteps(
      #show.limits = T,
      barheight = 1, # 1 # 17
      draw.llim = T,
      barwidth = 24, #12 # 1.2
      # title.position = "top"
    )) +
    scale_fill_viridis_d(option = "viridis", direction = -1) +
    # scale_fill_manual(values=wes_palette(n=6, name="Zissou1", type="continuous")) +
    # scale_fill_gradient2(high = "blue",mid = "yellow", low = "orange") +
    # scale_fill_brewer(palette="Dark2") +
    coord_equal() +
    facet_wrap( ~ method, nrow = 1) +
    geom_sf(data = sf_wshed, fill = NA, colour = "black", linewidth = 0.5) +
    labs(x = NULL, y = NULL, fill = "Mean Annual\nPrecipitation (mm)") +
    theme_minimal(base_size = base_size_figs) +
    theme(
      legend.position = "bottom",
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_line(colour = "transparent"),
      strip.text.x = element_text(size = base_size_figs * 1.2),
      strip.text.y = element_text(size = base_size_figs * 1.2)
    )
)

ggsave(plot_ras_methods,
       filename = file_plot,
       width = 190,height = 105,units = "mm")
