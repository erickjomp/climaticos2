library(tidyverse)
library(terra)
library(sf)
library(metR)
# remotes::install_github("https://github.com/erickjomp/Rthemes.git")
library(Rthemes)


#### INPUT ####
file_rasters_mean_annual_pr <-
  "output/02_interpolation_pr/raster/ras_mean_annual_pr__inter.tiff"
file_wshed_shp <- "data/watersheds/shp/pativilca_watershed.shp"
file_raster_mean_annual_pr_pisco <- 
  "output/00_processed_data/raster/ras_avg_prannual__pisco.tiff"
file_stas_shp <- "output/00_processed_data/shp/sf_stas_pr.shp"

#### OUTPUT ####
file_plot <- 
  "output/02_interpolation_pr/plots/fig_rasters_mean_annual_pr.pdf"


#### reading INPUT ####
ras_inters <- rast(file_rasters_mean_annual_pr)
sf_wshed <- st_read(file_wshed_shp)
ras_pisco <- rast(file_raster_mean_annual_pr_pisco)
sf_stas <- st_read(file_stas_shp)

#### calculating mean annual precip ####
areal_pr_value_inters <- ras_inters %>% terra::extract(sf_wshed,fun = "mean",) %>% .[,-1] %>% 
  unlist(., use.names = TRUE)
areal_pr_value_pisco <- ras_pisco%>% terra::extract(sf_wshed,fun = "mean") %>% .[,-1] %>% 
  setNames(c("PISCO"))
areal_pr_values <- c(areal_pr_value_inters,areal_pr_value_pisco )

#### processing data ####
labels_plot = c(names(ras_inters),"PISCO")
df_ras_inters <- as.data.frame(ras_inters, xy = TRUE)
df_ras_pisco <- as.data.frame(ras_pisco, xy = TRUE)
df_ras_methods <- df_ras_inters %>% 
  gather("method", "pry", -c(1:2)) %>%
  rbind(.,mutate(rename(df_ras_pisco,pry = mean),method = "PISCO",.after = 2)) %>% 
  mutate(method = factor(method, levels = labels_plot))




base_size_figs <- 8 # 7 : recommended for normal text by elsevier
breaks = c(-Inf, seq(200, 1000, 100),Inf)


# data frame for annuation data
xmin = ext(sf_wshed)$xmin
xmax = ext(sf_wshed)$xmax
ymin = ext(sf_wshed)$ymin
ymax = ext(sf_wshed)$ymax
df_ann <- data.frame(
  method = as.factor(labels_plot),
  xmin = xmin + 0.6*(xmax - xmin), xmax = 1*xmax,
  ymin = ymin + 0.925*(ymax - ymin), ymax = 1*ymax,
  label = paste(round(areal_pr_values,0),"mm")
)

# actual plot
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
    # scale_fill_viridis_d(option = "Spectral", direction = -1) +
    # scale_fill_manual(values=wes_palette(n=6, name="Zissou1", type="continuous")) +
    # scale_fill_gradient2(high = "blue",mid = "yellow", low = "orange") +
    scale_fill_brewer(palette="Spectral") +
    facet_wrap( ~ method, nrow = 1) +
    # coord_cartesian() + 
    
    geom_sf(data = sf_wshed, fill = NA, colour = "black", linewidth = 0.5) +
    geom_sf(data = sf_stas, color = "black") +
    labs(x = NULL, y = NULL, fill = "Mean Annual\nPrecipitation (mm)") +
    
    coord_sf(xlim = c(xmin,xmax), ylim = c(ymin,ymax)) +
    geom_rect(data = df_ann,
              aes(xmin = xmin, xmax = xmax,
                  ymin = ymin, ymax = ymax),
              # fill = "lightblue", color = "black",
              fill = "white", color = "black",
              inherit.aes = FALSE) +
    geom_text(data = df_ann,
              aes(x = (xmin+xmax)/2, y = (ymin+ymax)/2, label = label),
              inherit.aes = FALSE, size =base_size_figs*1.0 / .pt) +
    # theme_minimal(base_size = base_size_figs) +
    my_ggtheme(base_size = base_size_figs)  +
    theme(
      strip.background = element_blank(),
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
       width = 190,height = 98,units = "mm")
