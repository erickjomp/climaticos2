## PRECIPITATION INTERPOLATION
# nethods iwd, ok, ied, ked

library(tidyverse)
library(terra)
library(sf)
library(elevatr)
library(sp)
library(gstat)
library(automap)
library(tmap)
Sys.setenv(TZ = 'GMT')

#### INPUT ####
file_pr_m <- "output/01_filling_pr/df_pr_m__filled.rds"
file_stas_pr_shp <- "output/00_processed_data/shp/sf_stas_pr.shp"
file_wshed_shp <- "data/watersheds/shp/pativilca_watershed.shp"

#### OUTPUT ####
file_df_pr_m__inter_rds <- "output/02_interpolation_pr/df_pr_m__inter.rds"
file_df_pr_m__inter_csv <- "output/02_interpolation_pr/df_pr_m__inter.csv"

file_ras_mean_annual_pr__inter <- 
  "output/02_interpolation_pr/raster/ras_mean_annual_pr__inter.tiff"

file_dem_geog_gen = "output/02_interpolation_pr/raster/topography_dem/ras_dem_geog_%s.tiff"
file_dem_utm_gen = "output/02_interpolation_pr/raster/topography_dem/ras_dem_utm_%s.tiff"


#### reading input ####
df_pr_m <- readRDS(file_pr_m)
sf_stas_pr <- st_read(file_stas_pr_shp)
sf_wshed <- st_read(file_wshed_shp)

#### auxiliar objects ####
sp_stas_pr__inter <- sf_stas_pr %>% as_Spatial()
ras_dem0 <- get_elev_raster(sf_stas_pr, #z = 6, 
                            clip = "bbox",src = "gl3")
names(ras_dem0) <- "elev"
ras_dem <- terra::aggregate(ras_dem0, fact = 24)  # 0.02 deg ~ 2.2 km
ras_dem0 <- rast(ras_dem0)
ras_dem <- rast(ras_dem)

# sp_stas_pr_inter <- as_Spatial(sf_stas_pr__inter)
# sp_dem <- as(ras_dem, "SpatialGridDataFrame")
sp_dem <- as(raster::raster(ras_dem), "SpatialGridDataFrame")
# using sp cause is still faster than sf and stars
# stars_dem <- stars::st_as_stars(ras_dem0)

plot(ras_dem)
plot(sf_wshed$geometry, add = T)

elev_avg <- terra::extract(ras_dem, sf_wshed,fun = mean)#[,-1]
message("The mean elevation of the watershed is:   " , 
        round(elev_avg, 1), "    masl")

#### IDW INTERPOLATION ####
ras_idw <-
  lapply(1:nrow(df_pr_m), function(i_m) {
    message("Processing ", i_m, " / ", nrow(df_pr_m), "\r", appendLF = F)
    values_pr <- df_pr_m[i_m,-1] %>% as.numeric()
    sp_stas_pr__inter$pr <- values_pr
    sp_stas_pr__inter <- sp_stas_pr__inter[is.finite(values_pr), ]
    
    sp_inter <- idw(pr ~ 1, sp_stas_pr__inter, sp_dem,
                    debug.level = 0)
    ras_inter <- rast(sp_inter[1])
    return(ras_inter)
    ## ignore it  (just to compare sf & stars  vs  sp)
    # microbenchmark::microbenchmark(
    #   ave1 <- idw(pr~1, sp_stas_pr__inter,sp_dem,
    #               debug.level = 0) ,
    #   ave2 <- idw(pr~1, sf_stas_pr__inter,stars_dem,
    #               debug.level = 0),times = 10)
    
  }) %>% do.call(c, .)

values_pr_idw <-
  terra::extract(ras_idw, sf_wshed, "mean")[, -1] %>% as.numeric()
df_pr_idw <- tibble(dates = df_pr_m$dates,
                    IDW = values_pr_idw)
plot(df_pr_idw, type = "b")
mean_annual_pr_idw <- 12 * mean(df_pr_idw[[2]])
message("Mean Annual pr IDW is: ", round(mean_annual_pr_idw, 1))


#### OK INTERPOLATION ####
sp_dem_utm <- 
  sp_dem %>% spTransform(CRS("+init=epsg:32718")) # laso for KED
sp_stas_pr__inter_utm <- 
  sp_stas_pr__inter %>% spTransform(CRS("+init=epsg:32718")) # laso for KED

ras_ok <-
  lapply(1:nrow(df_pr_m), function(i_m) {
    # print(i_m)
    message("Processing ", i_m, " / ", nrow(df_pr_m), "\r", appendLF = F)
    values_pr <- df_pr_m[i_m,-1] %>% as.numeric()
    sp_stas_pr__inter_utm$pr <- values_pr
    if (any(values_pr != 0)) {
      sp_stas_pr__inter_utm <- 
        sp_stas_pr__inter_utm[is.finite(values_pr), ]
      
      autokrige_out <- autoKrige(
        formula = pr~1,
        input_data = sp_stas_pr__inter_utm,
        new_data = sp_dem_utm,
        debug.level = 0,
        miscFitOptions = list(merge.small.bins = FALSE))
      
      ras_inter <- ras_dem
      values(ras_inter) <- autokrige_out$krige_output$var1.pred
      values(ras_inter)[values(ras_inter) < 0] <- 0
    } else {
      ras_inter <- rast(sp_dem)
      values(ras_inter) <- 0
    }
    names(ras_inter) <- "pr"
    return(ras_inter)
    
  }) %>% do.call(c, .)

values_pr_ok <-
  terra::extract(ras_ok, sf_wshed, "mean")[, -1] %>% as.numeric()
df_pr_ok <- tibble(dates = df_pr_m$dates,
                   OK = values_pr_ok)
plot(df_pr_ok, type = "b")
mean_annual_pr_ok <- 12 * mean(df_pr_ok[[2]])
message("Mean Annual pr OK is: ", round(mean_annual_pr_ok, 1))


#### plot pr vs elevation (exploratory) ####
sf_stas_pr %>% ggplot(aes(elev, pry)) +
  geom_point() +
  theme_linedraw() +
  geom_smooth(method = "lm")


#### IED INTERPOLATION ####
ras_ied <-
  lapply(1:nrow(df_pr_m), function(i_m) {
    message("Processing ", i_m, " / ", nrow(df_pr_m), "\r", appendLF = F)
    values_pr <- df_pr_m[i_m,-1] %>% as.numeric()
    sp_stas_pr__inter$pr <- values_pr
    # sp_stas_pr__inter <- sp_stas_pr__inter[is.finite(values_pr),]
    
    lm_model <- lm(pr ~ elev, sp_stas_pr__inter)
    sp_lm <- sp_dem
    names(sp_lm) <- "pr"
    sp_lm$pr <- predict(lm_model, sp_dem)
    sp_stas_pr__inter$res <- residuals(lm_model)
    sp_res <- idw(res ~ 1, sp_stas_pr__inter, sp_dem,
                  debug.level = 0)
    ras_inter <- ras_dem
    names(ras_inter) <- "pr"
    values(ras_inter) <- sp_lm$pr + sp_res$var1.pred
    values(ras_inter)[values(ras_inter) < 0] <- 0
    return(ras_inter)
    
  }) %>% do.call(c, .)

values_pr_ied <-
  terra::extract(ras_ied, sf_wshed, "mean")[, -1] %>% as.numeric()
df_pr_ied <- tibble(dates = df_pr_m$dates,
                    IED = values_pr_ied)
plot(df_pr_ied, type = "b")
mean_annual_pr_ied <- 12 * mean(df_pr_ied[[2]])
message("Mean Annual pr IED is: ", round(mean_annual_pr_ied, 1))

#### KED INTERPOLATION ####
ras_ked <-
  lapply(1:nrow(df_pr_m), function(i_m) {
    # print(i_m)
    message("Processing ", i_m, " / ", nrow(df_pr_m), "\r", appendLF = F)
    values_pr <- df_pr_m[i_m,-1] %>% as.numeric()
    sp_stas_pr__inter_utm$pr <- values_pr
    if (any(values_pr != 0)) {
      sp_stas_pr__inter_utm <- 
        sp_stas_pr__inter_utm[is.finite(values_pr), ]
      
      autokrige_out <- autoKrige(
        formula = pr~elev,
        input_data = sp_stas_pr__inter_utm,
        new_data = sp_dem_utm,
        debug.level = 0,
        miscFitOptions = list(merge.small.bins = FALSE))
      
      ras_inter <- ras_dem
      values(ras_inter) <- autokrige_out$krige_output$var1.pred
      values(ras_inter)[values(ras_inter) < 0] <- 0
    } else {
      ras_inter <- rast(sp_dem)
      values(ras_inter) <- 0
    }
    names(ras_inter) <- "pr"
    
    return(ras_inter)
    
  }) %>% do.call(c, .)

values_pr_ked <-
  terra::extract(ras_ked, sf_wshed, "mean")[, -1] %>% as.numeric()
df_pr_ked <- tibble(dates = df_pr_m$dates,
                    KED = values_pr_ked)
plot(df_pr_ked, type = "b")
mean_annual_pr_ked <- 12 * mean(df_pr_ked[[2]])
message("Mean Annual pr KED is: ", round(mean_annual_pr_ked, 1))


#### rasters  mean annual agraggation ####

ras_idw_mean_annual <- mean(ras_idw) * 12
ras_ok_mean_annual <- mean(ras_ok) * 12
ras_ied_mean_annual <- mean(ras_ied) * 12
ras_ked_mean_annual <- mean(ras_ked) * 12

ras_inters_mean_annual <-
  c(ras_idw_mean_annual,
    ras_ok_mean_annual,
    ras_ied_mean_annual,
    ras_ked_mean_annual)
names(ras_inters_mean_annual) <- c("IDW", "OK", "IED", "KED")
plot(ras_inters_mean_annual)
plot(sf_wshed$geometry, add = TRUE)

tm_shape(ras_inters_mean_annual) +
  tm_raster(palette = "-viridis", n = 10) +
  tm_shape(sf_wshed) + tm_borders(col = "white") +
  tm_shape(sf_stas_pr) + tm_dots(shape = 24,
                                 size = 0.2,
                                 col = "red") +
  tm_facets(nrow = 2)

## joining series
df_pr_m__inter <- list(df_pr_idw,df_pr_ok,
                      df_pr_ied, df_pr_ked) %>% Reduce(full_join, .)

#### writing output ####
saveRDS(df_pr_m__inter, file_df_pr_m__inter_rds)
write.csv(df_pr_m__inter, file_df_pr_m__inter_csv)

writeRaster(ras_inters_mean_annual, 
            file_ras_mean_annual_pr__inter, 
            overwrite = TRUE)

#### writing dem ####
resol_dem <-  terra::res(ras_dem)[1] %>% round(6) %>% paste0(.,"deg")
ras_dem %>% writeRaster(sprintf(file_dem_geog_gen,resol_dem))
ras_dem %>% terra::project("EPSG:32718") %>% writeRaster(sprintf(file_dem_utm_gen,resol_dem))

resol_dem0 <-  terra::res(ras_dem0)[1] %>% round(6) %>% paste0(.,"deg")
ras_dem0 %>% writeRaster(sprintf(file_dem_geog_gen,resol_dem0))
ras_dem0 %>% terra::project("EPSG:32718") %>% writeRaster(sprintf(file_dem_utm_gen,resol_dem0))
