
# Figures

library(viridis)
library(ggplot2)
library(rworldmap)
library(maps)
library(ggmap)
library(ggthemes)
library(sf)
library(rgdal)
library(dplyr)
library(rgl)
library(raster)
library(rgeos)
library(ggsn)
library(marmap)
library(SDMtune)
library(terra)

setwd('SET WORKING DIRECTORY')

# Coastline (spatial polygons) - taken from GSHHG coast database as of June 15, 2017
coastline <- 'DOWNLOAD Coast shape'
coast0 <- readOGR(dsn = coastline, layer = 'GSHHS_f_L1_SouthAmerica')
coast <- readOGR(dsn = coastline, layer = 'GSHHS_f_L1_World')
coast <- st_read(dsn = coastline, layer = 'GSHHS_f_L1_World')

# Political borders
Borders <- 'DOWNLOAD Political borders'
Border <- readOGR(dsn = Borders, layer = 'WDBII_border_f_L1')
Border <- st_read(dsn = Borders, layer = 'WDBII_border_f_L1')

# Lat-lon projection
crs <- CRS('+proj=longlat +datum=WGS84')

# Functions to compute 5% and 10% Minimum Training Presence (taken from https://babichmorrowc.github.io/post/2019-04-12-sdm-threshold/)
sdm_threshold.5 <- function(sdm, occs, type = 'mtp', binary = FALSE){
  occPredVals <- raster::extract(sdm, occs)
  if(type == 'mtp'){
    thresh <- min(na.omit(occPredVals))
  } else if(type == 'p05'){
    if(length(occPredVals) < 10){
      p05 <- floor(length(occPredVals) * 0.8)
    } else {
      p05 <- ceiling(length(occPredVals) * 0.8)
    }
    thresh <- rev(sort(occPredVals))[p05]
  }
  sdm_thresh <- sdm
  sdm_thresh[sdm_thresh < thresh] <- NA
  if(binary){
    sdm_thresh[sdm_thresh >= thresh] <- 1
  }
  return(sdm_thresh)
} # MTP5
sdm_threshold.10 <- function(sdm, occs, type = 'mtp', binary = FALSE){
  occPredVals <- raster::extract(sdm, occs)
  if(type == 'mtp'){
    thresh <- min(na.omit(occPredVals))
  } else if(type == 'p10'){
    if(length(occPredVals) < 10){
      p10 <- floor(length(occPredVals) * 0.7)
    } else {
      p10 <- ceiling(length(occPredVals) * 0.7)
    }
    thresh <- rev(sort(occPredVals))[p10]
  }
  sdm_thresh <- sdm
  sdm_thresh[sdm_thresh < thresh] <- NA
  if(binary){
    sdm_thresh[sdm_thresh >= thresh] <- 1
  }
  return(sdm_thresh)
} # MTP10

# Functions for response curves
.get_presence <- function(swd) {
  return(swd@data[swd@pa == 1, , drop = FALSE])
}
.get_absence <- function(swd) {
  return(swd@data[swd@pa == 0, , drop = FALSE])
}


#------------------------------------ Figures 1, 2, S1.2 & S1.3  ---------------------------------------

# Overlap in the annual and seasonal predictions of G. galeus and N. cepedianus

# Annual and seasonal binary habitat suitability

# G. galeus
{
  final_mod <- raster('G_galeus_annual.tif')
  bg_binary <- final_mod
  bg_binary[bg_binary < 0.1] <- 1
  bg_binary[bg_binary] <- 2
  
  # Summer
  final_mod <- raster('G_galeus_summer.tif')
  occ_cal <- read.csv('G_galeus_summer.csv')
  # Threshold (%5 minimum training presence)
  mtp5 <- sdm_threshold.5(final_mod, occ_cal[, c('longitude', 'latitude')], 'p05', binary = F)
  bin_5_summer_cazon <- final_mod
  bin_5_summer_cazon[bin_5_summer_cazon < minValue(mtp5)] <- NA
  bin_5_summer_cazon[bin_5_summer_cazon >= minValue(mtp5)] <- 1
  # Threshold (%10 minimum training presence)
  mtp10 <- sdm_threshold.10(final_mod, occ_cal[, c('longitude', 'latitude')], 'p10', binary = F)
  bin_10_summer_cazon <- final_mod
  bin_10_summer_cazon[bin_10_summer_cazon < minValue(mtp10)] <- NA
  bin_10_summer_cazon[bin_10_summer_cazon >= minValue(mtp10)] <- 1
  
  # Autumn
  final_mod <- raster('G_galeus_autumn.tif') 
  occ_cal <- read.csv('G_galeus_autumn.csv')
  # Threshold (%5 minimum training presence)
  mtp5 <- sdm_threshold.5(final_mod, occ_cal[, c('longitude', 'latitude')], 'p05', binary = F)
  bin_5_autumn_cazon <- final_mod
  bin_5_autumn_cazon[bin_5_autumn_cazon < minValue(mtp5)] <- NA
  bin_5_autumn_cazon[bin_5_autumn_cazon >= minValue(mtp5)] <- 1
  # Threshold (%10 minimum training presence)
  mtp10 <- sdm_threshold.10(final_mod, occ_cal[, c('longitude', 'latitude')], 'p10', binary = F)
  bin_10_autumn_cazon <- final_mod
  bin_10_autumn_cazon[bin_10_autumn_cazon < minValue(mtp10)] <- NA
  bin_10_autumn_cazon[bin_10_autumn_cazon >= minValue(mtp10)] <- 1
  
  # Winter
  final_mod <- raster('G_galeus_winter.tif') 
  occ_cal <- read.csv('G_galeus_winter.csv')
  # Threshold (%5 minimum training presence)
  mtp5 <- sdm_threshold.5(final_mod, occ_cal[, c('longitude', 'latitude')], 'p05', binary = F)
  bin_5_winter_cazon <- final_mod
  bin_5_winter_cazon[bin_5_winter_cazon < minValue(mtp5)] <- NA
  bin_5_winter_cazon[bin_5_winter_cazon >= minValue(mtp5)] <- 1
  # Threshold (%10 minimum training presence)
  mtp10 <- sdm_threshold.10(final_mod, occ_cal[, c('longitude', 'latitude')], 'p10', binary = F)
  bin_10_winter_cazon <- final_mod
  bin_10_winter_cazon[bin_10_winter_cazon < minValue(mtp10)] <- NA
  bin_10_winter_cazon[bin_10_winter_cazon >= minValue(mtp10)] <- 1
  
  # Spring
  final_mod <- raster('G_galeus_spring.tif')
  occ_cal <- read.csv('G_galeus_spring.csv')
  # Threshold (%5 minimum training presence)
  mtp5 <- sdm_threshold.5(final_mod, occ_cal[, c('longitude', 'latitude')], 'p05', binary = F)
  bin_5_spring_cazon <- final_mod
  bin_5_spring_cazon[bin_5_spring_cazon < minValue(mtp5)] <- NA
  bin_5_spring_cazon[bin_5_spring_cazon >= minValue(mtp5)] <- 1
  # Threshold (%10 minimum training presence)
  mtp10 <- sdm_threshold.10(final_mod, occ_cal[, c('longitude', 'latitude')], 'p10', binary = F)
  bin_10_spring_cazon <- final_mod
  bin_10_spring_cazon[bin_10_spring_cazon < minValue(mtp10)] <- NA
  bin_10_spring_cazon[bin_10_spring_cazon >= minValue(mtp10)] <- 1
  
  # Mosaic 10% MTP
  mod_mosaic_10_cazon <- mosaic(bg_binary, bin_10_summer_cazon, bin_10_autumn_cazon, bin_10_winter_cazon, bin_10_spring_cazon, fun = sum)
  mod_mosaic_10_cazon[mod_mosaic_10_cazon < 2] <- NA
  
  # Mosaic 5% MTP
  mod_mosaic_5_cazon <- mosaic(bg_binary, bin_5_summer_cazon, bin_5_autumn_cazon, bin_5_winter_cazon, bin_5_spring_cazon, fun = sum)
  mod_mosaic_5_cazon[mod_mosaic_5_cazon < 2] <- NA
}

# N. cepedianus
{
  final_mod <- raster('N_cepedianus_annual.tif')
  bg_binary <- final_mod
  bg_binary[bg_binary < 0.1] <- 1
  bg_binary[bg_binary] <- 2
  
  # Summer
  final_mod <- raster('N_cepedianus_summer.tif')
  occ_cal <- read.csv('N_cepedianus_summer.csv')
  # Threshold (%5 minimum training presence)
  mtp5 <- sdm_threshold.5(final_mod, occ_cal[, c('longitude', 'latitude')], 'p05', binary = F)
  bin_5_summer_gatop <- final_mod
  bin_5_summer_gatop[bin_5_summer_gatop < minValue(mtp5)] <- NA
  bin_5_summer_gatop[bin_5_summer_gatop >= minValue(mtp5)] <- 1
  # Threshold (%10 minimum training presence)
  mtp10 <- sdm_threshold.10(final_mod, occ_cal[, c('longitude', 'latitude')], 'p10', binary = F)
  bin_10_summer_gatop <- final_mod
  bin_10_summer_gatop[bin_10_summer_gatop < minValue(mtp10)] <- NA
  bin_10_summer_gatop[bin_10_summer_gatop >= minValue(mtp10)] <- 1
  
  # Autumn
  final_mod <- raster('N_cepedianus_autumn.tif') 
  occ_cal <- read.csv('N_cepedianus_autumn.csv')
  # Threshold (%5 minimum training presence)
  mtp5 <- sdm_threshold.5(final_mod, occ_cal[, c('longitude', 'latitude')], 'p05', binary = F)
  bin_5_autumn_gatop <- final_mod
  bin_5_autumn_gatop[bin_5_autumn_gatop < minValue(mtp5)] <- NA
  bin_5_autumn_gatop[bin_5_autumn_gatop >= minValue(mtp5)] <- 1
  # Threshold (%10 minimum training presence)
  mtp10 <- sdm_threshold.10(final_mod, occ_cal[, c('longitude', 'latitude')], 'p10', binary = F)
  bin_10_autumn_gatop <- final_mod
  bin_10_autumn_gatop[bin_10_autumn_gatop < minValue(mtp10)] <- NA
  bin_10_autumn_gatop[bin_10_autumn_gatop >= minValue(mtp10)] <- 1
  
  # 
  final_mod <- raster('N_cepedianus_winter.tif') 
  occ_cal <- read.csv('N_cepedianus_winter.csv')
  # Threshold (%5 minimum training presence)
  mtp5 <- sdm_threshold.5(final_mod, occ_cal[, c('longitude', 'latitude')], 'p05', binary = F)
  bin_5_winter_gatop <- final_mod
  bin_5_winter_gatop[bin_5_winter_gatop < minValue(mtp5)] <- NA
  bin_5_winter_gatop[bin_5_winter_gatop >= minValue(mtp5)] <- 1
  # Threshold (%10 minimum training presence)
  mtp10 <- sdm_threshold.10(final_mod, occ_cal[, c('longitude', 'latitude')], 'p10', binary = F)
  bin_10_winter_gatop <- final_mod
  bin_10_winter_gatop[bin_10_winter_gatop < minValue(mtp10)] <- NA
  bin_10_winter_gatop[bin_10_winter_gatop >= minValue(mtp10)] <- 1
  
  # Spring
  final_mod <- raster('N_cepedianus_spring.tif')
  occ_cal <- read.csv('N_cepedianus_spring.csv')
  # Threshold (%5 minimum training presence)
  mtp5 <- sdm_threshold.5(final_mod, occ_cal[, c('longitude', 'latitude')], 'p05', binary = F)
  bin_5_spring_gatop <- final_mod
  bin_5_spring_gatop[bin_5_spring_gatop < minValue(mtp5)] <- NA
  bin_5_spring_gatop[bin_5_spring_gatop >= minValue(mtp5)] <- 1
  # Threshold (%10 minimum training presence)
  mtp10 <- sdm_threshold.10(final_mod, occ_cal[, c('longitude', 'latitude')], 'p10', binary = F)
  bin_10_spring_gatop <- final_mod
  bin_10_spring_gatop[bin_10_spring_gatop < minValue(mtp10)] <- NA
  bin_10_spring_gatop[bin_10_spring_gatop >= minValue(mtp10)] <- 1
  
  # Mosaic 10% MTP
  mod_mosaic_10_gatop <- mosaic(bg_binary, bin_10_summer_gatop, bin_10_autumn_gatop, bin_10_winter_gatop, bin_10_spring_gatop, fun = sum)
  mod_mosaic_10_gatop[mod_mosaic_10_gatop < 2] <- NA
  
  # Mosaic 5% MTP
  mod_mosaic_5_gatop <- mosaic(bg_binary, bin_5_summer_gatop, bin_5_autumn_gatop, bin_5_winter_gatop, bin_5_spring_gatop, fun = sum)
  mod_mosaic_5_gatop[mod_mosaic_5_gatop < 2] <- NA
}

# Mosaic with 10% MTP
mod_mosaic_10_cazon[mod_mosaic_10_cazon == 2] <- 0
mod_mosaic_10_cazon[mod_mosaic_10_cazon == 3] <- 1.1
mod_mosaic_10_cazon[mod_mosaic_10_cazon == 4] <- 2.2
mod_mosaic_10_cazon[mod_mosaic_10_cazon == 5] <- 3.3
mod_mosaic_10_cazon[mod_mosaic_10_cazon == 6] <- 4.4

mod_mosaic_10_gatop[mod_mosaic_10_gatop == 2] <- 0
mod_mosaic_10_gatop[mod_mosaic_10_gatop == 3] <- 1.4
mod_mosaic_10_gatop[mod_mosaic_10_gatop == 4] <- 2.3
mod_mosaic_10_gatop[mod_mosaic_10_gatop == 5] <- 3.2
mod_mosaic_10_gatop[mod_mosaic_10_gatop == 6] <- 4.1

# General overlap
mod_mosaic_10 <- mosaic(mod_mosaic_10_cazon, mod_mosaic_10_gatop, fun = sum)
mod_mosaic_10[mod_mosaic_10 == 4.4] <- 0
mod_mosaic_10[mod_mosaic_10 == 3.3] <- 0
mod_mosaic_10[mod_mosaic_10 == 2.2] <- 0
mod_mosaic_10[mod_mosaic_10 == 1.1] <- 0
mod_mosaic_10[mod_mosaic_10 == 1.4] <- 0
mod_mosaic_10[mod_mosaic_10 == 2.3] <- 0
mod_mosaic_10[mod_mosaic_10 == 3.2] <- 0
mod_mosaic_10[mod_mosaic_10 == 4.1] <- 0
mod_mosaic_10[mod_mosaic_10 == 2.5] <- 2
mod_mosaic_10[mod_mosaic_10 > 3 & mod_mosaic_10 < 4] <- 3
mod_mosaic_10[mod_mosaic_10 > 4 & mod_mosaic_10 < 5] <- 4
mod_mosaic_10[mod_mosaic_10 > 5 & mod_mosaic_10 < 6] <- 5
mod_mosaic_10[mod_mosaic_10 > 6 & mod_mosaic_10 < 7] <- 6
mod_mosaic_10[mod_mosaic_10 > 7 & mod_mosaic_10 < 8] <- 7
mod_mosaic_10[mod_mosaic_10 == 8.5] <- 8
round(table(getValues(mod_mosaic_10))[-1] * 100 / sum(table(getValues(mod_mosaic_10))[-1]), 1)

# Overlap favouring G. galeus
mod_mosaic_10c <- mosaic(mod_mosaic_10_cazon, mod_mosaic_10_gatop, fun = sum)
mod_mosaic_10c[mod_mosaic_10c == 1.1] <- 0
mod_mosaic_10c[mod_mosaic_10c == 1.4] <- 0
mod_mosaic_10c[mod_mosaic_10c == 2.2] <- 0
mod_mosaic_10c[mod_mosaic_10c == 2.3] <- 0
mod_mosaic_10c[mod_mosaic_10c == 3.2] <- 0
mod_mosaic_10c[mod_mosaic_10c == 3.3] <- 0
mod_mosaic_10c[mod_mosaic_10c == 4.1] <- 0
mod_mosaic_10c[mod_mosaic_10c == 4.4] <- 0
mod_mosaic_10c[mod_mosaic_10c == 2.5] <- 1
mod_mosaic_10c[mod_mosaic_10c == 3.4] <- 1
mod_mosaic_10c[mod_mosaic_10c > 4.2 & mod_mosaic_10c < 4.4] <- 1
mod_mosaic_10c[mod_mosaic_10c > 5.1 & mod_mosaic_10c < 5.3] <- 1
mod_mosaic_10c[mod_mosaic_10c == 3.6] <- 1
mod_mosaic_10c[mod_mosaic_10c == 4.5] <- 1
mod_mosaic_10c[mod_mosaic_10c == 5.4] <- 1
mod_mosaic_10c[mod_mosaic_10c == 6.3] <- 1
mod_mosaic_10c[mod_mosaic_10c > 4.6 & mod_mosaic_10c < 4.8] <- 1
mod_mosaic_10c[mod_mosaic_10c == 5.6] <- 1
mod_mosaic_10c[mod_mosaic_10c == 6.5] <- 2
mod_mosaic_10c[mod_mosaic_10c > 7.3 & mod_mosaic_10c < 7.5] <- 1
mod_mosaic_10c[mod_mosaic_10c > 5.7 & mod_mosaic_10c < 5.9] <- 1
mod_mosaic_10c[mod_mosaic_10c == 6.7] <- 1
mod_mosaic_10c[mod_mosaic_10c > 7.5 & mod_mosaic_10c < 7.7] <- 3
mod_mosaic_10c[mod_mosaic_10c == 8.5] <- 4
round(table(getValues(mod_mosaic_10c))[-1] * 100 / sum(table(getValues(mod_mosaic_10c))[-1]), 1)

# Overlap favouring N. cepedianus
mod_mosaic_10g <- mosaic(mod_mosaic_10_cazon, mod_mosaic_10_gatop, fun = sum)
mod_mosaic_10g[mod_mosaic_10g == 1.1] <- 0
mod_mosaic_10g[mod_mosaic_10g == 1.4] <- 0
mod_mosaic_10g[mod_mosaic_10g == 2.2] <- 0
mod_mosaic_10g[mod_mosaic_10g == 2.3] <- 0
mod_mosaic_10g[mod_mosaic_10g == 3.2] <- 0
mod_mosaic_10g[mod_mosaic_10g == 3.3] <- 0
mod_mosaic_10g[mod_mosaic_10g == 4.1] <- 0
mod_mosaic_10g[mod_mosaic_10g == 4.4] <- 0
mod_mosaic_10g[mod_mosaic_10g == 2.5] <- 1
mod_mosaic_10g[mod_mosaic_10g == 3.4] <- 1
mod_mosaic_10g[mod_mosaic_10g > 4.2 & mod_mosaic_10g < 4.4] <- 1
mod_mosaic_10g[mod_mosaic_10g > 5.1 & mod_mosaic_10g < 5.3] <- 1
mod_mosaic_10g[mod_mosaic_10g == 3.6] <- 1
mod_mosaic_10g[mod_mosaic_10g == 4.5] <- 1
mod_mosaic_10g[mod_mosaic_10g == 5.4] <- 1
mod_mosaic_10g[mod_mosaic_10g == 6.3] <- 1
mod_mosaic_10g[mod_mosaic_10g > 4.6 & mod_mosaic_10g < 4.8] <- 1
mod_mosaic_10g[mod_mosaic_10g == 5.6] <- 1
mod_mosaic_10g[mod_mosaic_10g == 6.5] <- 2
mod_mosaic_10g[mod_mosaic_10g > 7.3 & mod_mosaic_10g < 7.5] <- 3
mod_mosaic_10g[mod_mosaic_10g > 5.7 & mod_mosaic_10g < 5.9] <- 1
mod_mosaic_10g[mod_mosaic_10g == 6.7] <- 1
mod_mosaic_10g[mod_mosaic_10g > 7.5 & mod_mosaic_10g < 7.7] <- 1
mod_mosaic_10g[mod_mosaic_10g == 8.5] <- 4
round(table(getValues(mod_mosaic_10g))[-1] * 100 / sum(table(getValues(mod_mosaic_10g))[-1]), 1)

# Plot
Col <- c('#000004FF', '#280B54FF', '#65156EFF', '#9F2A63FF', '#D44842FF', '#F57D15FF', '#FAC127FF', '#FCFFA4FF')

df_10 <- data.frame(coordinates(mod_mosaic_10), as.data.frame(mod_mosaic_10))
ggplot() +
  geom_tile(data = df_10, aes(x = x, y = y, fill = as.factor(layer))) + 
  scale_fill_manual(values = Col, na.value = 'grey95') + 
  geom_polygon(data = df_rdp, aes(x = x, y = y), color = 'grey95', fill = 'grey95', linewidth = 0.75) +
  geom_polygon(data = df_lagoa, aes(x = x, y = y), color = 'grey95', fill = 'grey95',linewidth = 0.1) +
  geom_polygon(data = coast0, aes(x = long, y = lat, group = group), color = 'grey50', fill = 'white', linewidth = 0.15) +
  geom_rect(data = df_mar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'grey95') +
  geom_rect(data = df_tierra, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'white') +
  scale_y_continuous(name = NULL, breaks = c(-55, -46, -37, -28), labels = c('55º', '46º', '37º', '28º')) + 
  scale_x_continuous(name = NULL, breaks = c(-66, -58, -50), labels = c('66º', '58º', '50º')) +
  coord_equal(xlim = c(-70, -47), ylim = c(-57, -26), expand = 0) + 
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_blank(), legend.position = 'none',
        panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.35))
ggsave('Figure 1a.tiff', dpi = 900, width = 10.2, height = 10.6, units = 'cm', device = grDevices::tiff)

Col <- c('#000004FF', '#56106EFF', '#BB3754FF', '#F98C0AFF', '#FCFFA4FF')
df_10_cazon <- data.frame(coordinates(mod_mosaic_10c), as.data.frame(mod_mosaic_10c))
ggplot() +
  geom_tile(data = df_10_cazon, aes(x = x, y = y, fill = as.factor(layer))) + 
  scale_fill_manual(values = Col, na.value = 'grey95') + 
  geom_polygon(data = df_rdp, aes(x = x, y = y), color = 'grey95', fill = 'grey95', linewidth = 0.75) +
  geom_polygon(data = df_lagoa, aes(x = x, y = y), color = 'grey95', fill = 'grey95',linewidth = 0.1) +
  geom_polygon(data = coast0, aes(x = long, y = lat, group = group), color = 'grey50', fill = 'white', linewidth = 0.15) +
  geom_rect(data = df_mar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'grey95') +
  geom_rect(data = df_tierra, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'white') +
  scale_y_continuous(name = NULL, breaks = c(-55, -46, -37, -28), labels = c('55º', '46º', '37º', '28º')) + 
  scale_x_continuous(name = NULL, breaks = c(-66, -58, -50), labels = c('66º', '58º', '50º')) +
  coord_equal(xlim = c(-70, -47), ylim = c(-57, -26), expand = 0) + 
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_blank(), legend.position = 'none',
        panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.35))
ggsave('Figure 1b.tiff', dpi = 900, width = 10.2, height = 10.6, units = 'cm', device = grDevices::tiff)

df_10_gatop <- data.frame(coordinates(mod_mosaic_10g), as.data.frame(mod_mosaic_10g))
ggplot() +
  geom_tile(data = df_10_gatop, aes(x = x, y = y, fill = as.factor(layer))) + 
  scale_fill_manual(values = Col, na.value = 'grey95') + 
  geom_polygon(data = df_rdp, aes(x = x, y = y), color = 'grey95', fill = 'grey95', linewidth = 0.75) +
  geom_polygon(data = df_lagoa, aes(x = x, y = y), color = 'grey95', fill = 'grey95',linewidth = 0.1) +
  geom_polygon(data = coast0, aes(x = long, y = lat, group = group), color = 'grey50', fill = 'white', linewidth = 0.15) +
  geom_rect(data = df_mar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'grey95') +
  geom_rect(data = df_tierra, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'white') +
  scale_y_continuous(name = NULL, breaks = c(-55, -46, -37, -28), labels = c('55º', '46º', '37º', '28º')) + 
  scale_x_continuous(name = NULL, breaks = c(-66, -58, -50), labels = c('66º', '58º', '50º')) +
  coord_equal(xlim = c(-70, -47), ylim = c(-57, -26), expand = 0) + 
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_blank(), legend.position = 'none',
        panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.35))
ggsave('Figure 1c.tiff', dpi = 900, width = 10.2, height = 10.6, units = 'cm', device = grDevices::tiff)

# Mosaic with 5% MTP
mod_mosaic_5_cazon[mod_mosaic_5_cazon == 2] <- 0
mod_mosaic_5_cazon[mod_mosaic_5_cazon == 3] <- 1.1
mod_mosaic_5_cazon[mod_mosaic_5_cazon == 4] <- 2.2
mod_mosaic_5_cazon[mod_mosaic_5_cazon == 5] <- 3.3
mod_mosaic_5_cazon[mod_mosaic_5_cazon == 6] <- 4.4

mod_mosaic_5_gatop[mod_mosaic_5_gatop == 2] <- 0
mod_mosaic_5_gatop[mod_mosaic_5_gatop == 3] <- 1.4
mod_mosaic_5_gatop[mod_mosaic_5_gatop == 4] <- 2.3
mod_mosaic_5_gatop[mod_mosaic_5_gatop == 5] <- 3.2
mod_mosaic_5_gatop[mod_mosaic_5_gatop == 6] <- 4.1

# General overlap
mod_mosaic_5 <- mosaic(mod_mosaic_5_cazon, mod_mosaic_5_gatop, fun = sum)
mod_mosaic_5[mod_mosaic_5 == 4.4] <- 0
mod_mosaic_5[mod_mosaic_5 == 3.3] <- 0
mod_mosaic_5[mod_mosaic_5 == 2.2] <- 0
mod_mosaic_5[mod_mosaic_5 == 1.1] <- 0
mod_mosaic_5[mod_mosaic_5 == 1.4] <- 0
mod_mosaic_5[mod_mosaic_5 == 2.3] <- 0
mod_mosaic_5[mod_mosaic_5 == 3.2] <- 0
mod_mosaic_5[mod_mosaic_5 == 4.1] <- 0
mod_mosaic_5[mod_mosaic_5 == 2.5] <- 2
mod_mosaic_5[mod_mosaic_5 > 3 & mod_mosaic_5 < 4] <- 3
mod_mosaic_5[mod_mosaic_5 > 4 & mod_mosaic_5 < 5] <- 4
mod_mosaic_5[mod_mosaic_5 > 5 & mod_mosaic_5 < 6] <- 5
mod_mosaic_5[mod_mosaic_5 > 6 & mod_mosaic_5 < 7] <- 6
mod_mosaic_5[mod_mosaic_5 > 7 & mod_mosaic_5 < 8] <- 7
mod_mosaic_5[mod_mosaic_5 == 8.5] <- 8
round(table(getValues(mod_mosaic_5))[-1] * 100 / sum(table(getValues(mod_mosaic_5))[-1]), 1)

# Overlap favouring G. galeus
mod_mosaic_5c <- mosaic(mod_mosaic_5_cazon, mod_mosaic_5_gatop, fun = sum)
mod_mosaic_5c[mod_mosaic_5c == 1.1] <- 0
mod_mosaic_5c[mod_mosaic_5c == 1.4] <- 0
mod_mosaic_5c[mod_mosaic_5c == 2.2] <- 0
mod_mosaic_5c[mod_mosaic_5c == 2.3] <- 0
mod_mosaic_5c[mod_mosaic_5c == 3.2] <- 0
mod_mosaic_5c[mod_mosaic_5c == 3.3] <- 0
mod_mosaic_5c[mod_mosaic_5c == 4.1] <- 0
mod_mosaic_5c[mod_mosaic_5c == 4.4] <- 0
mod_mosaic_5c[mod_mosaic_5c == 2.5] <- 1
mod_mosaic_5c[mod_mosaic_5c == 3.4] <- 1
mod_mosaic_5c[mod_mosaic_5c > 4.2 & mod_mosaic_5c < 4.4] <- 1
mod_mosaic_5c[mod_mosaic_5c > 5.1 & mod_mosaic_5c < 5.3] <- 1
mod_mosaic_5c[mod_mosaic_5c == 3.6] <- 1
mod_mosaic_5c[mod_mosaic_5c == 4.5] <- 1
mod_mosaic_5c[mod_mosaic_5c == 5.4] <- 1
mod_mosaic_5c[mod_mosaic_5c == 6.3] <- 1
mod_mosaic_5c[mod_mosaic_5c > 4.6 & mod_mosaic_5c < 4.8] <- 1
mod_mosaic_5c[mod_mosaic_5c == 5.6] <- 1
mod_mosaic_5c[mod_mosaic_5c == 6.5] <- 2
mod_mosaic_5c[mod_mosaic_5c > 7.3 & mod_mosaic_5c < 7.5] <- 1
mod_mosaic_5c[mod_mosaic_5c > 5.7 & mod_mosaic_5c < 5.9] <- 1
mod_mosaic_5c[mod_mosaic_5c == 6.7] <- 1
mod_mosaic_5c[mod_mosaic_5c > 7.5 & mod_mosaic_5c < 7.7] <- 3
mod_mosaic_5c[mod_mosaic_5c == 8.5] <- 4
round(table(getValues(mod_mosaic_5c))[-1] * 100 / sum(table(getValues(mod_mosaic_5c))[-1]), 1)

# Overlap favouring N. cepedianus
mod_mosaic_5g <- mosaic(mod_mosaic_5_cazon, mod_mosaic_5_gatop, fun = sum)
mod_mosaic_5g[mod_mosaic_5g == 1.1] <- 0
mod_mosaic_5g[mod_mosaic_5g == 1.4] <- 0
mod_mosaic_5g[mod_mosaic_5g == 2.2] <- 0
mod_mosaic_5g[mod_mosaic_5g == 2.3] <- 0
mod_mosaic_5g[mod_mosaic_5g == 3.2] <- 0
mod_mosaic_5g[mod_mosaic_5g == 3.3] <- 0
mod_mosaic_5g[mod_mosaic_5g == 4.1] <- 0
mod_mosaic_5g[mod_mosaic_5g == 4.4] <- 0
mod_mosaic_5g[mod_mosaic_5g == 2.5] <- 1
mod_mosaic_5g[mod_mosaic_5g == 3.4] <- 1
mod_mosaic_5g[mod_mosaic_5g > 4.2 & mod_mosaic_5g < 4.4] <- 1
mod_mosaic_5g[mod_mosaic_5g > 5.1 & mod_mosaic_5g < 5.3] <- 1
mod_mosaic_5g[mod_mosaic_5g == 3.6] <- 1
mod_mosaic_5g[mod_mosaic_5g == 4.5] <- 1
mod_mosaic_5g[mod_mosaic_5g == 5.4] <- 1
mod_mosaic_5g[mod_mosaic_5g == 6.3] <- 1
mod_mosaic_5g[mod_mosaic_5g > 4.6 & mod_mosaic_5g < 4.8] <- 1
mod_mosaic_5g[mod_mosaic_5g == 5.6] <- 1
mod_mosaic_5g[mod_mosaic_5g == 6.5] <- 2
mod_mosaic_5g[mod_mosaic_5g > 7.3 & mod_mosaic_5g < 7.5] <- 3
mod_mosaic_5g[mod_mosaic_5g > 5.7 & mod_mosaic_5g < 5.9] <- 1
mod_mosaic_5g[mod_mosaic_5g == 6.7] <- 1
mod_mosaic_5g[mod_mosaic_5g > 7.5 & mod_mosaic_5g < 7.7] <- 1
mod_mosaic_5g[mod_mosaic_5g == 8.5] <- 4
round(table(getValues(mod_mosaic_5g))[-1] * 100 / sum(table(getValues(mod_mosaic_5g))[-1]), 1)

# Plot
Col <- c('#000004FF', '#280B54FF', '#65156EFF', '#9F2A63FF', '#D44842FF', '#F57D15FF', '#FAC127FF', '#FCFFA4FF')
df_5 <- data.frame(coordinates(mod_mosaic_5), as.data.frame(mod_mosaic_5))
ggplot() +
  geom_tile(data = df_5, aes(x = x, y = y, fill = as.factor(layer))) + 
  scale_fill_manual(values = Col, na.value = 'grey95') + 
  geom_polygon(data = df_rdp, aes(x = x, y = y), color = 'grey95', fill = 'grey95', linewidth = 0.75) +
  geom_polygon(data = df_lagoa, aes(x = x, y = y), color = 'grey95', fill = 'grey95',linewidth = 0.1) +
  geom_polygon(data = coast0, aes(x = long, y = lat, group = group), color = 'grey50', fill = 'white', linewidth = 0.15) +
  geom_rect(data = df_mar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'grey95') +
  geom_rect(data = df_tierra, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'white') +
  scale_y_continuous(name = NULL, breaks = c(-55, -46, -37, -28), labels = c('55º', '46º', '37º', '28º')) + 
  scale_x_continuous(name = NULL, breaks = c(-66, -58, -50), labels = c('66º', '58º', '50º')) +
  coord_equal(xlim = c(-70, -47), ylim = c(-57, -26), expand = 0) + 
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_blank(), legend.position = 'none',
        panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.35))
ggsave('Figure S1.2a.tiff', dpi = 900, width = 10.2, height = 10.6, units = 'cm', device = grDevices::tiff)

Col <- c('#000004FF', '#56106EFF', '#BB3754FF', '#F98C0AFF', '#FCFFA4FF')
df_5_cazon <- data.frame(coordinates(mod_mosaic_5c), as.data.frame(mod_mosaic_5c))
ggplot() +
  geom_tile(data = df_5_cazon, aes(x = x, y = y, fill = as.factor(layer))) + 
  scale_fill_manual(values = Col, na.value = 'grey95') + 
  geom_polygon(data = df_rdp, aes(x = x, y = y), color = 'grey95', fill = 'grey95', linewidth = 0.75) +
  geom_polygon(data = df_lagoa, aes(x = x, y = y), color = 'grey95', fill = 'grey95',linewidth = 0.1) +
  geom_polygon(data = coast0, aes(x = long, y = lat, group = group), color = 'grey50', fill = 'white', linewidth = 0.15) +
  geom_rect(data = df_mar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'grey95') +
  geom_rect(data = df_tierra, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'white') +
  scale_y_continuous(name = NULL, breaks = c(-55, -46, -37, -28), labels = c('55º', '46º', '37º', '28º')) + 
  scale_x_continuous(name = NULL, breaks = c(-66, -58, -50), labels = c('66º', '58º', '50º')) +
  coord_equal(xlim = c(-70, -47), ylim = c(-57, -26), expand = 0) + 
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_blank(), legend.position = 'none',
        panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.35))
ggsave('Figure S1.2b.tiff', dpi = 900, width = 10.2, height = 10.6, units = 'cm', device = grDevices::tiff)

df_5_gatop <- data.frame(coordinates(mod_mosaic_5g), as.data.frame(mod_mosaic_5g))
ggplot() +
  geom_tile(data = df_5_gatop, aes(x = x, y = y, fill = as.factor(layer))) + 
  scale_fill_manual(values = Col, na.value = 'grey95') + 
  geom_polygon(data = df_rdp, aes(x = x, y = y), color = 'grey95', fill = 'grey95', linewidth = 0.75) +
  geom_polygon(data = df_lagoa, aes(x = x, y = y), color = 'grey95', fill = 'grey95',linewidth = 0.1) +
  geom_polygon(data = coast0, aes(x = long, y = lat, group = group), color = 'grey50', fill = 'white', linewidth = 0.15) +
  geom_rect(data = df_mar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'grey95') +
  geom_rect(data = df_tierra, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'white') +
  scale_y_continuous(name = NULL, breaks = c(-55, -46, -37, -28), labels = c('55º', '46º', '37º', '28º')) + 
  scale_x_continuous(name = NULL, breaks = c(-66, -58, -50), labels = c('66º', '58º', '50º')) +
  coord_equal(xlim = c(-70, -47), ylim = c(-57, -26), expand = 0) + 
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_blank(), legend.position = 'none',
        panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.35))
ggsave('Figure S1.2c.tiff', dpi = 900, width = 10.2, height = 10.6, units = 'cm', device = grDevices::tiff)


# Seasonal overlap
mod_mosaic_10_summer <- mosaic(bin_10_summer_cazon + 1, bin_10_summer_gatop, fun = sum) # Summer
round(table(getValues(mod_mosaic_10_summer))[-1] * 100 / sum(table(getValues(mod_mosaic_10_summer))[-1]), 1)
mod_mosaic_10_autumn <- mosaic(bin_10_autumn_cazon + 1, bin_10_autumn_gatop, fun = sum) # Autumn
round(table(getValues(mod_mosaic_10_autumn))[-1] * 100 / sum(table(getValues(mod_mosaic_10_autumn))[-1]), 1)
mod_mosaic_10_winter <- mosaic(bin_10_winter_cazon + 1, bin_10_winter_gatop, fun = sum) # Winter
round(table(getValues(mod_mosaic_10_winter))[-1] * 100 / sum(table(getValues(mod_mosaic_10_winter))[-1]), 1)
mod_mosaic_10_spring<- mosaic(bin_10_spring_cazon + 1, bin_10_spring_gatop, fun = sum) # Spring
round(table(getValues(mod_mosaic_10_spring))[-1] * 100 / sum(table(getValues(mod_mosaic_10_spring))[-1]), 1)

mod_mosaic_5_summer <- mosaic(bin_5_summer_cazon + 1, bin_5_summer_gatop, fun = sum) # Summer
round(table(getValues(mod_mosaic_5_summer))[-1] * 100 / sum(table(getValues(mod_mosaic_5_summer))[-1]), 1)
mod_mosaic_5_autumn <- mosaic(bin_5_autumn_cazon + 1, bin_5_autumn_gatop, fun = sum) # Autumn
round(table(getValues(mod_mosaic_5_autumn))[-1] * 100 / sum(table(getValues(mod_mosaic_5_autumn))[-1]), 1)
mod_mosaic_5_winter <- mosaic(bin_5_winter_cazon + 1, bin_5_winter_gatop, fun = sum) # Winter
round(table(getValues(mod_mosaic_5_winter))[-1] * 100 / sum(table(getValues(mod_mosaic_5_winter))[-1]), 1)
mod_mosaic_5_spring<- mosaic(bin_5_spring_cazon + 1, bin_5_spring_gatop, fun = sum) # Spring
round(table(getValues(mod_mosaic_5_spring))[-1] * 100 / sum(table(getValues(mod_mosaic_5_spring))[-1]), 1)

{
  Col <- c('#000004FF', '#781C6DFF', '#ED6925FF', '#FCFFA4FF')
  df_10_summer <- data.frame(coordinates(mod_mosaic_10_summer), as.data.frame(mod_mosaic_10_summer))
  ggplot() +
    geom_tile(data = df_10_summer, aes(x = x, y = y, fill = as.factor(layer))) + 
    scale_fill_manual(values = Col, na.value = '#000004FF') + 
    geom_polygon(data = df_rdp, aes(x = x, y = y), color = 'grey95', fill = 'grey95', linewidth = 0.75) +
    geom_polygon(data = df_lagoa, aes(x = x, y = y), color = 'grey95', fill = 'grey95',linewidth = 0.1) +
    geom_polygon(data = coast0, aes(x = long, y = lat, group = group), color = 'grey50', fill = 'white', linewidth = 0.15) +
    geom_rect(data = df_mar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'grey95') +
    geom_rect(data = df_tierra, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'white') +
    scale_y_continuous(name = NULL, breaks = c(-55, -46, -37, -28), labels = c('55º', '46º', '37º', '28º')) + 
    scale_x_continuous(name = NULL, breaks = c(-66, -58, -50), labels = c('66º', '58º', '50º')) +
    coord_equal(xlim = c(-70, -47), ylim = c(-57, -26), expand = 0) + 
    theme(panel.background = element_rect(fill = 'transparent'),
          panel.grid = element_blank(), legend.position = 'none',
          panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.5))
  ggsave('Figure 2_summer.tiff', dpi = 900, width = 10.2, height = 10.6, units = 'cm', device = grDevices::tiff)
  
  mod_mosaic_10_autumn <- extend(mod_mosaic_10_autumn, mod_mosaic_10_summer)
  df_10_autumn <- data.frame(coordinates(mod_mosaic_10_autumn), as.data.frame(mod_mosaic_10_autumn))
  ggplot() +
    geom_tile(data = df_10_autumn, aes(x = x, y = y, fill = as.factor(layer))) + 
    scale_fill_manual(values = Col, na.value = '#000004FF') + 
    geom_polygon(data = df_rdp, aes(x = x, y = y), color = 'grey95', fill = 'grey95', linewidth = 0.75) +
    geom_polygon(data = df_lagoa, aes(x = x, y = y), color = 'grey95', fill = 'grey95',linewidth = 0.1) +
    geom_polygon(data = coast0, aes(x = long, y = lat, group = group), color = 'grey50', fill = 'white', linewidth = 0.15) +
    geom_rect(data = df_mar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'grey95') +
    geom_rect(data = df_tierra, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'white') +
    scale_y_continuous(name = NULL, breaks = c(-55, -46, -37, -28), labels = c('55º', '46º', '37º', '28º')) + 
    scale_x_continuous(name = NULL, breaks = c(-66, -58, -50), labels = c('66º', '58º', '50º')) +
    coord_equal(xlim = c(-70, -47), ylim = c(-57, -26), expand = 0) + 
    theme(panel.background = element_rect(fill = 'transparent'),
          panel.grid = element_blank(), legend.position = 'none',
          panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.5))
  ggsave('Figure 2_autumn.tiff', dpi = 900, width = 10.2, height = 10.6, units = 'cm', device = grDevices::tiff)
  
  mod_mosaic_10_winter <- extend(mod_mosaic_10_winter, mod_mosaic_10_summer)
  df_10_winter <- data.frame(coordinates(mod_mosaic_10_winter), as.data.frame(mod_mosaic_10_winter))
  ggplot() +
    geom_tile(data = df_10_winter, aes(x = x, y = y, fill = as.factor(layer))) + 
    scale_fill_manual(values = Col, na.value = '#000004FF') + 
    geom_polygon(data = df_rdp, aes(x = x, y = y), color = 'grey95', fill = 'grey95', linewidth = 0.75) +
    geom_polygon(data = df_lagoa, aes(x = x, y = y), color = 'grey95', fill = 'grey95',linewidth = 0.1) +
    geom_polygon(data = coast0, aes(x = long, y = lat, group = group), color = 'grey50', fill = 'white', linewidth = 0.15) +
    geom_rect(data = df_mar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'grey95') +
    geom_rect(data = df_tierra, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'white') +
    scale_y_continuous(name = NULL, breaks = c(-55, -46, -37, -28), labels = c('55º', '46º', '37º', '28º')) + 
    scale_x_continuous(name = NULL, breaks = c(-66, -58, -50), labels = c('66º', '58º', '50º')) +
    coord_equal(xlim = c(-70, -47), ylim = c(-57, -26), expand = 0) + 
    theme(panel.background = element_rect(fill = 'transparent'),
          panel.grid = element_blank(), legend.position = 'none',
          panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.5))
  ggsave('Figure 2_winter.tiff', dpi = 900, width = 10.2, height = 10.6, units = 'cm', device = grDevices::tiff)
  
  mod_mosaic_10_spring <- extend(mod_mosaic_10_spring, mod_mosaic_10_summer)
  df_10_spring <- data.frame(coordinates(mod_mosaic_10_spring), as.data.frame(mod_mosaic_10_spring))
  ggplot() +
    geom_tile(data = df_10_spring, aes(x = x, y = y, fill = as.factor(layer))) + 
    scale_fill_manual(values = Col, na.value = '#000004FF') + 
    geom_polygon(data = df_rdp, aes(x = x, y = y), color = 'grey95', fill = 'grey95', linewidth = 0.75) +
    geom_polygon(data = df_lagoa, aes(x = x, y = y), color = 'grey95', fill = 'grey95',linewidth = 0.1) +
    geom_polygon(data = coast0, aes(x = long, y = lat, group = group), color = 'grey50', fill = 'white', linewidth = 0.15) +
    geom_rect(data = df_mar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'grey95') +
    geom_rect(data = df_tierra, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'white') +
    scale_y_continuous(name = NULL, breaks = c(-55, -46, -37, -28), labels = c('55º', '46º', '37º', '28º')) + 
    scale_x_continuous(name = NULL, breaks = c(-66, -58, -50), labels = c('66º', '58º', '50º')) +
    coord_equal(xlim = c(-70, -47), ylim = c(-57, -26), expand = 0) + 
    theme(panel.background = element_rect(fill = 'transparent'),
          panel.grid = element_blank(), legend.position = 'none',
          panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.5))
  ggsave('Figure 2_spring.tiff', dpi = 900, width = 10.2, height = 10.6, units = 'cm', device = grDevices::tiff)
}

{
  Col <- c('#000004FF', '#781C6DFF', '#ED6925FF', '#FCFFA4FF')
  df_5_summer <- data.frame(coordinates(mod_mosaic_5_summer), as.data.frame(mod_mosaic_5_summer))
  ggplot() +
    geom_tile(data = df_5_summer, aes(x = x, y = y, fill = as.factor(layer))) + 
    scale_fill_manual(values = Col, na.value = '#000004FF') + 
    geom_polygon(data = df_rdp, aes(x = x, y = y), color = 'grey95', fill = 'grey95', linewidth = 0.75) +
    geom_polygon(data = df_lagoa, aes(x = x, y = y), color = 'grey95', fill = 'grey95',linewidth = 0.1) +
    geom_polygon(data = coast0, aes(x = long, y = lat, group = group), color = 'grey50', fill = 'white', linewidth = 0.15) +
    geom_rect(data = df_mar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'grey95') +
    geom_rect(data = df_tierra, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'white') +
    scale_y_continuous(name = NULL, breaks = c(-55, -46, -37, -28), labels = c('55º', '46º', '37º', '28º')) + 
    scale_x_continuous(name = NULL, breaks = c(-66, -58, -50), labels = c('66º', '58º', '50º')) +
    coord_equal(xlim = c(-70, -47), ylim = c(-57, -26), expand = 0) + 
    theme(panel.background = element_rect(fill = 'transparent'),
          panel.grid = element_blank(), legend.position = 'none',
          panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.5))
  ggsave('Figure S1.3_Summer.tiff', dpi = 900, width = 10.2, height = 10.6, units = 'cm', device = grDevices::tiff)
  
  mod_mosaic_5_autumn <- extend(mod_mosaic_5_autumn, mod_mosaic_5_summer)
  df_5_autumn <- data.frame(coordinates(mod_mosaic_5_autumn), as.data.frame(mod_mosaic_5_autumn))
  ggplot() +
    geom_tile(data = df_5_autumn, aes(x = x, y = y, fill = as.factor(layer))) + 
    scale_fill_manual(values = Col, na.value = '#000004FF') + 
    geom_polygon(data = df_rdp, aes(x = x, y = y), color = 'grey95', fill = 'grey95', linewidth = 0.75) +
    geom_polygon(data = df_lagoa, aes(x = x, y = y), color = 'grey95', fill = 'grey95',linewidth = 0.1) +
    geom_polygon(data = coast0, aes(x = long, y = lat, group = group), color = 'grey50', fill = 'white', linewidth = 0.15) +
    geom_rect(data = df_mar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'grey95') +
    geom_rect(data = df_tierra, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'white') +
    scale_y_continuous(name = NULL, breaks = c(-55, -46, -37, -28), labels = c('55º', '46º', '37º', '28º')) + 
    scale_x_continuous(name = NULL, breaks = c(-66, -58, -50), labels = c('66º', '58º', '50º')) +
    coord_equal(xlim = c(-70, -47), ylim = c(-57, -26), expand = 0) + 
    theme(panel.background = element_rect(fill = 'transparent'),
          panel.grid = element_blank(), legend.position = 'none',
          panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.5))
  ggsave('Figure S1.3_Autumn.tiff', dpi = 900, width = 10.2, height = 10.6, units = 'cm', device = grDevices::tiff)
  
  mod_mosaic_5_winter <- extend(mod_mosaic_5_winter, mod_mosaic_5_summer)
  df_5_winter <- data.frame(coordinates(mod_mosaic_5_winter), as.data.frame(mod_mosaic_5_winter))
  ggplot() +
    geom_tile(data = df_5_winter, aes(x = x, y = y, fill = as.factor(layer))) + 
    scale_fill_manual(values = Col, na.value = '#000004FF') + 
    geom_polygon(data = df_rdp, aes(x = x, y = y), color = 'grey95', fill = 'grey95', linewidth = 0.75) +
    geom_polygon(data = df_lagoa, aes(x = x, y = y), color = 'grey95', fill = 'grey95',linewidth = 0.1) +
    geom_polygon(data = coast0, aes(x = long, y = lat, group = group), color = 'grey50', fill = 'white', linewidth = 0.15) +
    geom_rect(data = df_mar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'grey95') +
    geom_rect(data = df_tierra, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'white') +
    scale_y_continuous(name = NULL, breaks = c(-55, -46, -37, -28), labels = c('55º', '46º', '37º', '28º')) + 
    scale_x_continuous(name = NULL, breaks = c(-66, -58, -50), labels = c('66º', '58º', '50º')) +
    coord_equal(xlim = c(-70, -47), ylim = c(-57, -26), expand = 0) + 
    theme(panel.background = element_rect(fill = 'transparent'),
          panel.grid = element_blank(), legend.position = 'none',
          panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.5))
  ggsave('Figure S1.3_Winter.tiff', dpi = 900, width = 10.2, height = 10.6, units = 'cm', device = grDevices::tiff)
  
  mod_mosaic_5_spring <- extend(mod_mosaic_5_spring, mod_mosaic_5_summer)
  df_5_spring <- data.frame(coordinates(mod_mosaic_5_spring), as.data.frame(mod_mosaic_5_spring))
  ggplot() +
    geom_tile(data = df_5_spring, aes(x = x, y = y, fill = as.factor(layer))) + 
    scale_fill_manual(values = Col, na.value = '#000004FF') + 
    geom_polygon(data = df_rdp, aes(x = x, y = y), color = 'grey95', fill = 'grey95', linewidth = 0.75) +
    geom_polygon(data = df_lagoa, aes(x = x, y = y), color = 'grey95', fill = 'grey95',linewidth = 0.1) +
    geom_polygon(data = coast0, aes(x = long, y = lat, group = group), color = 'grey50', fill = 'white', linewidth = 0.15) +
    geom_rect(data = df_mar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'grey95') +
    geom_rect(data = df_tierra, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'white') +
    scale_y_continuous(name = NULL, breaks = c(-55, -46, -37, -28), labels = c('55º', '46º', '37º', '28º')) + 
    scale_x_continuous(name = NULL, breaks = c(-66, -58, -50), labels = c('66º', '58º', '50º')) +
    coord_equal(xlim = c(-70, -47), ylim = c(-57, -26), expand = 0) + 
    theme(panel.background = element_rect(fill = 'transparent'),
          panel.grid = element_blank(), legend.position = 'none',
          panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.5))
  ggsave('Figure S1.3_Spring.tiff', dpi = 900, width = 10.2, height = 10.6, units = 'cm', device = grDevices::tiff)
}


#------------------------------------ Figures 3 & S1.4 ---------------------------------------

# Overlap between regional annual, regional seasonal, and global models

# Galeorhinus galeus

{
  final_mod0 <- raster('G_galeus_annual.tif')
  final_mod0 <- crop(final_mod0, extent(final_mod0) + c(0.5, 0, 0, 0))
  final_mod <- extend(final_mod0, extent(final_mod0) + 1.75)
  occ_cal <- read.csv('G_galeus_annual.csv')
  
  # Threshold (%5 minimum training presence)
  mtp5 <- sdm_threshold.5(final_mod, occ_cal[, c('longitude', 'latitude')], 'p05', binary = F)
  regional_anual_5_cazon <- final_mod
  regional_anual_5_cazon[regional_anual_5_cazon < minValue(mtp5)] <- NA
  regional_anual_5_cazon[regional_anual_5_cazon >= minValue(mtp5)] <- 1
  
  # Threshold (%10 minimum training presence)
  mtp10 <- sdm_threshold.10(final_mod, occ_cal[, c('longitude', 'latitude')], 'p10', binary = F)
  regional_anual_10_cazon <- final_mod
  regional_anual_10_cazon[regional_anual_10_cazon < minValue(mtp10)] <- NA
  regional_anual_10_cazon[regional_anual_10_cazon >= minValue(mtp10)] <- 1
} # Regional annual
{
  # Summer
  final_mod <- raster('G_galeus_summer.tif')
  occ_cal <- read.csv('G_galeus_summer.csv')
  # Threshold (%5 minimum training presence)
  mtp5 <- sdm_threshold.5(final_mod, occ_cal[, c('longitude', 'latitude')], 'p05', binary = F)
  bin_5_summer_cazon <- final_mod
  bin_5_summer_cazon[bin_5_summer_cazon < minValue(mtp5)] <- NA
  bin_5_summer_cazon[bin_5_summer_cazon >= minValue(mtp5)] <- 1
  # Threshold (%10 minimum training presence)
  mtp10 <- sdm_threshold.10(final_mod, occ_cal[, c('longitude', 'latitude')], 'p10', binary = F)
  bin_10_summer_cazon <- final_mod
  bin_10_summer_cazon[bin_10_summer_cazon < minValue(mtp10)] <- NA
  bin_10_summer_cazon[bin_10_summer_cazon >= minValue(mtp10)] <- 1
  
  # Autumn
  final_mod <- raster('G_galeus_autumn.tif') 
  occ_cal <- read.csv('G_galeus_autumn.csv')
  # Threshold (%5 minimum training presence)
  mtp5 <- sdm_threshold.5(final_mod, occ_cal[, c('longitude', 'latitude')], 'p05', binary = F)
  bin_5_autumn_cazon <- final_mod
  bin_5_autumn_cazon[bin_5_autumn_cazon < minValue(mtp5)] <- NA
  bin_5_autumn_cazon[bin_5_autumn_cazon >= minValue(mtp5)] <- 1
  # Threshold (%10 minimum training presence)
  mtp10 <- sdm_threshold.10(final_mod, occ_cal[, c('longitude', 'latitude')], 'p10', binary = F)
  bin_10_autumn_cazon <- final_mod
  bin_10_autumn_cazon[bin_10_autumn_cazon < minValue(mtp10)] <- NA
  bin_10_autumn_cazon[bin_10_autumn_cazon >= minValue(mtp10)] <- 1
  
  # Winter
  final_mod <- raster('G_galeus_winter.tif') 
  occ_cal <- read.csv('G_galeus_winter.csv')
  # Threshold (%5 minimum training presence)
  mtp5 <- sdm_threshold.5(final_mod, occ_cal[, c('longitude', 'latitude')], 'p05', binary = F)
  bin_5_winter_cazon <- final_mod
  bin_5_winter_cazon[bin_5_winter_cazon < minValue(mtp5)] <- NA
  bin_5_winter_cazon[bin_5_winter_cazon >= minValue(mtp5)] <- 1
  # Threshold (%10 minimum training presence)
  mtp10 <- sdm_threshold.10(final_mod, occ_cal[, c('longitude', 'latitude')], 'p10', binary = F)
  bin_10_winter_cazon <- final_mod
  bin_10_winter_cazon[bin_10_winter_cazon < minValue(mtp10)] <- NA
  bin_10_winter_cazon[bin_10_winter_cazon >= minValue(mtp10)] <- 1
  
  # Spring
  final_mod <- raster('G_galeus_spring.tif')
  occ_cal <- read.csv('G_galeus_spring.csv')
  # Threshold (%5 minimum training presence)
  mtp5 <- sdm_threshold.5(final_mod, occ_cal[, c('longitude', 'latitude')], 'p05', binary = F)
  bin_5_spring_cazon <- final_mod
  bin_5_spring_cazon[bin_5_spring_cazon < minValue(mtp5)] <- NA
  bin_5_spring_cazon[bin_5_spring_cazon >= minValue(mtp5)] <- 1
  # Threshold (%10 minimum training presence)
  mtp10 <- sdm_threshold.10(final_mod, occ_cal[, c('longitude', 'latitude')], 'p10', binary = F)
  bin_10_spring_cazon <- final_mod
  bin_10_spring_cazon[bin_10_spring_cazon < minValue(mtp10)] <- NA
  bin_10_spring_cazon[bin_10_spring_cazon >= minValue(mtp10)] <- 1
  
  # Mosaic 10% MTP
  regional_estacional_10_cazon <- mosaic(bin_10_summer_cazon, bin_10_autumn_cazon, bin_10_winter_cazon, bin_10_spring_cazon, fun = sum)
  regional_estacional_10_cazon[regional_estacional_10_cazon < 1] <- NA
  area_nucleo_10_cazon0 <- regional_estacional_10_cazon
  area_nucleo_10_cazon0[area_nucleo_10_cazon0 < 4] <- NA
  area_nucleo_10_cazon <- as.polygons(rast(area_nucleo_10_cazon0))
  area_nucleo_10_cazon <- as(area_nucleo_10_cazon, "Spatial")
  regional_estacional_10_cazon[regional_estacional_10_cazon > 0] <- 1
  
  # Mosaic 5% MTP
  regional_estacional_5_cazon <- mosaic(bin_5_summer_cazon, bin_5_autumn_cazon, bin_5_winter_cazon, bin_5_spring_cazon, fun = sum)
  regional_estacional_5_cazon[regional_estacional_5_cazon < 1] <- NA
  area_nucleo_5_cazon0 <- regional_estacional_5_cazon
  area_nucleo_5_cazon0[area_nucleo_5_cazon0 < 4] <- NA
  area_nucleo_5_cazon <- as.polygons(rast(area_nucleo_5_cazon0))
  area_nucleo_5_cazon <- as(area_nucleo_5_cazon, "Spatial")
  regional_estacional_5_cazon[regional_estacional_5_cazon > 0] <- 1
} # Regional estacional
{
  pred <- raster('G_galeus_global_Predictions.tif')
  occ_cal <- read.csv('G_galeus_global.csv') 
  sdm_threshold.5_caz <- function(sdm, occs, type = 'mtp', binary = FALSE){
    occPredVals <- raster::extract(sdm, occs)
    if(type == 'mtp'){
      thresh <- min(na.omit(occPredVals))
    } else if(type == 'p05'){
      if(length(occPredVals) < 10){
        p05 <- floor(length(occPredVals) * 0.8)
      } else {
        p05 <- ceiling(length(occPredVals) * 0.8)
      }
      thresh <- rev(sort(occPredVals))[p05]
    }
    sdm_thresh <- sdm
    sdm_thresh[sdm_thresh < thresh] <- NA
    if(binary){
      sdm_thresh[sdm_thresh >= thresh] <- 1
    }
    return(sdm_thresh)
  } # MTP5
  sdm_threshold.10_caz <- function(sdm, occs, type = 'mtp', binary = FALSE){
    occPredVals <- raster::extract(sdm, occs)
    if(type == 'mtp'){
      thresh <- min(na.omit(occPredVals))
    } else if(type == 'p10'){
      if(length(occPredVals) < 10){
        p10 <- floor(length(occPredVals) * 0.75)
      } else {
        p10 <- ceiling(length(occPredVals) * 0.75)
      }
      thresh <- rev(sort(occPredVals))[p10]
    }
    sdm_thresh <- sdm
    sdm_thresh[sdm_thresh < thresh] <- NA
    if(binary){
      sdm_thresh[sdm_thresh >= thresh] <- 1
    }
    return(sdm_thresh)
  } # MTP10
  
  # Threshold (%5 minimum training presence)
  mtp5 <- sdm_threshold.5_caz(pred, occ_cal[, c('Longitude', 'Latitude')], 'p05', binary = F)
  global_5_cazon <- pred
  global_5_cazon[global_5_cazon < minValue(mtp5)] <- NA
  global_5_cazon[global_5_cazon >= minValue(mtp5)] <- 1
  global_5_cazon <- crop(global_5_cazon, regional_estacional_10_cazon)
  
  # Threshold (%10 minimum training presence)
  mtp10 <- sdm_threshold.10_caz(pred, occ_cal[, c('Longitude', 'Latitude')], 'p10', binary = F)
  global_10_cazon <- pred
  global_10_cazon[global_10_cazon < minValue(mtp10)] <- NA
  global_10_cazon[global_10_cazon >= minValue(mtp10)] <- 1
  global_10_cazon <- crop(global_10_cazon, regional_estacional_10_cazon)
} # Global

# Extend, cut, and reproject to the same graph
regional_anual_10_cazon <- extend(regional_anual_10_cazon, regional_estacional_10_cazon)
regional_anual_10_cazon <- crop(regional_anual_10_cazon, regional_estacional_10_cazon)
global_10_cazon <- extend(global_10_cazon, regional_estacional_10_cazon)
global_10_cazon <- crop(global_10_cazon, regional_estacional_10_cazon)
regional_anual_5_cazon <- extend(regional_anual_5_cazon, regional_estacional_5_cazon)
regional_anual_5_cazon <- crop(regional_anual_5_cazon, regional_estacional_5_cazon)
global_5_cazon <- extend(global_5_cazon, regional_estacional_5_cazon)
global_5_cazon <- crop(global_5_cazon, regional_estacional_5_cazon)

# Reproject to the lowest resolution among species
regional_estacional_10_cazon <- projectRaster(regional_estacional_10_cazon, global_10_cazon)
regional_estacional_10_cazon[regional_estacional_10_cazon >= minValue(regional_estacional_10_cazon)] <- 1
regional_anual_10_cazon <- projectRaster(regional_anual_10_cazon, global_10_cazon)
regional_anual_10_cazon[regional_anual_10_cazon >= minValue(regional_anual_10_cazon)] <- 1
area_nucleo_10_cazon0 <- projectRaster(area_nucleo_10_cazon0, global_10_cazon)
area_nucleo_10_cazon0[area_nucleo_10_cazon0 >= minValue(area_nucleo_10_cazon0)] <- 1
regional_estacional_5_cazon <- projectRaster(regional_estacional_5_cazon, global_5_cazon)
regional_estacional_5_cazon[regional_estacional_5_cazon >= minValue(regional_estacional_5_cazon)] <- 1
regional_anual_5_cazon <- projectRaster(regional_anual_5_cazon, global_5_cazon)
regional_anual_5_cazon[regional_anual_5_cazon >= minValue(regional_anual_5_cazon)] <- 1
area_nucleo_5_cazon0 <- projectRaster(area_nucleo_5_cazon0, global_10_cazon)
area_nucleo_5_cazon0[area_nucleo_5_cazon0 >= minValue(area_nucleo_5_cazon0)] <- 1

# Mosaic
mosaico_10_cazon <- mosaic(regional_anual_10_cazon, regional_estacional_10_cazon, global_10_cazon, fun = sum)
round(table(getValues(mosaico_10_cazon))[-1] * 100 / sum(table(getValues(mosaico_10_cazon))[-1]), 1)
round(table(getValues(area_nucleo_10_cazon0)) * 100 / sum(table(getValues(mosaico_10_cazon))[-1], table(getValues(area_nucleo_10_cazon0))), 1)
area_nucleo_10 <- mask(mosaico_10_cazon, area_nucleo_10_cazon0)
round(table(getValues(area_nucleo_10)) * 100 / sum(table(getValues(area_nucleo_10))), 1)
mosaico_5_cazon <- mosaic(regional_anual_5_cazon, regional_estacional_5_cazon, global_5_cazon, fun = sum)
round(table(getValues(mosaico_5_cazon))[-1] * 100 / sum(table(getValues(mosaico_5_cazon))[-1]), 1)
round(table(getValues(area_nucleo_5_cazon0)) * 100 / sum(table(getValues(mosaico_5_cazon))[-1], table(getValues(area_nucleo_5_cazon0))), 1)
area_nucleo_5 <- mask(mosaico_5_cazon, area_nucleo_5_cazon0)
round(table(getValues(area_nucleo_5)) * 100 / sum(table(getValues(area_nucleo_5))), 1)

Col <- c('#000004FF', '#781C6DFF', '#ED6925FF', '#FCFFA4FF')

df_10 <- data.frame(coordinates(mosaico_10_cazon), as.data.frame(mosaico_10_cazon))
ggplot() +
  geom_tile(data = df_10, aes(x = x, y = y, fill = as.factor(layer))) + 
  scale_fill_manual(values = Col, na.value = '#000004FF') + 
  geom_polygon(data = df_rdp, aes(x = x, y = y), color = 'grey95', fill = 'grey95', linewidth = 0.75) +
  geom_polygon(data = df_lagoa, aes(x = x, y = y), color = 'grey95', fill = 'grey95',linewidth = 0.1) +
  geom_polygon(data = coast0, aes(x = long, y = lat, group = group), color = 'grey50', fill = 'white', linewidth = 0.15) +
  geom_polygon(data = area_nucleo_10_cazon, aes(x = long, y = lat, group = group), color = '#000004FF', fill = "#000004FF", alpha = 0.1, linewidth = 0.3) +
  geom_rect(data = df_mar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = '#000004FF') +
  geom_rect(data = df_tierra, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'white') +
  scale_y_continuous(name = NULL, breaks = c(-54, -44, -34, -24), labels = c('54º', '44º', '34º', '24º')) + 
  scale_x_continuous(name = NULL, breaks = c(-66, -58, -50), labels = c('66º', '58º', '50º')) +
  coord_equal(xlim = c(-70, -44), ylim = c(-57, -22), expand = 0) + 
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_blank(), legend.position = 'none',
        panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.35))
ggsave('Figure 3a.tiff', dpi = 900, width = 10.2, height = 10.6, units = 'cm', device = grDevices::tiff)


# Notorynchus cepedianus

{
  final_mod0 <- raster('N_cepedianus_annual.tif')
  final_mod0 <- crop(final_mod0, extent(final_mod0) + c(0.5, 0, 0, 0))
  final_mod <- extend(final_mod0, extent(final_mod0) + 1.75)
  occ_cal <- read.csv('N_cepedianus_annual.csv')
  
  # Threshold (%5 minimum training presence)
  mtp5 <- sdm_threshold.5(final_mod, occ_cal[, c('longitude', 'latitude')], 'p05', binary = F)
  regional_anual_5_gatop <- final_mod
  regional_anual_5_gatop[regional_anual_5_gatop < minValue(mtp5)] <- NA
  regional_anual_5_gatop[regional_anual_5_gatop >= minValue(mtp5)] <- 1
  
  # Threshold (%10 minimum training presence)
  mtp10 <- sdm_threshold.10(final_mod, occ_cal[, c('longitude', 'latitude')], 'p10', binary = F)
  regional_anual_10_gatop <- final_mod
  regional_anual_10_gatop[regional_anual_10_gatop < minValue(mtp10)] <- NA
  regional_anual_10_gatop[regional_anual_10_gatop >= minValue(mtp10)] <- 1
} # Regional annual
{
  # Summer
  final_mod <- raster('N_cepedianus_summer.tif')
  occ_cal <- read.csv('N_cepedianus_summer.csv')
  # Threshold (%5 minimum training presence)
  mtp5 <- sdm_threshold.5(final_mod, occ_cal[, c('longitude', 'latitude')], 'p05', binary = F)
  bin_5_summer_gatop <- final_mod
  bin_5_summer_gatop[bin_5_summer_gatop < minValue(mtp5)] <- NA
  bin_5_summer_gatop[bin_5_summer_gatop >= minValue(mtp5)] <- 1
  # Threshold (%10 minimum training presence)
  mtp10 <- sdm_threshold.10(final_mod, occ_cal[, c('longitude', 'latitude')], 'p10', binary = F)
  bin_10_summer_gatop <- final_mod
  bin_10_summer_gatop[bin_10_summer_gatop < minValue(mtp10)] <- NA
  bin_10_summer_gatop[bin_10_summer_gatop >= minValue(mtp10)] <- 1
  
  # Autumn
  final_mod <- raster('N_cepedianus_autumn.tif') 
  occ_cal <- read.csv('N_cepedianus_autumn.csv')
  # Threshold (%5 minimum training presence)
  mtp5 <- sdm_threshold.5(final_mod, occ_cal[, c('longitude', 'latitude')], 'p05', binary = F)
  bin_5_autumn_gatop <- final_mod
  bin_5_autumn_gatop[bin_5_autumn_gatop < minValue(mtp5)] <- NA
  bin_5_autumn_gatop[bin_5_autumn_gatop >= minValue(mtp5)] <- 1
  # Threshold (%10 minimum training presence)
  mtp10 <- sdm_threshold.10(final_mod, occ_cal[, c('longitude', 'latitude')], 'p10', binary = F)
  bin_10_autumn_gatop <- final_mod
  bin_10_autumn_gatop[bin_10_autumn_gatop < minValue(mtp10)] <- NA
  bin_10_autumn_gatop[bin_10_autumn_gatop >= minValue(mtp10)] <- 1
  
  # Winter
  final_mod <- raster('N_cepedianus_winter.tif') 
  occ_cal <- read.csv('N_cepedianus_winter.csv')
  # Threshold (%5 minimum training presence)
  mtp5 <- sdm_threshold.5(final_mod, occ_cal[, c('longitude', 'latitude')], 'p05', binary = F)
  bin_5_winter_gatop <- final_mod
  bin_5_winter_gatop[bin_5_winter_gatop < minValue(mtp5)] <- NA
  bin_5_winter_gatop[bin_5_winter_gatop >= minValue(mtp5)] <- 1
  # Threshold (%10 minimum training presence)
  mtp10 <- sdm_threshold.10(final_mod, occ_cal[, c('longitude', 'latitude')], 'p10', binary = F)
  bin_10_winter_gatop <- final_mod
  bin_10_winter_gatop[bin_10_winter_gatop < minValue(mtp10)] <- NA
  bin_10_winter_gatop[bin_10_winter_gatop >= minValue(mtp10)] <- 1
  
  # Spring
  final_mod <- raster('N_cepedianus_spring.tif')
  occ_cal <- read.csv('N_cepedianus_spring.csv')
  # Threshold (%5 minimum training presence)
  mtp5 <- sdm_threshold.5(final_mod, occ_cal[, c('longitude', 'latitude')], 'p05', binary = F)
  bin_5_spring_gatop <- final_mod
  bin_5_spring_gatop[bin_5_spring_gatop < minValue(mtp5)] <- NA
  bin_5_spring_gatop[bin_5_spring_gatop >= minValue(mtp5)] <- 1
  # Threshold (%10 minimum training presence)
  mtp10 <- sdm_threshold.10(final_mod, occ_cal[, c('longitude', 'latitude')], 'p10', binary = F)
  bin_10_spring_gatop <- final_mod
  bin_10_spring_gatop[bin_10_spring_gatop < minValue(mtp10)] <- NA
  bin_10_spring_gatop[bin_10_spring_gatop >= minValue(mtp10)] <- 1
  
  # Mosaic 10% MTP
  regional_estacional_10_gatop <- mosaic(bin_10_summer_gatop, bin_10_autumn_gatop, bin_10_winter_gatop, bin_10_spring_gatop, fun = sum)
  regional_estacional_10_gatop[regional_estacional_10_gatop < 1] <- NA
  area_nucleo_10_gatop0 <- regional_estacional_10_gatop
  area_nucleo_10_gatop0[area_nucleo_10_gatop0 < 4] <- NA
  area_nucleo_10_gatop <- as.polygons(rast(area_nucleo_10_gatop0))
  area_nucleo_10_gatop <- as(area_nucleo_10_gatop, "Spatial")
  regional_estacional_10_gatop[regional_estacional_10_gatop > 0] <- 1
  
  # Mosaic 5% MTP
  regional_estacional_5_gatop <- mosaic(bin_5_summer_gatop, bin_5_autumn_gatop, bin_5_winter_gatop, bin_5_spring_gatop, fun = sum)
  regional_estacional_5_gatop[regional_estacional_5_gatop < 1] <- NA
  area_nucleo_5_gatop0 <- regional_estacional_5_gatop
  area_nucleo_5_gatop0[area_nucleo_5_gatop0 < 4] <- NA
  area_nucleo_5_gatop <- as.polygons(rast(area_nucleo_5_gatop0))
  area_nucleo_5_gatop <- as(area_nucleo_5_gatop, "Spatial")
  regional_estacional_5_gatop[regional_estacional_5_gatop > 0] <- 1
} # Regional seasonal
{
  pred <- raster('N_cepedianus_global_Predictions.tif')
  occ_cal <- read.csv('N_cepedianus_global.csv') 
  sdm_threshold.5_gat <- function(sdm, occs, type = 'mtp', binary = FALSE){
    occPredVals <- raster::extract(sdm, occs)
    if(type == 'mtp'){
      thresh <- min(na.omit(occPredVals))
    } else if(type == 'p05'){
      if(length(occPredVals) < 10){
        p05 <- floor(length(occPredVals) * 0.85)
      } else {
        p05 <- ceiling(length(occPredVals) * 0.85)
      }
      thresh <- rev(sort(occPredVals))[p05]
    }
    sdm_thresh <- sdm
    sdm_thresh[sdm_thresh < thresh] <- NA
    if(binary){
      sdm_thresh[sdm_thresh >= thresh] <- 1
    }
    return(sdm_thresh)
  } # MTP5
  sdm_threshold.10_gat <- function(sdm, occs, type = 'mtp', binary = FALSE){
    occPredVals <- raster::extract(sdm, occs)
    if(type == 'mtp'){
      thresh <- min(na.omit(occPredVals))
    } else if(type == 'p10'){
      if(length(occPredVals) < 10){
        p10 <- floor(length(occPredVals) * 0.8)
      } else {
        p10 <- ceiling(length(occPredVals) * 0.8)
      }
      thresh <- rev(sort(occPredVals))[p10]
    }
    sdm_thresh <- sdm
    sdm_thresh[sdm_thresh < thresh] <- NA
    if(binary){
      sdm_thresh[sdm_thresh >= thresh] <- 1
    }
    return(sdm_thresh)
  } # MTP10
  
  # Threshold (%5 minimum training presence)
  mtp5 <- sdm_threshold.5_gat(pred, occ_cal[, c('Longitude', 'Latitude')], 'p05', binary = F)
  global_5_gatop <- pred
  global_5_gatop[global_5_gatop < minValue(mtp5)] <- NA
  global_5_gatop[global_5_gatop >= minValue(mtp5)] <- 1
  global_5_gatop <- crop(global_5_gatop, regional_estacional_10_gatop)
  
  # Threshold (%10 minimum training presence)
  mtp10 <- sdm_threshold.10_gat(pred, occ_cal[, c('Longitude', 'Latitude')], 'p10', binary = F)
  global_10_gatop <- pred
  global_10_gatop[global_10_gatop < minValue(mtp10)] <- NA
  global_10_gatop[global_10_gatop >= minValue(mtp10)] <- 1
  global_10_gatop <- crop(global_10_gatop, regional_estacional_10_gatop)
} # Global

# Extend, cut, and reproject to the same graph
regional_anual_10_gatop <- extend(regional_anual_10_gatop, regional_estacional_10_cazon)
regional_anual_10_gatop <- crop(regional_anual_10_gatop, regional_estacional_10_cazon)
global_10_gatop <- extend(global_10_gatop, regional_estacional_10_cazon)
global_10_gatop <- crop(global_10_gatop, regional_estacional_10_cazon)
area_nucleo_10_gatop0 <- projectRaster(area_nucleo_10_gatop0, global_10_cazon)
area_nucleo_10_gatop0[area_nucleo_10_gatop0 >= minValue(area_nucleo_10_gatop0)] <- 1
regional_anual_5_gatop <- extend(regional_anual_5_gatop, regional_estacional_5_cazon)
regional_anual_5_gatop <- crop(regional_anual_5_gatop, regional_estacional_5_cazon)
global_5_gatop <- extend(global_5_gatop, regional_estacional_5_cazon)
global_5_gatop <- crop(global_5_gatop, regional_estacional_5_cazon)
area_nucleo_5_gatop0 <- projectRaster(area_nucleo_5_gatop0, global_10_cazon)
area_nucleo_5_gatop0[area_nucleo_5_gatop0 >= minValue(area_nucleo_5_gatop0)] <- 1

# Reproject to the lowest resolution among species
regional_estacional_10_gatop <- projectRaster(regional_estacional_10_gatop, global_10_gatop)
regional_estacional_10_gatop[regional_estacional_10_gatop >= minValue(regional_estacional_10_gatop)] <- 1
regional_anual_10_gatop <- projectRaster(regional_anual_10_gatop, global_10_gatop)
regional_anual_10_gatop[regional_anual_10_gatop >= minValue(regional_anual_10_gatop)] <- 1
regional_estacional_5_gatop <- projectRaster(regional_estacional_5_gatop, global_5_gatop)
regional_estacional_5_gatop[regional_estacional_5_gatop >= minValue(regional_estacional_5_gatop)] <- 1
regional_anual_5_gatop <- projectRaster(regional_anual_5_gatop, global_5_gatop)
regional_anual_5_gatop[regional_anual_5_gatop >= minValue(regional_anual_5_gatop)] <- 1

# Mosaic
mosaico_10_gatop <- mosaic(regional_anual_10_gatop, regional_estacional_10_gatop, global_10_gatop, fun = sum)
round(table(getValues(mosaico_10_gatop))[-1] * 100 / sum(table(getValues(mosaico_10_gatop))[-1]), 1)
round(table(getValues(area_nucleo_10_gatop0)) * 100 / sum(table(getValues(mosaico_10_gatop))[-1], table(getValues(area_nucleo_10_gatop0))), 1)
area_nucleo_10 <- mask(mosaico_10_gatop, area_nucleo_10_gatop0)
round(table(getValues(area_nucleo_10)) * 100 / sum(table(getValues(area_nucleo_10))), 1)
mosaico_5_gatop <- mosaic(regional_anual_5_gatop, regional_estacional_5_gatop, global_5_gatop, fun = sum)
round(table(getValues(mosaico_5_gatop))[-1] * 100 / sum(table(getValues(mosaico_5_gatop))[-1]), 1)
round(table(getValues(area_nucleo_5_gatop0)) * 100 / sum(table(getValues(mosaico_5_gatop))[-1], table(getValues(area_nucleo_5_gatop0))), 1)
area_nucleo_5 <- mask(mosaico_5_gatop, area_nucleo_5_gatop0)
round(table(getValues(area_nucleo_5)) * 100 / sum(table(getValues(area_nucleo_5))), 1)

Col <- c('#000004FF', '#781C6DFF', '#ED6925FF', '#FCFFA4FF')

df_10 <- data.frame(coordinates(mosaico_10_gatop), as.data.frame(mosaico_10_gatop))
ggplot() +
  geom_tile(data = df_10, aes(x = x, y = y, fill = as.factor(layer))) + 
  scale_fill_manual(values = Col, na.value = '#000004FF') + 
  geom_polygon(data = df_rdp, aes(x = x, y = y), color = 'grey95', fill = 'grey95', linewidth = 0.75) +
  geom_polygon(data = df_lagoa, aes(x = x, y = y), color = 'grey95', fill = 'grey95',linewidth = 0.1) +
  geom_polygon(data = coast0, aes(x = long, y = lat, group = group), color = 'grey50', fill = 'white', linewidth = 0.15) +
  geom_polygon(data = area_nucleo_10_gatop, aes(x = long, y = lat, group = group), color = '#000004FF', fill = "#000004FF", alpha = 0.1, linewidth = 0.3) +
  geom_rect(data = df_mar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = '#000004FF') +
  geom_rect(data = df_tierra, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'white') +
  scale_y_continuous(name = NULL, breaks = c(-54, -44, -34, -24), labels = c('54º', '44º', '34º', '24º')) + 
  scale_x_continuous(name = NULL, breaks = c(-66, -58, -50), labels = c('66º', '58º', '50º')) +
  coord_equal(xlim = c(-70, -44), ylim = c(-57, -22), expand = 0) + 
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_blank(), legend.position = 'none',
        panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.35))
ggsave('Figure 3b.tiff', dpi = 900, width = 10.2, height = 10.6, units = 'cm', device = grDevices::tiff)

# Galeorhinus galeus
df_5 <- data.frame(coordinates(mosaico_5_cazon), as.data.frame(mosaico_5_cazon))
ggplot() +
  geom_tile(data = df_5, aes(x = x, y = y, fill = as.factor(layer))) + 
  scale_fill_manual(values = Col, na.value = '#000004FF') + 
  geom_polygon(data = df_rdp, aes(x = x, y = y), color = 'grey95', fill = 'grey95', linewidth = 0.75) +
  geom_polygon(data = df_lagoa, aes(x = x, y = y), color = 'grey95', fill = 'grey95',linewidth = 0.1) +
  geom_polygon(data = coast0, aes(x = long, y = lat, group = group), color = 'grey50', fill = 'white', linewidth = 0.15) +
  geom_polygon(data = area_nucleo_5_cazon, aes(x = long, y = lat, group = group), color = '#000004FF', fill = "#000004FF", alpha = 0.1, linewidth = 0.3) +
  geom_rect(data = df_mar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = '#000004FF') +
  geom_rect(data = df_tierra, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'white') +
  scale_y_continuous(name = NULL, breaks = c(-54, -44, -34, -24), labels = c('54º', '44º', '34º', '24º')) + 
  scale_x_continuous(name = NULL, breaks = c(-66, -58, -50), labels = c('66º', '58º', '50º')) +
  coord_equal(xlim = c(-70, -44), ylim = c(-57, -22), expand = 0) + 
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_blank(), legend.position = 'none',
        panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.35))
ggsave('Figure S1.4a.tiff', dpi = 900, width = 10.2, height = 10.6, units = 'cm', device = grDevices::tiff)

# Notorynchus cepedianus
df_5 <- data.frame(coordinates(mosaico_5_gatop), as.data.frame(mosaico_5_gatop))
ggplot() +
  geom_tile(data = df_5, aes(x = x, y = y, fill = as.factor(layer))) + 
  scale_fill_manual(values = Col, na.value = '#000004FF') + 
  geom_polygon(data = df_rdp, aes(x = x, y = y), color = 'grey95', fill = 'grey95', linewidth = 0.75) +
  geom_polygon(data = df_lagoa, aes(x = x, y = y), color = 'grey95', fill = 'grey95',linewidth = 0.1) +
  geom_polygon(data = coast0, aes(x = long, y = lat, group = group), color = 'grey50', fill = 'white', linewidth = 0.15) +
  geom_polygon(data = area_nucleo_5_gatop, aes(x = long, y = lat, group = group), color = '#000004FF', fill = "#000004FF", alpha = 0.1, linewidth = 0.3) +
  geom_rect(data = df_mar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = '#000004FF') +
  geom_rect(data = df_tierra, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'white') +
  scale_y_continuous(name = NULL, breaks = c(-54, -44, -34, -24), labels = c('54º', '44º', '34º', '24º')) + 
  scale_x_continuous(name = NULL, breaks = c(-66, -58, -50), labels = c('66º', '58º', '50º')) +
  coord_equal(xlim = c(-70, -44), ylim = c(-57, -22), expand = 0) + 
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_blank(), legend.position = 'none',
        panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.35))
ggsave('Figure S1.4b.tiff', dpi = 900, width = 10.2, height = 10.6, units = 'cm', device = grDevices::tiff)


#------------------------------------ Figures 4 $ S1.5 ---------------------------------------

# Overlap in annual predictions of the four species
# Combining regional and global model approaches

# Annual binary habitat suitability by number of species

{
  final_mod0 <- raster('G_galeus_annual.tif')
  final_mod0 <- crop(final_mod0, extent(final_mod0) + c(0.5, 0, 0, 0))
  final_mod <- extend(final_mod0, extent(final_mod0) + 1.75)
  occ_cal <- read.csv('G_galeus_annual.csv')
  
  # Threshold (%5 minimum training presence)
  mtp5 <- sdm_threshold.5(final_mod, occ_cal[, c('longitude', 'latitude')], 'p05', binary = F)
  regional_anual_5_cazon <- final_mod
  regional_anual_5_cazon[regional_anual_5_cazon < minValue(mtp5)] <- NA
  regional_anual_5_cazon[regional_anual_5_cazon >= minValue(mtp5)] <- 1
  
  # Threshold (%10 minimum training presence)
  mtp10 <- sdm_threshold.10(final_mod, occ_cal[, c('longitude', 'latitude')], 'p10', binary = F)
  regional_anual_10_cazon <- final_mod
  regional_anual_10_cazon[regional_anual_10_cazon < minValue(mtp10)] <- NA
  regional_anual_10_cazon[regional_anual_10_cazon >= minValue(mtp10)] <- 1
} # G. galeus - regional annual
{
  # Summer
  final_mod <- raster('G_galeus_summer.tif')
  occ_cal <- read.csv('G_galeus_summer.csv')
  # Threshold (%5 minimum training presence)
  mtp5 <- sdm_threshold.5(final_mod, occ_cal[, c('longitude', 'latitude')], 'p05', binary = F)
  bin_5_summer_cazon <- final_mod
  bin_5_summer_cazon[bin_5_summer_cazon < minValue(mtp5)] <- NA
  bin_5_summer_cazon[bin_5_summer_cazon >= minValue(mtp5)] <- 1
  # Threshold (%10 minimum training presence)
  mtp10 <- sdm_threshold.10(final_mod, occ_cal[, c('longitude', 'latitude')], 'p10', binary = F)
  bin_10_summer_cazon <- final_mod
  bin_10_summer_cazon[bin_10_summer_cazon < minValue(mtp10)] <- NA
  bin_10_summer_cazon[bin_10_summer_cazon >= minValue(mtp10)] <- 1
  
  # Autumn
  final_mod <- raster('G_galeus_autumn.tif') 
  occ_cal <- read.csv('G_galeus_autumn.csv')
  # Threshold (%5 minimum training presence)
  mtp5 <- sdm_threshold.5(final_mod, occ_cal[, c('longitude', 'latitude')], 'p05', binary = F)
  bin_5_autumn_cazon <- final_mod
  bin_5_autumn_cazon[bin_5_autumn_cazon < minValue(mtp5)] <- NA
  bin_5_autumn_cazon[bin_5_autumn_cazon >= minValue(mtp5)] <- 1
  # Threshold (%10 minimum training presence)
  mtp10 <- sdm_threshold.10(final_mod, occ_cal[, c('longitude', 'latitude')], 'p10', binary = F)
  bin_10_autumn_cazon <- final_mod
  bin_10_autumn_cazon[bin_10_autumn_cazon < minValue(mtp10)] <- NA
  bin_10_autumn_cazon[bin_10_autumn_cazon >= minValue(mtp10)] <- 1
  
  # Winter
  final_mod <- raster('G_galeus_winter.tif') 
  occ_cal <- read.csv('G_galeus_winter.csv')
  # Threshold (%5 minimum training presence)
  mtp5 <- sdm_threshold.5(final_mod, occ_cal[, c('longitude', 'latitude')], 'p05', binary = F)
  bin_5_winter_cazon <- final_mod
  bin_5_winter_cazon[bin_5_winter_cazon < minValue(mtp5)] <- NA
  bin_5_winter_cazon[bin_5_winter_cazon >= minValue(mtp5)] <- 1
  # Threshold (%10 minimum training presence)
  mtp10 <- sdm_threshold.10(final_mod, occ_cal[, c('longitude', 'latitude')], 'p10', binary = F)
  bin_10_winter_cazon <- final_mod
  bin_10_winter_cazon[bin_10_winter_cazon < minValue(mtp10)] <- NA
  bin_10_winter_cazon[bin_10_winter_cazon >= minValue(mtp10)] <- 1
  
  # Spring
  final_mod <- raster('G_galeus_spring.tif')
  occ_cal <- read.csv('G_galeus_spring.csv')
  # Threshold (%5 minimum training presence)
  mtp5 <- sdm_threshold.5(final_mod, occ_cal[, c('longitude', 'latitude')], 'p05', binary = F)
  bin_5_spring_cazon <- final_mod
  bin_5_spring_cazon[bin_5_spring_cazon < minValue(mtp5)] <- NA
  bin_5_spring_cazon[bin_5_spring_cazon >= minValue(mtp5)] <- 1
  # Threshold (%10 minimum training presence)
  mtp10 <- sdm_threshold.10(final_mod, occ_cal[, c('longitude', 'latitude')], 'p10', binary = F)
  bin_10_spring_cazon <- final_mod
  bin_10_spring_cazon[bin_10_spring_cazon < minValue(mtp10)] <- NA
  bin_10_spring_cazon[bin_10_spring_cazon >= minValue(mtp10)] <- 1
  
  # Mosaic 10% MTP
  regional_estacional_10_cazon <- mosaic(bin_10_summer_cazon, bin_10_autumn_cazon, bin_10_winter_cazon, bin_10_spring_cazon, fun = sum)
  regional_estacional_10_cazon[regional_estacional_10_cazon < 1] <- NA
  area_nucleo_10_cazon <- regional_estacional_10_cazon
  area_nucleo_10_cazon[area_nucleo_10_cazon < 4] <- NA
  area_nucleo_10_cazon <- as.polygons(rast(area_nucleo_10_cazon))
  area_nucleo_10_cazon <- as(area_nucleo_10_cazon, "Spatial")
  regional_estacional_10_cazon[regional_estacional_10_cazon < 4] <- NA
  
  # Mosaic 5% MTP
  regional_estacional_5_cazon <- mosaic(bin_5_summer_cazon, bin_5_autumn_cazon, bin_5_winter_cazon, bin_5_spring_cazon, fun = sum)
  regional_estacional_5_cazon[regional_estacional_5_cazon < 1] <- NA
  area_nucleo_5_cazon <- regional_estacional_5_cazon
  area_nucleo_5_cazon[area_nucleo_5_cazon < 4] <- NA
  area_nucleo_5_cazon <- as.polygons(rast(area_nucleo_5_cazon))
  area_nucleo_5_cazon <- as(area_nucleo_5_cazon, "Spatial")
  regional_estacional_5_cazon[regional_estacional_5_cazon < 4] <- NA
} # G. galeus - regional seasonal
{
  pred <- raster('G_galeus_global_Predictions.tif')
  occ_cal <- read.csv('G_galeus_global.csv') 
  sdm_threshold.5_caz <- function(sdm, occs, type = 'mtp', binary = FALSE){
    occPredVals <- raster::extract(sdm, occs)
    if(type == 'mtp'){
      thresh <- min(na.omit(occPredVals))
    } else if(type == 'p05'){
      if(length(occPredVals) < 10){
        p05 <- floor(length(occPredVals) * 0.8)
      } else {
        p05 <- ceiling(length(occPredVals) * 0.8)
      }
      thresh <- rev(sort(occPredVals))[p05]
    }
    sdm_thresh <- sdm
    sdm_thresh[sdm_thresh < thresh] <- NA
    if(binary){
      sdm_thresh[sdm_thresh >= thresh] <- 1
    }
    return(sdm_thresh)
  } # MTP5
  sdm_threshold.10_caz <- function(sdm, occs, type = 'mtp', binary = FALSE){
    occPredVals <- raster::extract(sdm, occs)
    if(type == 'mtp'){
      thresh <- min(na.omit(occPredVals))
    } else if(type == 'p10'){
      if(length(occPredVals) < 10){
        p10 <- floor(length(occPredVals) * 0.75)
      } else {
        p10 <- ceiling(length(occPredVals) * 0.75)
      }
      thresh <- rev(sort(occPredVals))[p10]
    }
    sdm_thresh <- sdm
    sdm_thresh[sdm_thresh < thresh] <- NA
    if(binary){
      sdm_thresh[sdm_thresh >= thresh] <- 1
    }
    return(sdm_thresh)
  } # MTP10
  
  # Threshold (%5 minimum training presence)
  mtp5 <- sdm_threshold.5_caz(pred, occ_cal[, c('Longitude', 'Latitude')], 'p05', binary = F)
  global_5_cazon <- pred
  global_5_cazon[global_5_cazon < minValue(mtp5)] <- NA
  global_5_cazon[global_5_cazon >= minValue(mtp5)] <- 1
  global_5_cazon <- crop(global_5_cazon, regional_estacional_10_cazon)
  
  # Threshold (%10 minimum training presence)
  mtp10 <- sdm_threshold.10_caz(pred, occ_cal[, c('Longitude', 'Latitude')], 'p10', binary = F)
  global_10_cazon <- pred
  global_10_cazon[global_10_cazon < minValue(mtp10)] <- NA
  global_10_cazon[global_10_cazon >= minValue(mtp10)] <- 1
  global_10_cazon <- crop(global_10_cazon, regional_estacional_10_cazon)
} # G. galeus - global
{
  final_mod0 <- raster('N_cepedianus_annual.tif')
  final_mod0 <- crop(final_mod0, extent(final_mod0) + c(0.5, 0, 0, 0))
  final_mod <- extend(final_mod0, extent(final_mod0) + 1.75)
  occ_cal <- read.csv('N_cepedianus_annual.csv')
  
  # Threshold (%5 minimum training presence)
  mtp5 <- sdm_threshold.5(final_mod, occ_cal[, c('longitude', 'latitude')], 'p05', binary = F)
  regional_anual_5_gatop <- final_mod
  regional_anual_5_gatop[regional_anual_5_gatop < minValue(mtp5)] <- NA
  regional_anual_5_gatop[regional_anual_5_gatop >= minValue(mtp5)] <- 1
  
  # Threshold (%10 minimum training presence)
  mtp10 <- sdm_threshold.10(final_mod, occ_cal[, c('longitude', 'latitude')], 'p10', binary = F)
  regional_anual_10_gatop <- final_mod
  regional_anual_10_gatop[regional_anual_10_gatop < minValue(mtp10)] <- NA
  regional_anual_10_gatop[regional_anual_10_gatop >= minValue(mtp10)] <- 1
} # N. cepedianus - regional annual
{
  # Summer
  final_mod <- raster('N_cepedianus_summer.tif')
  occ_cal <- read.csv('N_cepedianus_summer.csv')
  # Threshold (%5 minimum training presence)
  mtp5 <- sdm_threshold.5(final_mod, occ_cal[, c('longitude', 'latitude')], 'p05', binary = F)
  bin_5_summer_gatop <- final_mod
  bin_5_summer_gatop[bin_5_summer_gatop < minValue(mtp5)] <- NA
  bin_5_summer_gatop[bin_5_summer_gatop >= minValue(mtp5)] <- 1
  # Threshold (%10 minimum training presence)
  mtp10 <- sdm_threshold.10(final_mod, occ_cal[, c('longitude', 'latitude')], 'p10', binary = F)
  bin_10_summer_gatop <- final_mod
  bin_10_summer_gatop[bin_10_summer_gatop < minValue(mtp10)] <- NA
  bin_10_summer_gatop[bin_10_summer_gatop >= minValue(mtp10)] <- 1
  
  # Autumn
  final_mod <- raster('N_cepedianus_autumn.tif') 
  occ_cal <- read.csv('N_cepedianus_autumn.csv')
  # Threshold (%5 minimum training presence)
  mtp5 <- sdm_threshold.5(final_mod, occ_cal[, c('longitude', 'latitude')], 'p05', binary = F)
  bin_5_autumn_gatop <- final_mod
  bin_5_autumn_gatop[bin_5_autumn_gatop < minValue(mtp5)] <- NA
  bin_5_autumn_gatop[bin_5_autumn_gatop >= minValue(mtp5)] <- 1
  # Threshold (%10 minimum training presence)
  mtp10 <- sdm_threshold.10(final_mod, occ_cal[, c('longitude', 'latitude')], 'p10', binary = F)
  bin_10_autumn_gatop <- final_mod
  bin_10_autumn_gatop[bin_10_autumn_gatop < minValue(mtp10)] <- NA
  bin_10_autumn_gatop[bin_10_autumn_gatop >= minValue(mtp10)] <- 1
  
  # Winter
  final_mod <- raster('N_cepedianus_winter.tif') 
  occ_cal <- read.csv('N_cepedianus_winter.csv')
  # Threshold (%5 minimum training presence)
  mtp5 <- sdm_threshold.5(final_mod, occ_cal[, c('longitude', 'latitude')], 'p05', binary = F)
  bin_5_winter_gatop <- final_mod
  bin_5_winter_gatop[bin_5_winter_gatop < minValue(mtp5)] <- NA
  bin_5_winter_gatop[bin_5_winter_gatop >= minValue(mtp5)] <- 1
  # Threshold (%10 minimum training presence)
  mtp10 <- sdm_threshold.10(final_mod, occ_cal[, c('longitude', 'latitude')], 'p10', binary = F)
  bin_10_winter_gatop <- final_mod
  bin_10_winter_gatop[bin_10_winter_gatop < minValue(mtp10)] <- NA
  bin_10_winter_gatop[bin_10_winter_gatop >= minValue(mtp10)] <- 1
  
  # Spring
  final_mod <- raster('N_cepedianus_spring.tif')
  occ_cal <- read.csv('N_cepedianus_spring.csv')
  # Threshold (%5 minimum training presence)
  mtp5 <- sdm_threshold.5(final_mod, occ_cal[, c('longitude', 'latitude')], 'p05', binary = F)
  bin_5_spring_gatop <- final_mod
  bin_5_spring_gatop[bin_5_spring_gatop < minValue(mtp5)] <- NA
  bin_5_spring_gatop[bin_5_spring_gatop >= minValue(mtp5)] <- 1
  # Threshold (%10 minimum training presence)
  mtp10 <- sdm_threshold.10(final_mod, occ_cal[, c('longitude', 'latitude')], 'p10', binary = F)
  bin_10_spring_gatop <- final_mod
  bin_10_spring_gatop[bin_10_spring_gatop < minValue(mtp10)] <- NA
  bin_10_spring_gatop[bin_10_spring_gatop >= minValue(mtp10)] <- 1
  
  # Mosaic 10% MTP
  regional_estacional_10_gatop <- mosaic(bin_10_summer_gatop, bin_10_autumn_gatop, bin_10_winter_gatop, bin_10_spring_gatop, fun = sum)
  regional_estacional_10_gatop[regional_estacional_10_gatop < 1] <- NA
  area_nucleo_10_gatop <- regional_estacional_10_gatop
  area_nucleo_10_gatop[area_nucleo_10_gatop < 4] <- NA
  area_nucleo_10_gatop <- as.polygons(rast(area_nucleo_10_gatop))
  area_nucleo_10_gatop <- as(area_nucleo_10_gatop, "Spatial")
  regional_estacional_10_gatop[regional_estacional_10_gatop < 4] <- NA
  
  # Mosaic 5% MTP
  regional_estacional_5_gatop <- mosaic(bin_5_summer_gatop, bin_5_autumn_gatop, bin_5_winter_gatop, bin_5_spring_gatop, fun = sum)
  regional_estacional_5_gatop[regional_estacional_5_gatop < 1] <- NA
  area_nucleo_5_gatop <- regional_estacional_5_gatop
  area_nucleo_5_gatop[area_nucleo_5_gatop < 4] <- NA
  area_nucleo_5_gatop <- as.polygons(rast(area_nucleo_5_gatop))
  area_nucleo_5_gatop <- as(area_nucleo_5_gatop, "Spatial")
  regional_estacional_5_gatop[regional_estacional_5_gatop < 4] <- NA
} # N. cepedianus - regional seasonal
{
  pred <- raster('N_cepedianus_global_Predictions.tif')
  occ_cal <- read.csv('N_cepedianus_global.csv') 
  sdm_threshold.5_gat <- function(sdm, occs, type = 'mtp', binary = FALSE){
    occPredVals <- raster::extract(sdm, occs)
    if(type == 'mtp'){
      thresh <- min(na.omit(occPredVals))
    } else if(type == 'p05'){
      if(length(occPredVals) < 10){
        p05 <- floor(length(occPredVals) * 0.85)
      } else {
        p05 <- ceiling(length(occPredVals) * 0.85)
      }
      thresh <- rev(sort(occPredVals))[p05]
    }
    sdm_thresh <- sdm
    sdm_thresh[sdm_thresh < thresh] <- NA
    if(binary){
      sdm_thresh[sdm_thresh >= thresh] <- 1
    }
    return(sdm_thresh)
  } # MTP5
  sdm_threshold.10_gat <- function(sdm, occs, type = 'mtp', binary = FALSE){
    occPredVals <- raster::extract(sdm, occs)
    if(type == 'mtp'){
      thresh <- min(na.omit(occPredVals))
    } else if(type == 'p10'){
      if(length(occPredVals) < 10){
        p10 <- floor(length(occPredVals) * 0.8)
      } else {
        p10 <- ceiling(length(occPredVals) * 0.8)
      }
      thresh <- rev(sort(occPredVals))[p10]
    }
    sdm_thresh <- sdm
    sdm_thresh[sdm_thresh < thresh] <- NA
    if(binary){
      sdm_thresh[sdm_thresh >= thresh] <- 1
    }
    return(sdm_thresh)
  } # MTP10
  
  # Threshold (%5 minimum training presence)
  mtp5 <- sdm_threshold.5_gat(pred, occ_cal[, c('Longitude', 'Latitude')], 'p05', binary = F)
  global_5_gatop <- pred
  global_5_gatop[global_5_gatop < minValue(mtp5)] <- NA
  global_5_gatop[global_5_gatop >= minValue(mtp5)] <- 1
  global_5_gatop <- crop(global_5_gatop, regional_estacional_10_gatop)
  
  # Threshold (%10 minimum training presence)
  mtp10 <- sdm_threshold.10_gat(pred, occ_cal[, c('Longitude', 'Latitude')], 'p10', binary = F)
  global_10_gatop <- pred
  global_10_gatop[global_10_gatop < minValue(mtp10)] <- NA
  global_10_gatop[global_10_gatop >= minValue(mtp10)] <- 1
  global_10_gatop <- crop(global_10_gatop, regional_estacional_10_gatop)
} # N. cepedianus - global
{
  # C. brachyurus
  final_mod <- raster('C_brachyurus_global_Predictions.tif')
  occ_cal <- read.csv('C_brachyurus_global.csv') 
  swa_ext <- extent(-68, -39, -48, -19)
  # Threshold (%5 minimum training presence)
  mtp5 <- sdm_threshold.5(final_mod, occ_cal[, c('Longitude', 'Latitude')], 'p05', binary = F)
  global_5_bacot <- final_mod
  global_5_bacot[global_5_bacot < minValue(mtp5)] <- NA
  global_5_bacot[global_5_bacot >= minValue(mtp5)] <- 1
  global_5_bacot <- crop(global_5_bacot, swa_ext)
  # Threshold (%10 minimum training presence)
  mtp10 <- sdm_threshold.10(final_mod, occ_cal[, c('Longitude', 'Latitude')], 'p10', binary = F)
  global_10_bacot <- final_mod
  global_10_bacot[global_10_bacot < minValue(mtp10)] <- NA
  global_10_bacot[global_10_bacot >= minValue(mtp10)] <- 1
  global_10_bacot <- crop(global_10_bacot, swa_ext)
} # C. brachyurus - global
{
  # C. taurus
  final_mod <- raster('C_taurus_global_Predictions.tif')
  occ_cal <- read.csv('C_taurus_global.csv') 
  swa_ext <- extent(-68, -37, -46, -12)
  # Threshold (%5 minimum training presence)
  sdm_threshold.5_taurus <- function(sdm, occs, type = 'mtp', binary = FALSE){
    occPredVals <- raster::extract(sdm, occs)
    if(type == 'mtp'){
      thresh <- min(na.omit(occPredVals))
    } else if(type == 'p05'){
      if(length(occPredVals) < 10){
        p05 <- floor(length(occPredVals) * 0.8)
      } else {
        p05 <- ceiling(length(occPredVals) * 0.8)
      }
      thresh <- rev(sort(occPredVals))[p05]
    }
    sdm_thresh <- sdm
    sdm_thresh[sdm_thresh < thresh] <- NA
    if(binary){
      sdm_thresh[sdm_thresh >= thresh] <- 1
    }
    return(sdm_thresh)
  } # MTP5
  mtp5 <- sdm_threshold.5_taurus(final_mod, occ_cal[, c('Longitude', 'Latitude')], 'p05', binary = F)
  global_5_escal <- final_mod
  global_5_escal[global_5_escal < minValue(mtp5)] <- NA
  global_5_escal[global_5_escal >= minValue(mtp5)] <- 1
  global_5_escal <- crop(global_5_escal, swa_ext)
  # Threshold (%10 minimum training presence)
  sdm_threshold.10_taurus <- function(sdm, occs, type = 'mtp', binary = FALSE){
    occPredVals <- raster::extract(sdm, occs)
    if(type == 'mtp'){
      thresh <- min(na.omit(occPredVals))
    } else if(type == 'p10'){
      if(length(occPredVals) < 10){
        p10 <- floor(length(occPredVals) * 0.7)
      } else {
        p10 <- ceiling(length(occPredVals) * 0.7)
      }
      thresh <- rev(sort(occPredVals))[p10]
    }
    sdm_thresh <- sdm
    sdm_thresh[sdm_thresh < thresh] <- NA
    if(binary){
      sdm_thresh[sdm_thresh >= thresh] <- 1
    }
    return(sdm_thresh)
  } # MTP10
  mtp10 <- sdm_threshold.10_taurus(final_mod, occ_cal[, c('Longitude', 'Latitude')], 'p10', binary = F)
  global_10_escal <- final_mod
  global_10_escal[global_10_escal < minValue(mtp10)] <- NA
  global_10_escal[global_10_escal >= minValue(mtp10)] <- 1
  global_10_escal <- crop(global_10_escal, swa_ext)
} # C. taurus - global

# Extend dimensions among species to accommodate everything
global_10_escal <- extend(global_10_escal, regional_anual_10_cazon)# C. taurus as model
global_10_escal <- extend(global_10_escal, regional_estacional_10_cazon)# C. taurus as model
regional_anual_10_cazon <- extend(regional_anual_10_cazon, global_10_escal)
regional_estacional_10_cazon <- extend(regional_estacional_10_cazon, global_10_escal)
regional_anual_10_gatop <- extend(regional_anual_10_gatop, global_10_escal)
regional_estacional_10_gatop <- extend(regional_estacional_10_gatop, global_10_escal)
global_10_bacot <- extend(global_10_bacot, global_10_escal)
global_10_cazon <- extend(global_10_cazon, global_10_escal)
global_10_gatop <- extend(global_10_gatop, global_10_escal)
global_5_escal <- extend(global_5_escal, regional_anual_5_cazon)# C. taurus as model
global_5_escal <- extend(global_5_escal, regional_estacional_5_cazon)# C. taurus as model
regional_anual_5_cazon <- extend(regional_anual_5_cazon, global_5_escal)
regional_estacional_5_cazon <- extend(regional_estacional_5_cazon, global_5_escal)
regional_anual_5_gatop <- extend(regional_anual_5_gatop, global_5_escal)
regional_estacional_5_gatop <- extend(regional_estacional_5_gatop, global_5_escal)
global_5_bacot <- extend(global_5_bacot, global_5_escal)
global_5_cazon <- extend(global_5_cazon, global_10_escal)
global_5_gatop <- extend(global_5_gatop, global_10_escal)

# Reproject to the lowest resolution among species
regional_anual_10_cazon <- projectRaster(regional_anual_10_cazon, global_10_escal)
regional_anual_10_cazon[regional_anual_10_cazon >= minValue(regional_anual_10_cazon)] <- 1
regional_estacional_10_cazon <- projectRaster(regional_estacional_10_cazon, global_10_escal)
regional_estacional_10_cazon[regional_estacional_10_cazon >= minValue(regional_estacional_10_cazon)] <- 1
regional_anual_10_gatop <- projectRaster(regional_anual_10_gatop, global_10_escal)
regional_anual_10_gatop[regional_anual_10_gatop >= minValue(regional_anual_10_gatop)] <- 1
regional_estacional_10_gatop <- projectRaster(regional_estacional_10_gatop, global_10_escal)
regional_estacional_10_gatop[regional_estacional_10_gatop >= minValue(regional_estacional_10_gatop)] <- 1
global_10_cazon <- projectRaster(global_10_cazon, global_10_escal)
global_10_cazon[global_10_cazon >= minValue(global_10_cazon)] <- 1
global_10_gatop <- projectRaster(global_10_gatop, global_10_escal)
global_10_gatop[global_10_gatop >= minValue(global_10_gatop)] <- 1
regional_anual_5_cazon <- projectRaster(regional_anual_5_cazon, global_5_escal)
regional_anual_5_cazon[regional_anual_5_cazon >= minValue(regional_anual_5_cazon)] <- 1
regional_estacional_5_cazon <- projectRaster(regional_estacional_5_cazon, global_5_escal)
regional_estacional_5_cazon[regional_estacional_5_cazon >= minValue(regional_estacional_5_cazon)] <- 1
regional_anual_5_gatop <- projectRaster(regional_anual_5_gatop, global_5_escal)
regional_anual_5_gatop[regional_anual_5_gatop >= minValue(regional_anual_5_gatop)] <- 1
regional_estacional_5_gatop <- projectRaster(regional_estacional_5_gatop, global_5_escal)
regional_estacional_5_gatop[regional_estacional_5_gatop >= minValue(regional_estacional_5_gatop)] <- 1
global_5_cazon <- projectRaster(global_5_cazon, global_5_escal)
global_5_cazon[global_5_cazon >= minValue(global_5_cazon)] <- 1
global_5_gatop <- projectRaster(global_5_gatop, global_5_escal)
global_5_gatop[global_5_gatop >= minValue(global_5_gatop)] <- 1

# Overlap between species

# Annual (G. galeus) 
# Annual (N. cepedianus)
# Global (C. brachyurus) 
# Global (C. taurus)
anual_global_10 <- mosaic(regional_anual_10_cazon, regional_anual_10_gatop, global_10_bacot, global_10_escal, fun = sum)
round(table(getValues(anual_global_10))[-1] * 100 / sum(table(getValues(anual_global_10))[-1]), 1)
anual_global_5 <- mosaic(regional_anual_5_cazon, regional_anual_5_gatop, global_5_bacot, global_5_escal, fun = sum)
round(table(getValues(anual_global_5))[-1] * 100 / sum(table(getValues(anual_global_5))[-1]), 1)

# Plot
Col <- c('#000004FF', '#56106EFF', '#BB3754FF', '#F98C0AFF', '#FCFFA4FF')

df_10 <- data.frame(coordinates(anual_global_10), as.data.frame(anual_global_10))
ggplot() +
  geom_tile(data = df_10, aes(x = x, y = y, fill = as.factor(layer))) + 
  scale_fill_manual(values = Col, na.value = '#000004FF') + 
  geom_polygon(data = df_rdp, aes(x = x, y = y), color = 'grey95', fill = 'grey95', linewidth = 0.75) +
  geom_polygon(data = df_lagoa, aes(x = x, y = y), color = 'grey95', fill = 'grey95',linewidth = 0.1) +
  geom_polygon(data = coast0, aes(x = long, y = lat, group = group), color = 'grey50', fill = 'white', linewidth = 0.15) +
  geom_rect(data = df_mar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = '#000004FF') +
  geom_rect(data = df_tierra, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'white') +
  scale_y_continuous(name = NULL, breaks = c(-54, -44, -34, -24, -14), labels = c('54º', '44º', '34º', '24º', '14º')) + 
  scale_x_continuous(name = NULL, breaks = c(-66, -58, -50, -42), labels = c('66º', '58º', '50º', '42º')) +
  coord_equal(xlim = c(-70, -37), ylim = c(-59, -12), expand = 0) + 
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_blank(), legend.position = 'none',
        panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.35))
ggsave('Figure 4a.tiff', dpi = 900, width = 10.2, height = 10.6, units = 'cm', device = grDevices::tiff)

# Seasonal (G. galeus) 
# Seasonal (N. cepedianus)
# Global (C. brachyurus) 
# Global (C. taurus)
estacional_global_10 <- mosaic(regional_estacional_10_cazon, regional_estacional_10_gatop, global_10_bacot, global_10_escal, fun = sum)
round(table(getValues(estacional_global_10))[-1] * 100 / sum(table(getValues(estacional_global_10))[-1]), 1)
estacional_global_5 <- mosaic(regional_estacional_5_cazon, regional_estacional_5_gatop, global_5_bacot, global_5_escal, fun = sum)
round(table(getValues(estacional_global_5))[-1] * 100 / sum(table(getValues(estacional_global_5))[-1]), 1)

# Plot
Col <- c('#000004FF', '#56106EFF', '#BB3754FF', '#F98C0AFF', '#FCFFA4FF')

df_10 <- data.frame(coordinates(estacional_global_10), as.data.frame(estacional_global_10))
ggplot() +
  geom_tile(data = df_10, aes(x = x, y = y, fill = as.factor(layer))) + 
  scale_fill_manual(values = Col, na.value = '#000004FF') + 
  geom_polygon(data = df_rdp, aes(x = x, y = y), color = 'grey95', fill = 'grey95', linewidth = 0.75) +
  geom_polygon(data = df_lagoa, aes(x = x, y = y), color = 'grey95', fill = 'grey95',linewidth = 0.1) +
  geom_polygon(data = coast0, aes(x = long, y = lat, group = group), color = 'grey50', fill = 'white', linewidth = 0.15) +
  geom_rect(data = df_mar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = '#000004FF') +
  geom_rect(data = df_tierra, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'white') +
  geom_path(data = Border[which(Border$id=="1143"),], aes(x = long, y = lat, group = group), color = 'grey50', linewidth = 0.15) +
  scale_y_continuous(name = NULL, breaks = c(-54, -44, -34, -24, -14), labels = c('54º', '44º', '34º', '24º', '14º')) + 
  scale_x_continuous(name = NULL, breaks = c(-66, -58, -50, -42), labels = c('66º', '58º', '50º', '42º')) +
  coord_equal(xlim = c(-70, -37), ylim = c(-59, -12), expand = 0) + 
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_blank(), legend.position = 'none',
        panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.35))
ggsave('Figure 4b.tiff', dpi = 900, width = 10.2, height = 10.6, units = 'cm', device = grDevices::tiff)

# Global (G. galeus) 
# Global (N. cepedianus)
# Global (C. brachyurus) 
# Global (C. taurus)
global_global_10 <- mosaic(global_10_cazon, global_10_gatop, global_10_bacot, global_10_escal, fun = sum)
round(table(getValues(global_global_10))[-1] * 100 / sum(table(getValues(global_global_10))[-1]), 1)
global_global_5 <- mosaic(global_5_cazon, global_5_gatop, global_5_bacot, global_5_escal, fun = sum)
round(table(getValues(global_global_5))[-1] * 100 / sum(table(getValues(global_global_5))[-1]), 1)

# Plot
Col <- c('#000004FF', '#56106EFF', '#BB3754FF', '#F98C0AFF', '#FCFFA4FF')

df_10 <- data.frame(coordinates(global_global_10), as.data.frame(global_global_10))
ggplot() +
  geom_tile(data = df_10, aes(x = x, y = y, fill = as.factor(layer))) + 
  scale_fill_manual(values = Col, na.value = '#000004FF') + 
  geom_polygon(data = df_rdp, aes(x = x, y = y), color = 'grey95', fill = 'grey95', linewidth = 0.75) +
  geom_polygon(data = df_lagoa, aes(x = x, y = y), color = 'grey95', fill = 'grey95',linewidth = 0.1) +
  geom_polygon(data = coast0, aes(x = long, y = lat, group = group), color = 'grey50', fill = 'white', linewidth = 0.15) +
  geom_rect(data = df_mar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = '#000004FF') +
  geom_rect(data = df_tierra, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'white') +
  scale_y_continuous(name = NULL, breaks = c(-54, -44, -34, -24, -14), labels = c('54º', '44º', '34º', '24º', '14º')) + 
  scale_x_continuous(name = NULL, breaks = c(-66, -58, -50, -42), labels = c('66º', '58º', '50º', '42º')) +
  coord_equal(xlim = c(-70, -37), ylim = c(-59, -12), expand = 0) + 
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_blank(), legend.position = 'none',
        panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.35))
ggsave('Figure 4c.tiff', dpi = 900, width = 10.2, height = 10.6, units = 'cm', device = grDevices::tiff)

df_5 <- data.frame(coordinates(anual_global_5), as.data.frame(anual_global_5))
ggplot() +
  geom_tile(data = df_5, aes(x = x, y = y, fill = as.factor(layer))) + 
  scale_fill_manual(values = Col, na.value = '#000004FF') + 
  geom_polygon(data = df_rdp, aes(x = x, y = y), color = 'grey95', fill = 'grey95', linewidth = 0.75) +
  geom_polygon(data = df_lagoa, aes(x = x, y = y), color = 'grey95', fill = 'grey95',linewidth = 0.1) +
  geom_polygon(data = coast0, aes(x = long, y = lat, group = group), color = 'grey50', fill = 'white', linewidth = 0.15) +
  geom_rect(data = df_mar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = '#000004FF') +
  geom_rect(data = df_tierra, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'white') +
  scale_y_continuous(name = NULL, breaks = c(-54, -44, -34, -24, -14), labels = c('54º', '44º', '34º', '24º', '14º')) + 
  scale_x_continuous(name = NULL, breaks = c(-66, -58, -50, -42), labels = c('66º', '58º', '50º', '42º')) +
  coord_equal(xlim = c(-70, -37), ylim = c(-59, -12), expand = 0) + 
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_blank(), legend.position = 'none',
        panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.35))
ggsave('Figure S1.5a.tiff', dpi = 900, width = 10.2, height = 10.6, units = 'cm', device = grDevices::tiff)

df_5 <- data.frame(coordinates(estacional_global_5), as.data.frame(estacional_global_5))
ggplot() +
  geom_tile(data = df_5, aes(x = x, y = y, fill = as.factor(layer))) + 
  scale_fill_manual(values = Col, na.value = '#000004FF') + 
  geom_polygon(data = df_rdp, aes(x = x, y = y), color = 'grey95', fill = 'grey95', linewidth = 0.75) +
  geom_polygon(data = df_lagoa, aes(x = x, y = y), color = 'grey95', fill = 'grey95',linewidth = 0.1) +
  geom_polygon(data = coast0, aes(x = long, y = lat, group = group), color = 'grey50', fill = 'white', linewidth = 0.15) +
  geom_rect(data = df_mar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = '#000004FF') +
  geom_rect(data = df_tierra, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'white') +
  scale_y_continuous(name = NULL, breaks = c(-54, -44, -34, -24, -14), labels = c('54º', '44º', '34º', '24º', '14º')) + 
  scale_x_continuous(name = NULL, breaks = c(-66, -58, -50, -42), labels = c('66º', '58º', '50º', '42º')) +
  coord_equal(xlim = c(-70, -37), ylim = c(-59, -12), expand = 0) + 
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_blank(), legend.position = 'none',
        panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.35))
ggsave('Figure S1.5b.tiff', dpi = 900, width = 10.2, height = 10.6, units = 'cm', device = grDevices::tiff)

df_5 <- data.frame(coordinates(global_global_5), as.data.frame(global_global_5))
ggplot() +
  geom_tile(data = df_5, aes(x = x, y = y, fill = as.factor(layer))) + 
  scale_fill_manual(values = Col, na.value = '#000004FF') + 
  geom_polygon(data = df_rdp, aes(x = x, y = y), color = 'grey95', fill = 'grey95', linewidth = 0.75) +
  geom_polygon(data = df_lagoa, aes(x = x, y = y), color = 'grey95', fill = 'grey95',linewidth = 0.1) +
  geom_polygon(data = coast0, aes(x = long, y = lat, group = group), color = 'grey50', fill = 'white', linewidth = 0.15) +
  geom_rect(data = df_mar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = '#000004FF') +
  geom_rect(data = df_tierra, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'white') +
  scale_y_continuous(name = NULL, breaks = c(-54, -44, -34, -24, -14), labels = c('54º', '44º', '34º', '24º', '14º')) + 
  scale_x_continuous(name = NULL, breaks = c(-66, -58, -50, -42), labels = c('66º', '58º', '50º', '42º')) +
  coord_equal(xlim = c(-70, -37), ylim = c(-59, -12), expand = 0) + 
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_blank(), legend.position = 'none',
        panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.35))
ggsave('Figure S1.5c.tiff', dpi = 900, width = 10.2, height = 10.6, units = 'cm', device = grDevices::tiff)

#------------------------------------ Figure 5 ---------------------------------------

# Occurrences of recreational fishing collected by species

# Occurrences
dat0 <- read.csv('Free_occurrence_records.csv')
dat0 <- subset(dat0, Region == 'Southwest Atlantic')
dat1 <- subset(dat0, Source1 == 'Social media')
dat2 <- subset(dat0, Source2 %in% c("Dr Marina Coller", "Cuevas (2015)", "Uruguay Aventura No 41", 
                                    "Uruguay Aventura No 5", "Uruguay Aventura No 42"))
dat2 <- subset(dat2, Fishing_gear == "Rod and reel")
dat3 <- subset(dat0, Source2 %in% c("Dr Paula Cedrola"))
dat3 <- dat3[sample(nrow(dat3), 60), ]
dat4 <- subset(dat0, Source1 == 'Published literature')
dat4 <- subset(dat4, Source2 %in% c("Lucifora et al. (2005)", "Lucifora et al. (2002)", 
                                    "Lucifora et al. (2004)", "Bovcon et al. (2018)"))
dat <- rbind(dat1, dat2, dat3, dat4)
dat$Presence <- 1
dat <- aggregate(Presence ~ Species + Longitude + Latitude, data = dat, sum)
dat <- transform(dat, Species = factor(Species, levels = c('Galeorhinus galeus', 'Notorynchus cepedianus', 'Carcharhinus brachyurus', 'Carcharias taurus')))
coast <- crop(coast0, extent(c(-70, -50, -54, -30)))

# Provincial boundaries
provincias <- 'DOWNLOAD Political borders'
provincias <- readOGR(dsn = provincias, layer = 'provincias_argentina')
provincias <- crop(provincias, extent(c(-70, -50, -54, -30)))
limite_internacional <- crop(Border, extent(c(-70, -50, -54, -30)))

ggplot() +
  geom_polygon(data = coast, aes(x = long, y = lat, group = group), color = 'grey50', fill = 'white', linewidth = 0.01) +
  geom_path(data = provincias, aes(x = long, y = lat, group = group), color = 'grey70', linewidth = 0.15, linetype = "dashed") +
  geom_path(data = limite_internacional, aes(x = long, y = lat, group = group), color = 'grey50', linewidth = 0.15) +
  geom_point(data = dat, aes(x = Longitude, y = Latitude, size = Presence, color = Presence)) +
  scale_size_continuous(range = c(1, 7.5)) +
  scale_color_viridis(option = "B", trans = "log") + 
  scale_y_continuous(name = 'Latitude (S)', breaks = c(-52, -42, -32), labels = c('52º', '42º', '32º')) + 
  scale_x_continuous(name = 'Longitude (W)', breaks = c(-68, -60, -52), labels = c('68º', '60º', '52º')) +
  facet_wrap(~ Species, ncol = 2) +
  coord_equal(expand = 0) + 
  theme(panel.background = element_rect(fill = 'grey95'),
        panel.grid = element_blank(),
        panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.25))
ggsave('Figure 5.pdf', width = 15, height = 15, units = 'cm')


#------------------------------------ Figure 6 ---------------------------------------

# Occurrences of neonates collected by species

# Occurrences
dat <- read.csv('Free_occurrence_records.csv')
dat <- subset(dat, Region == 'Southwest Atlantic')
dat <- subset(dat, Stage == 'neonate')
dat_caz <- subset(dat, Species == 'Galeorhinus galeus')
dat_gat <- subset(dat, Species == 'Notorynchus cepedianus')
dat_bac <- subset(dat, Species == 'Carcharhinus brachyurus')
dat_esc <- subset(dat, Species == 'Carcharias taurus')
coast <- crop(coast0, extent(c(-69, -47, -47, -28)))

# Límites provinciales
provincias <- 'DOWNLOAD Political borders'
provincias <- readOGR(dsn = provincias, layer = 'provincias_argentina')
provincias <- crop(provincias, extent(c(-69, -47, -47, -28)))
limite_internacional <- crop(Border, extent(c(-69, -47, -47, -28)))

ggplot() +
  geom_point(data = dat, aes(x = Longitude, y = Latitude), shape = 21, size = 12, color = 'grey85', fill = 'grey85') +
  geom_polygon(data = coast, aes(x = long, y = lat, group = group), color = 'grey50', fill = 'white', linewidth = 0.01) +
  geom_path(data = provincias, aes(x = long, y = lat, group = group), color = 'grey70', linewidth = 0.15, linetype = "dashed") +
  geom_path(data = limite_internacional, aes(x = long, y = lat, group = group), color = 'grey50', linewidth = 0.15) +
  geom_point(data = dat_gat, aes(x = Longitude, y = Latitude), shape = 21, size = 3, stroke = 0.2, color = 'black', fill = '#781C6DFF') +
  geom_point(data = dat_esc, aes(x = Longitude, y = Latitude), shape = 24, size = 3, stroke = 0.2, color = 'black', fill = '#FCFFA4FF') +  
  geom_point(data = dat_bac, aes(x = Longitude, y = Latitude), shape = 3, size = 2, stroke = 0.75, color = '#ED6925FF') +  
  geom_point(data = dat_caz, aes(x = Longitude, y = Latitude), shape = 4, size = 2, stroke = 0.75, color = '#000004FF') +
  scale_y_continuous(name = 'Latitude (S)', breaks = c(-45, -38, -31), labels = c('45º', '38º', '31º')) + 
  scale_x_continuous(name = 'Longitude (W)', breaks = c(-68, -60, -52), labels = c('68º', '60º', '52º')) +
  coord_equal(expand = 0) + 
  theme(panel.background = element_rect(fill = 'grey95'),
        panel.grid = element_blank(),legend.position = 'none',
        panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.25))
ggsave('Figure 6.pdf', width = 15, height = 15, units = 'cm')


#------------------------------------ Figure 7 ---------------------------------------

# Closed areas that benefit chondrichthyans

# Read shapes
directorio <- 'READ shapes'
veda_rdp <- readOGR(dsn = directorio, layer = 'Área_de_veda_RdlP')
veda_rincon <- readOGR(dsn = directorio, layer = 'Área_de_veda_El_Rincón')

# Run the following with the objects loaded from Figures 1, 2, S1.2, & S1.3
Col <- c('#000004FF', '#781C6DFF', '#ED6925FF', '#FCFFA4FF')
df_10_summer <- data.frame(coordinates(mod_mosaic_10_summer), as.data.frame(mod_mosaic_10_summer))
ggplot() +
  geom_tile(data = df_10_summer, aes(x = x, y = y, fill = as.factor(layer))) + 
  scale_fill_manual(values = Col, na.value = '#000004FF') + 
  geom_polygon(data = df_rdp, aes(x = x, y = y), color = 'grey95', fill = 'grey95', linewidth = 0.75) +
  geom_polygon(data = df_lagoa, aes(x = x, y = y), color = 'grey95', fill = 'grey95',linewidth = 0.1) +
  geom_polygon(data = coast0, aes(x = long, y = lat, group = group), color = 'grey50', fill = 'white', linewidth = 0.15) +
  geom_polygon(data = veda_rdp, aes(x = long, y = lat, group = group), color = 'black', fill = NA, linewidth = 0.2) +
  geom_polygon(data = veda_rincon, aes(x = long, y = lat, group = group), color = 'black', fill = NA, linewidth = 0.2) +
  geom_rect(data = df_mar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'grey95') +
  geom_rect(data = df_tierra, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'white') +
  scale_y_continuous(name = NULL, breaks = c(-41, -38, -35), labels = c('41º', '38º', '35º')) + 
  scale_x_continuous(name = NULL, breaks = c(-62, -59, -56), labels = c('62º', '59º', '56º')) +
  coord_equal(xlim = c(-63.5, -55), ylim = c(-42.5, -34), expand = 0) + 
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_blank(), legend.position = 'none',
        panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.3))
ggsave('Figure 7a.tiff', dpi = 900, width = 10, height = 10, units = 'cm', device = grDevices::tiff)

# Percentages of area covered by MPAs for G. galeus and N. cepedianus
table(getValues(mod_mosaic_10_summer))
overlap_rdp <- crop(mod_mosaic_10_summer, veda_rdp)
overlap_rdp <- mask(overlap_rdp, veda_rdp)
overlap_rin <- crop(mod_mosaic_10_summer, veda_rincon)
overlap_rin <- mask(overlap_rin, veda_rincon)
# Percentage of habitat suitability protected for both species
(table(getValues(overlap_rdp))[1] + table(getValues(overlap_rin))[2]) * 100 / table(getValues(mod_mosaic_10_summer))[4]
# Percentage of habitat suitability protected for G. galeus
(table(getValues(overlap_rdp))[1] + table(getValues(overlap_rin))[1] + table(getValues(overlap_rin))[2]) * 100 / (table(getValues(mod_mosaic_10_summer))[3] + table(getValues(mod_mosaic_10_summer))[4])
# Percentage of habitat suitability protected for N. cepedianus
(table(getValues(overlap_rdp))[1] + table(getValues(overlap_rin))[2]) * 100 / (table(getValues(mod_mosaic_10_summer))[2] + table(getValues(mod_mosaic_10_summer))[4])

# Run the following with the objects loaded from Figures 8.4
Col <- c('#000004FF', '#56106EFF', '#BB3754FF', '#F98C0AFF', '#FCFFA4FF')
df_10 <- data.frame(coordinates(anual_global_10), as.data.frame(anual_global_10))
ggplot() +
  geom_tile(data = df_10, aes(x = x, y = y, fill = as.factor(layer))) + 
  scale_fill_manual(values = Col, na.value = '#000004FF') + 
  geom_polygon(data = df_rdp, aes(x = x, y = y), color = 'grey95', fill = 'grey95', linewidth = 0.75) +
  geom_polygon(data = df_lagoa, aes(x = x, y = y), color = 'grey95', fill = 'grey95',linewidth = 0.1) +
  geom_polygon(data = coast0, aes(x = long, y = lat, group = group), color = 'grey50', fill = 'white', linewidth = 0.15) +
  geom_polygon(data = veda_rdp, aes(x = long, y = lat, group = group), color = 'black', fill = NA, linewidth = 0.2) +
  geom_polygon(data = veda_rincon, aes(x = long, y = lat, group = group), color = 'black', fill = NA, linewidth = 0.2) +
  geom_rect(data = df_mar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'grey95') +
  geom_rect(data = df_tierra, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'white') +
  scale_y_continuous(name = NULL, breaks = c(-41, -38, -35), labels = c('41º', '38º', '35º')) + 
  scale_x_continuous(name = NULL, breaks = c(-62, -59, -56), labels = c('62º', '59º', '56º')) +
  coord_equal(xlim = c(-63.5, -55), ylim = c(-42.5, -34), expand = 0) + 
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_blank(), legend.position = 'none',
        panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.3))
ggsave('Figure 7b.tiff', dpi = 900, width = 10, height = 10, units = 'cm', device = grDevices::tiff)

# Percentage of area covered by MPAs for the 4 species
table(getValues(anual_global_10))
overlap_rdp <- crop(anual_global_10, veda_rdp)
overlap_rdp <- mask(overlap_rdp, veda_rdp)
overlap_rin <- crop(anual_global_10, veda_rincon)
overlap_rin <- mask(overlap_rin, veda_rincon)

# Percentage of habitat suitability for 4 protected species
(table(getValues(overlap_rdp))[1] + table(getValues(overlap_rin))[2]) * 100 / table(getValues(anual_global_10))[5]
# Percentage of habitat suitability for 3 protected species
(table(getValues(overlap_rdp))[1] + table(getValues(overlap_rin))[1] + table(getValues(overlap_rin))[2]) * 100 / (table(getValues(anual_global_10))[4] + table(getValues(anual_global_10))[5])


#------------------------------------ Figure 8 ---------------------------------------

# Image made in CorelDraw X8


#------------------------------------ Figure 9 ---------------------------------------

# Image made in CorelDraw X8


#------------------------------------ Figure S1.1 ---------------------------------------

# Annual continuous habitat suitability
final_mod0 <- raster('G_galeus_annual.tif')
final_mod0 <- crop(final_mod0, extent(final_mod0) + c(0.5, 0, 0, 0))
final_mod <- extend(final_mod0, extent(final_mod0) + 1.75)
occ_cal <- read.csv('G_galeus_annual.csv')

# Annual binary habitat suitability
bg_binary <- final_mod
bg_binary[bg_binary < 0.1] <- 1
bg_binary[bg_binary] <- 2

# Threshold (%5 minimum training presence)
mtp5 <- sdm_threshold.5(final_mod, occ_cal[, c('longitude', 'latitude')], 'p05', binary = F)
bin_5 <- final_mod
bin_5[bin_5 < minValue(mtp5)] <- NA
bin_5[bin_5 >= minValue(mtp5)] <- 3

# Threshold (%10 minimum training presence)
mtp10 <- sdm_threshold.10(final_mod, occ_cal[, c('longitude', 'latitude')], 'p10', binary = F)
bin_10 <- final_mod
bin_10[bin_10 < minValue(mtp10)] <- NA
bin_10[bin_10 >= minValue(mtp10)] <- 4

# Mosaico
mod_mosaic <- mosaic(bg_binary, bin_5, bin_10, fun = sum)
mod_mosaic[mod_mosaic < 1] <- NA

Col <- c('#00204DFF', '#7C7B78FF', '#FFEA46FF')
df <- data.frame(coordinates(mod_mosaic), as.data.frame(mod_mosaic))

# Plot
ggplot() +
  geom_tile(data = df, aes(x = x, y = y, fill = as.factor(layer))) + 
  scale_fill_manual(values = Col, na.value = 'grey95') + 
  geom_polygon(data = M.buff, aes(x = long, y = lat, group = group), linewidth = 0.75, color = 'grey5', fill = NA) +
  geom_polygon(data = df_rdp, aes(x = x, y = y), color = 'grey5', fill = 'grey95', linewidth = 0.75) +
  geom_polygon(data = df_lagoa, aes(x = x, y = y), color = 'grey5', fill = 'grey95',linewidth = 0.1) +
  geom_polygon(data = coast0, aes(x = long, y = lat, group = group), color = 'grey50', fill = 'white', linewidth = 0.15) +
  geom_rect(data = df_mar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'grey95') +
  geom_rect(data = df_tierra, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'white') +
  scale_y_continuous(name = NULL, breaks = c(-55, -45, -35, -25), labels = c('55º', '45º', '35º', '25º')) + 
  scale_x_continuous(name = NULL, breaks = c(-70, -60, -50, -40), labels = c('70º', '60º', '50º', '40º')) +
  coord_equal(xlim = c(-70.2, -37.1), ylim = c(-59.05, -21.5), expand = 0) + 
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_blank(), legend.position = 'none',
        panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.5)) 
ggsave('Figure S1.1a.tif', dpi = 900, width = 9.1, height = 10, units = 'cm', device = grDevices::tiff)


# Annual continuous habitat suitability
final_mod0 <- raster('N_cepedianus_annual.tif')
final_mod0 <- crop(final_mod0, extent(final_mod0) + c(0.5, 0, 0, 0))
final_mod <- extend(final_mod0, extent(final_mod0) + 1.75)
occ_cal <- read.csv('N_cepedianus_annual.csv')

# Annual binary habitat suitability
bg_binary <- final_mod
bg_binary[bg_binary < 0.1] <- 1
bg_binary[bg_binary] <- 2

# Threshold (%5 minimum training presence)
mtp5 <- sdm_threshold.5(final_mod, occ_cal[, c('longitude', 'latitude')], 'p05', binary = F)
bin_5 <- final_mod
bin_5[bin_5 < minValue(mtp5)] <- NA
bin_5[bin_5 >= minValue(mtp5)] <- 3

# Threshold (%10 minimum training presence)
mtp10 <- sdm_threshold.10(final_mod, occ_cal[, c('longitude', 'latitude')], 'p10', binary = F)
bin_10 <- final_mod
bin_10[bin_10 < minValue(mtp10)] <- NA
bin_10[bin_10 >= minValue(mtp10)] <- 4

# Mosaico
mod_mosaic <- mosaic(bg_binary, bin_5, bin_10, fun = sum)
mod_mosaic[mod_mosaic < 1] <- NA

Col <- c('#440154FF', '#21908CFF', '#FDE725FF')
df <- data.frame(coordinates(mod_mosaic), as.data.frame(mod_mosaic))

# Plot
ggplot() +
  geom_tile(data = df, aes(x = x, y = y, fill = as.factor(layer))) + 
  scale_fill_manual(values = Col, na.value = 'grey95') + 
  geom_polygon(data = M.buff, aes(x = long, y = lat, group = group), linewidth = 0.75, color = 'grey5', fill = NA) +
  geom_polygon(data = df_rdp, aes(x = x, y = y), color = 'grey5', fill = 'grey95', linewidth = 0.75) +
  geom_polygon(data = df_lagoa, aes(x = x, y = y), color = 'grey5', fill = 'grey95',linewidth = 0.1) +
  geom_polygon(data = coast0, aes(x = long, y = lat, group = group), color = 'grey50', fill = 'white', linewidth = 0.15) +
  scale_y_continuous(name = NULL, breaks = c(-55, -45, -35, -25), labels = c('55º', '45º', '35º', '25º')) + 
  scale_x_continuous(name = NULL, breaks = c(-70, -60, -50, -40), labels = c('70º', '60º', '50º', '40º')) +
  coord_equal(xlim = c(-70.2, -38.5), ylim = c(-59.05, -21.5), expand = 0) + 
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_blank(), legend.position = 'none',
        panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.5)) 
ggsave('Figure S1.1b.tif', dpi = 900, width = 9.1, height = 10, units = 'cm', device = grDevices::tiff)


#------------------------------------ Figure S2.1 ---------------------------------------

# Annual continuous habitat suitability

final_mod0 <- raster('N_cepedianus_annual.tif')
final_mod0 <- crop(final_mod0, extent(final_mod0) + c(0.5, 0, 0, 0))
final_mod <- extend(final_mod0, extent(final_mod0) + 1.75)
occ_cal <- read.csv('N_cepedianus_annual.csv')
occ_ind <- read.csv('N_cepedianus_annual/N_cepedianus_annual_indep.csv')

# Calibration areas
occ.tot <- SpatialPoints(cbind(occ_cal$longitude, occ_cal$latitude), proj4string = crs)
M.buff <- buffer(occ.tot, width = 1000000)
M.buff <- crop(M.buff, extent(final_mod0))

df <- data.frame(coordinates(final_mod), as.data.frame(final_mod))

# Plot
ggplot() +
  geom_tile(data = df, aes(x = x, y = y, fill = calibration_median)) + 
  scale_fill_viridis_c(option = 'D', na.value = 'grey95') + 
  geom_polygon(data = M.buff, aes(x = long, y = lat, group = group), linewidth = 0.75, color = 'grey5', fill = NA) +
  geom_polygon(data = df_rdp, aes(x = x, y = y), color = 'grey5', fill = 'grey95', linewidth = 0.75) +
  geom_polygon(data = df_lagoa, aes(x = x, y = y), color = 'grey5', fill = 'grey95', linewidth = 0.1) +
  geom_polygon(data = coast0, aes(x = long, y = lat, group = group), color = 'grey10', fill = 'grey50', linewidth = 0.15) +
  geom_point(data = occ_cal, aes(x = longitude, y = latitude), size  =  0.3, color = 'black', fill = 'black') +
  geom_point(data = occ_ind, aes(x = longitude, y = latitude), size  =  0.3, color = 'grey95', fill = 'grey95') +
  scale_y_continuous(name = NULL, breaks = c(-55, -45, -35, -25), labels = c('55º', '45º', '35º', '25º')) + 
  scale_x_continuous(name = NULL, breaks = c(-70, -60, -50, -40), labels = c('70º', '60º', '50º', '40º')) +
  ggsn::scalebar(x.min = min(df$x), x.max = max(df$x), y.min = min(df$y), y.max = max(df$y), transform = T, 
                 dist = 200, st.size = 2.5, height = 0.013, model = 'WGS84', dist_unit = 'km',
                 border.size = 0.5, anchor = c(x = -49, y = -57.4)) +
  coord_equal(xlim = c(-70.2, -38.5), ylim = c(-59.05, -21.5), expand = 0) + 
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_blank(), legend.position = 'none',
        panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.5)) 
ggsave('Figure S2.1a.tif', dpi = 900, width = 9.1, height = 10, units = 'cm', device = grDevices::tiff)

# Annual binary habitat suitability

bg_binary <- final_mod
bg_binary[bg_binary < 0.1] <- 1
bg_binary[bg_binary] <- 2

# Threshold (%5 minimum training presence)
mtp5 <- sdm_threshold.5(final_mod, occ_cal[, c('longitude', 'latitude')], 'p05', binary = F)
bin_5 <- final_mod
bin_5[bin_5 < minValue(mtp5)] <- NA
bin_5[bin_5 >= minValue(mtp5)] <- 3

# Threshold (%10 minimum training presence)
mtp10 <- sdm_threshold.10(final_mod, occ_cal[, c('longitude', 'latitude')], 'p10', binary = F)
bin_10 <- final_mod
bin_10[bin_10 < minValue(mtp10)] <- NA
bin_10[bin_10 >= minValue(mtp10)] <- 4

# Mosaico
mod_mosaic <- mosaic(bg_binary, bin_5, bin_10, fun = sum)
mod_mosaic[mod_mosaic < 1] <- NA

Col <- c('#440154FF', '#21908CFF', '#FDE725FF')
df <- data.frame(coordinates(mod_mosaic), as.data.frame(mod_mosaic))

# Plot
ggplot() +
  geom_tile(data = df, aes(x = x, y = y, fill = as.factor(layer))) + 
  scale_fill_manual(values = Col, na.value = 'grey95') + 
  geom_polygon(data = M.buff, aes(x = long, y = lat, group = group), linewidth = 0.75, color = 'grey5', fill = NA) +
  geom_polygon(data = df_rdp, aes(x = x, y = y), color = 'grey5', fill = 'grey95', linewidth = 0.75) +
  geom_polygon(data = df_lagoa, aes(x = x, y = y), color = 'grey5', fill = 'grey95',linewidth = 0.1) +
  geom_polygon(data = coast0, aes(x = long, y = lat, group = group), color = 'grey50', fill = 'white', linewidth = 0.15) +
  scale_y_continuous(name = NULL, breaks = c(-55, -45, -35, -25), labels = c('55º', '45º', '35º', '25º')) + 
  scale_x_continuous(name = NULL, breaks = c(-70, -60, -50, -40), labels = c('70º', '60º', '50º', '40º')) +
  coord_equal(xlim = c(-70.2, -38.5), ylim = c(-59.05, -21.5), expand = 0) + 
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_blank(), legend.position = 'none',
        panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.5)) 
ggsave('Figure S2.1b.tif', dpi = 900, width = 9.1, height = 10, units = 'cm', device = grDevices::tiff)


#------------------------------------ Figure S2.2 ---------------------------------------

# Annual model response plots

# Read predictors
env <- stack('env.tif')
var_names <- c('Distance_to_coast', 'Depth', 'Surface_temperature', 'SST_fronts', 'Turbidity', 'Primary_productivity', 'Distance_to_colonies')
names(env) <- var_names

# Subset of predictors relevant to the model
var_names <- c('Depth', 'Surface_temperature', 'SST_fronts', 'Turbidity', 'Distance_to_colonies')
env <- env[[var_names]]
env[['Depth']] <- env[['Depth']] * -1

p_df <- data.frame()
a_df <- data.frame()
plot_data_df <- data.frame()

# Rio de la Plata and Lagoa dos Patos spatial polygons
df_rdp <- matrix(c(-60.2998, -58.4014, -55.8800, -58.1127, -60.2998, 
                   -34.2257, -32.5165, -33.9072, -36.0317, -34.2257), 5, 2)
df_rdp <- SpatialPolygons(list(Polygons(list(Polygon(df_rdp)), ID = 1)), proj4string = crs)
df_lagoa <- matrix(c(-51.3938, -50.2811, -50.8794, -51.4996, -51.9246, -52.0806, -52.8042, -51.3938,
                     -29.7969, -30.2501, -31.1866, -31.7214, -31.9466, -32.1433, -32.2448, -29.7969), 8, 2)
df_lagoa <- SpatialPolygons(list(Polygons(list(Polygon(df_lagoa)), ID = 1)), proj4string = crs)

Species <- 'Notorynchus cepedianus'

# Occurrencies
dat <- read.csv('annual.csv')
dat <- dat[, c('longitude', 'latitude')]

for(j in var_names){
  
  # prepare data
  Env <- env[[j]]
  
  # calibration areas
  occ.tot <- SpatialPoints(dat, proj4string = crs) 
  occ.buff <- buffer(occ.tot, width = 1000000) 
  env.M <- crop(Env, extent(occ.buff))
  env.M <- mask(env.M, occ.buff) 
  env.M <- mask(env.M, df_rdp, inverse = T)
  env.M <- mask(env.M, df_lagoa, inverse = T)
  env.M <- stack(env.M)
  
  # background points
  set.seed(111)
  notna <- which(complete.cases(values(env.M)))
  samp <- sample(notna, 10000, replace = F)
  samplocs <- as.data.frame(xyFromCell(env.M, samp))
  
  # SWD object
  data <- prepareSWD(species = Species, p = dat, a = samplocs, env = env.M)
  
  # run MaxEnt replicates
  folds = randomFolds(data, k = 10, only_presence = T, seed = 111)
  default_model <- train(method = 'Maxent', data = data, fc = 'lq', reg = 0.1, iter = 1000, folds = folds)
  
  # Presences and pseudoabsences with environmental data
  p <- .get_presence(default_model@data)
  p$var <- j
  names(p)[names(p) == j] <- 'values'
  a <- .get_absence(default_model@data)
  a$var <- j
  names(a)[names(a) == j] <- 'values'
  
  pred <- as.data.frame(matrix(data = NA, nrow = dim(data@data)[1], ncol = 10))
  for(i in 1:10){
    pred[, i] <- predict(default_model@models[[i]], data = data@data, type = 'logistic')
  }
  
  # for plotting
  plot_data <- as.data.frame(matrix(data = NA, nrow = dim(data@data)[1], ncol = 4))
  names(plot_data) <- c('mean', 'sd', 'max', 'min')
  plot_data$mean <- rowMeans(pred)
  plot_data$sd <- apply(pred, 1, sd)
  plot_data$max <- plot_data$mean + plot_data$sd
  plot_data$min <- plot_data$mean - plot_data$sd
  plot_data$var <- j
  plot_data$values <- data@data[, j]
  
  # data frames
  p_df <- rbind(p_df, p)
  a_df <- rbind(a_df, a)
  plot_data_df <- rbind(plot_data_df, plot_data)
}

# Plot preparation
plot_data_df <- plot_data_df[!(plot_data_df$var == 'Depth' & plot_data_df$values > 500), ]
plot_data_df <- plot_data_df[!(plot_data_df$var == 'Surface_temperature' & plot_data_df$values > 25), ]
plot_data_df <- plot_data_df[!(plot_data_df$var == 'Surface_temperature' & plot_data_df$values < 5), ]
plot_data_df <- plot_data_df[!(plot_data_df$var == 'SST_fronts' & plot_data_df$values > 0.5), ]
plot_data_df <- plot_data_df[!(plot_data_df$var == 'Turbidity' & plot_data_df$values > 2), ]
plot_data_df <- plot_data_df[!(plot_data_df$var == 'Distance_to_colonies' & plot_data_df$values > 1000), ]
plot_data_df <- transform(plot_data_df, var = factor(var, levels = c('Depth', 'Surface_temperature', 'SST_fronts', 'Turbidity', 'Distance_to_colonies')))

a_df <- a_df[!(a_df$var == 'Depth' & a_df$values > 500), ]
a_df <- a_df[!(a_df$var == 'Surface_temperature' & a_df$values > 25), ]
a_df <- a_df[!(a_df$var == 'Surface_temperature' & a_df$values < 5), ]
a_df <- a_df[!(a_df$var == 'SST_fronts' & a_df$values > 0.5), ]
a_df <- a_df[!(a_df$var == 'Turbidity' & a_df$values > 2), ]
a_df <- a_df[!(a_df$var == 'Distance_to_colonies' & a_df$values > 1000), ]
a_df <- transform(a_df, var = factor(var, levels = c('Depth', 'Surface_temperature', 'SST_fronts', 'Turbidity', 'Distance_to_colonies')))

p_df <- p_df[!(p_df$var == 'Depth' & p_df$values > 500), ]
p_df <- p_df[!(p_df$var == 'Surface_temperature' & p_df$values > 25), ]
p_df <- p_df[!(p_df$var == 'Surface_temperature' & p_df$values < 5), ]
p_df <- p_df[!(p_df$var == 'SST_fronts' & p_df$values > 0.5), ]
p_df <- p_df[!(p_df$var == 'Turbidity' & p_df$values > 2), ]
p_df <- p_df[!(p_df$var == 'Distance_to_colonies' & p_df$values > 1000), ]
p_df <- transform(p_df, var = factor(var, levels = c('Depth', 'Surface_temperature', 'SST_fronts', 'Turbidity', 'Distance_to_colonies')))

# Predictor names
var_names <- as_labeller(c(Depth = 'Depth~(m)', Turbidity = 'Coefficient~Kd490~(m^-1)', Surface_temperature = 'Surface~temperature~(ºC)', 
                           SST_fronts = 'Thermal~fronts~(ºC)', Distance_to_colonies = 'Distance~to~colonies~(km)'), default = label_parsed)

# Plot
ggplot(data = plot_data_df, aes(x = values, y = mean, ymin = min, ymax = max)) + 
  geom_line(colour = '#440154FF') + 
  geom_ribbon(fill = '#440154FF', alpha = 0.2) +
  geom_rug(data = p_df, inherit.aes = F, aes(values), sides = 't', color = '#FDE725FF', size = 0.3) + 
  geom_rug(data = a_df, inherit.aes = F, aes(values), sides = 'b', color = '#21908CFF', size = 0.3) + 
  labs(x = NULL, y = 'Logistic output') + ylim(0, 1) +
  theme(panel.background = element_blank(), 
        panel.grid.minor = element_blank(),                                       
        panel.border = element_rect(colour = 'black', fill = NA, size = 0.35),
        panel.grid.major = element_line(size = 0.2, colour = 'grey90'),
        strip.background = element_rect(fill = 'transparent'),
        strip.text = element_text(vjust = -0.5, size = 8),
        panel.spacing.y = unit(-0.3, 'lines'),
        panel.spacing.x = unit(0.7, 'lines'),
        plot.margin = unit(c(-0.2, 0.4, 0.2, 0.2), 'cm'),
        axis.title = element_text(size = 10)) +
  facet_wrap(~ var, scales = 'free_x', labeller = var_names, ncol = 3) 
ggsave('Figure S2.2.pdf', width = 15, height = 9, units = 'cm')


#------------------------------------ Figure S2.3 ---------------------------------------

# Seasonal binary habitat suitability

final_mod00 <- raster('N_cepedianus_annual.tif')

for(i in 1:4){
  
  final_mod0 <- raster(paste(season[i], '.tif', sep = '')) 
  final_mod0 <- crop(final_mod0, extent(final_mod0) + c(0.5, 0, 0, 0))
  final_mod <- extend(final_mod0, extent(final_mod00) + 1.75)
  occ_cal <- read.csv(paste("N_cepedianus_", season[i], '.csv', sep = '')) 
  
  # Calibration areas
  occ.tot <- SpatialPoints(cbind(occ_cal$longitude, occ_cal$latitude), proj4string = crs)
  M.buff <- buffer(occ.tot, width = 1000000)
  M.buff <- crop(M.buff, extent(final_mod0))
  
  # Binary
  bg_binary <- final_mod
  bg_binary[bg_binary < 0.1] <- 1
  bg_binary[bg_binary] <- 2
  
  # Threshold (%5 minimum training presence)
  mtp5 <- sdm_threshold.5(final_mod, occ_cal[, c('longitude', 'latitude')], 'p05', binary = F)
  bin_5 <- final_mod
  bin_5[bin_5 < minValue(mtp5)] <- NA
  bin_5[bin_5 >= minValue(mtp5)] <- 3
  
  # Threshold (%10 minimum training presence)
  mtp10 <- sdm_threshold.10(final_mod, occ_cal[, c('longitude', 'latitude')], 'p10', binary = F)
  bin_10 <- final_mod
  bin_10[bin_10 < minValue(mtp10)] <- NA
  bin_10[bin_10 >= minValue(mtp10)] <- 4
  
  # Mosaic
  mod_mosaic <- mosaic(bg_binary, bin_5, bin_10, fun = sum)
  mod_mosaic[mod_mosaic < 1] <- NA
  
  Col <- c('#440154FF', '#21908CFF', '#FDE725FF')
  df <- data.frame(coordinates(mod_mosaic), as.data.frame(mod_mosaic))
  
  # Plot
  ggplot() +
    geom_tile(data = df, aes(x = x, y = y, fill = as.factor(layer))) + 
    scale_fill_manual(values = Col, na.value = 'grey95') + 
    geom_polygon(data = M.buff, aes(x = long, y = lat, group = group), linewidth = 0.75, color = 'grey5', fill = NA) +
    geom_polygon(data = df_rdp, aes(x = x, y = y), color = 'grey5', fill = 'grey95', linewidth = 0.75) +
    geom_polygon(data = df_lagoa, aes(x = x, y = y), color = 'grey5', fill = 'grey95',linewidth = 0.1) +
    geom_polygon(data = coast0, aes(x = long, y = lat, group = group), color = 'grey50', fill = 'white', linewidth = 0.15) +
    geom_rect(data = df_mar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'grey95') +
    geom_rect(data = df_tierra, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'white') +
    scale_y_continuous(name = NULL, breaks = c(-55, -45, -35, -25), labels = c('55º', '45º', '35º', '25º')) + 
    scale_x_continuous(name = NULL, breaks = c(-70, -60, -50, -40), labels = c('70º', '60º', '50º', '40º')) +
    ggsn::scalebar(x.min = min(df$x), x.max = max(df$x), y.min = min(df$y), y.max = max(df$y), transform = T,
                   dist = 200, st.size = 2, height = 0.013, model = 'WGS84', dist_unit = 'km',
                   border.size = 0.25, anchor = c(x = -52, y = -57.4)) +
    coord_equal(xlim = c(-70.2, -38.5), ylim = c(-59.05, -21.5), expand = 0) + 
    theme(panel.background = element_rect(fill = 'transparent'),
          panel.grid = element_blank(), legend.position = 'none',
          panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.3)) 
  ggsave(paste('Figure S2.3_', season[i], '.tif', sep = ''), dpi = 900, width = 6.2, height = 6.6, units = 'cm', device = grDevices::tiff)
}


#------------------------------------ Figure S2.4 ---------------------------------------

# Seasonal model response plots

# Obtaining feature classes and regularization multipliers results
fc <- c('lq', 'lq', 'lp', 'lqp')
rm <- c(1.1, 1.0, 0.6, 0.3)

p_df <- data.frame()
a_df <- data.frame()
plot_data_df <- data.frame()

# Rio de la Plata and Lagoa dos Patos spatial polygons
df_rdp <- matrix(c(-60.2998, -58.4014, -55.8800, -58.1127, -60.2998, 
                   -34.2257, -32.5165, -33.9072, -36.0317, -34.2257), 5, 2)
df_rdp <- SpatialPolygons(list(Polygons(list(Polygon(df_rdp)), ID = 1)), proj4string = crs)
df_lagoa <- matrix(c(-51.3938, -50.2811, -50.8794, -51.4996, -51.9246, -52.0806, -52.8042, -51.3938,
                     -29.7969, -30.2501, -31.1866, -31.7214, -31.9466, -32.1433, -32.2448, -29.7969), 8, 2)
df_lagoa <- SpatialPolygons(list(Polygons(list(Polygon(df_lagoa)), ID = 1)), proj4string = crs)

Species <- 'Notorynchus cepedianus'

for(k in 1:4){
  
  # Read predictors
  env <- stack(paste(season[k], '.tif', sep = ''))
  var_names <- c('Distance_to_coast', 'Depth', 'Surface_temperature', 'SST_fronts', 'Turbidity', 'Primary_productivity', 'Distance_to_colonies')
  names(env) <- var_names
  
  # Relevant model predictors
  var_names <- c('Depth', 'Surface_temperature', 'SST_fronts', 'Turbidity', 'Distance_to_colonies')
  env <- env[[var_names]]
  env[['Depth']] <- env[['Depth']] * -1
  
  # Occurrencies
  dat <- read.csv(paste("N_cepedianus_", season[k], '.csv', sep = ''))
  dat <- dat[, c('longitude', 'latitude')]
  
  for(j in var_names){
    
    # prepare data
    Env <- env[[j]]
    
    # calibration areas
    occ.tot <- SpatialPoints(dat, proj4string = crs) 
    occ.buff <- buffer(occ.tot, width = 1000000) 
    env.M <- crop(Env, extent(occ.buff))
    env.M <- mask(env.M, occ.buff) 
    env.M <- mask(env.M, df_rdp, inverse = T)
    env.M <- mask(env.M, df_lagoa, inverse = T)
    env.M <- stack(env.M)
    
    # brackground points
    set.seed(111)
    notna <- which(complete.cases(values(env.M)))
    samp <- sample(notna, 10000, replace = F)
    samplocs <- as.data.frame(xyFromCell(env.M, samp))
    
    # SWD object
    data <- prepareSWD(species = Species, p = dat, a = samplocs, env = env.M)
    
    # run MaxEnt replicates
    folds = randomFolds(data, k = 10, only_presence = T, seed = 111)
    default_model <- train(method = 'Maxent', data = data, fc = fc[k], reg = rm[k], iter = 1000, folds = folds)
    
    # Presences and pseudoabsences with environmental data
    p <- .get_presence(default_model@data)
    p$var <- j
    names(p)[names(p) == j] <- 'values'
    p$Season <- season[k]
    a <- .get_absence(default_model@data)
    a$var <- j
    names(a)[names(a) == j] <- 'values'
    a$Season <- season[k]
    
    pred <- as.data.frame(matrix(data = NA, nrow = dim(data@data)[1], ncol = 10))
    for(i in 1:10){
      pred[, i] <- predict(default_model@models[[i]], data = data@data, type = 'logistic')
    }
    
    # plot
    plot_data <- as.data.frame(matrix(data = NA, nrow = dim(data@data)[1], ncol = 4))
    names(plot_data) <- c('mean', 'sd', 'max', 'min')
    plot_data$mean <- rowMeans(pred)
    plot_data$sd <- apply(pred, 1, sd)
    plot_data$max <- plot_data$mean + plot_data$sd
    plot_data$min <- plot_data$mean - plot_data$sd
    plot_data$var <- j
    plot_data$values <- data@data[, j]
    plot_data$Season <- season[k]
    
    # data frames
    p_df <- rbind(p_df, p)
    a_df <- rbind(a_df, a)
    plot_data_df <- rbind(plot_data_df, plot_data)
    
  }
}

# Plots preparation
order_i_want <- c('Depth', 'Surface_temperature', 'SST_fronts', 'Turbidity', 'Distance_to_colonies')

plot_data_df <- plot_data_df[!(plot_data_df$var == 'Depth' & plot_data_df$values > 500), ]
plot_data_df <- plot_data_df[!(plot_data_df$var == 'Surface_temperature' & plot_data_df$values > 25), ]
plot_data_df <- plot_data_df[!(plot_data_df$var == 'Surface_temperature' & plot_data_df$values < 5), ]
plot_data_df <- plot_data_df[!(plot_data_df$var == 'SST_fronts' & plot_data_df$values > 0.5), ]
plot_data_df <- plot_data_df[!(plot_data_df$var == 'Turbidity' & plot_data_df$values > 2), ]
plot_data_df <- plot_data_df[!(plot_data_df$var == 'Distance_to_colonies' & plot_data_df$values > 1000), ]
plot_data_df <- transform(plot_data_df, var = factor(var, levels = order_i_want), Season = factor(Season, levels = season))

a_df <- a_df[!(a_df$var == 'Depth' & a_df$values > 500), ]
a_df <- a_df[!(a_df$var == 'Surface_temperature' & a_df$values > 25), ]
a_df <- a_df[!(a_df$var == 'Surface_temperature' & a_df$values < 5), ]
a_df <- a_df[!(a_df$var == 'SST_fronts' & a_df$values > 0.5), ]
a_df <- a_df[!(a_df$var == 'Turbidity' & a_df$values > 2), ]
a_df <- a_df[!(a_df$var == 'Distance_to_colonies' & a_df$values > 1000), ]
a_df <- transform(a_df, var = factor(var, levels = order_i_want), Season = factor(Season, levels = season))

p_df <- p_df[!(p_df$var == 'Depth' & p_df$values > 500), ]
p_df <- p_df[!(p_df$var == 'Surface_temperature' & p_df$values > 25), ]
p_df <- p_df[!(p_df$var == 'Surface_temperature' & p_df$values < 5), ]
p_df <- p_df[!(p_df$var == 'SST_fronts' & p_df$values > 0.5), ]
p_df <- p_df[!(p_df$var == 'Turbidity' & p_df$values > 2), ]
p_df <- p_df[!(p_df$var == 'Distance_to_colonies' & p_df$values > 1000), ]
p_df <- transform(p_df, var = factor(var, levels = order_i_want), Season = factor(Season, levels = season))

# Predictor names
var_names <- as_labeller(c(Depth = 'Depth~(m)', Turbidity = 'Coefficient~Kd490~(m^-1)', Surface_temperature = 'Surface~temperature~(ºC)', 
                           SST_fronts = 'thermal~fronts~(ºC)', Distance_to_colonies = 'Distance~to~colonies~(km)', `summer` = 'Summer', `autumn` = 'Autumn', `winter` = 'Winter', 
                           `spring` = 'Spring'), default = label_parsed)

# plot
ggplot(data = plot_data_df, aes(x = values, y = mean, ymin = min, ymax = max)) + 
  geom_line(colour = '#440154FF') + 
  geom_ribbon(fill = '#440154FF', alpha = 0.2) +
  geom_rug(data = p_df, inherit.aes = F, aes(values), sides = 't', color = '#FDE725FF', linewidth = 0.3) + 
  geom_rug(data = a_df, inherit.aes = F, aes(values), sides = 'b', color = '#21908CFF', linewidth = 0.3) + 
  labs(x = NULL, y = 'Logistic output') + ylim(0, 1) +
  theme(panel.background = element_blank(), 
        panel.grid.minor = element_blank(),                                       
        panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.2),
        panel.grid.major = element_line(linewidth = 0.2, colour = 'grey90'),
        strip.background = element_rect(fill = 'transparent'),
        strip.text = element_text(vjust = -0.3, size = 8),
        plot.margin = unit(c(0, 0, 0.2, 0.2), 'cm'),
        axis.title = element_text(size = 10)) +
  facet_grid(c('Season', 'var'), scales = 'free', labeller = var_names) 
ggsave('Figure S2.4.pdf', width = 23, height = 15, units = 'cm')


#------------------------------------ Figure S2.5 ---------------------------------------

# Map of the core area of potential distribution

# Annual
final_mod000 <- raster('N_cepedianus_annual.tif')
final_mod000 <- crop(final_mod000, extent(final_mod000) + c(0.5, 0, 0, 0))
final_mod00 <- extend(final_mod000, extent(final_mod000) + 1.75)
occ_cal <- read.csv('N_cepedianus_annual.csv')
occ.tot <- SpatialPoints(cbind(occ_cal$longitude, occ_cal$latitude), proj4string = crs)
M.buff <- buffer(occ.tot, width = 1000000)
M.buff <- crop(M.buff, extent(final_mod000))
bg_binary <- final_mod00
bg_binary[bg_binary < 0.1] <- 1
bg_binary[bg_binary] <- 2

# Summer
final_mod0 <- raster('N_cepedianus_summer.tif')
final_mod0 <- crop(final_mod0, extent(final_mod0) + c(0.5, 0, 0, 0))
final_mod <- extend(final_mod0, extent(final_mod00) + 1.75)
occ_cal <- read.csv('N_cepedianus_summer.csv')
# Threshold (%5 minimum training presence)
mtp5 <- sdm_threshold.5(final_mod, occ_cal[, c('longitude', 'latitude')], 'p05', binary = F)
bin_5_summer <- final_mod
bin_5_summer[bin_5_summer < minValue(mtp5)] <- NA
bin_5_summer[bin_5_summer >= minValue(mtp5)] <- 1
# Threshold (%10 minimum training presence)
mtp10 <- sdm_threshold.10(final_mod, occ_cal[, c('longitude', 'latitude')], 'p10', binary = F)
bin_10_summer <- final_mod
bin_10_summer[bin_10_summer < minValue(mtp10)] <- NA
bin_10_summer[bin_10_summer >= minValue(mtp10)] <- 1

# Autumn
final_mod0 <- raster('N_cepedianus_autumn.tif') 
final_mod0 <- crop(final_mod0, extent(final_mod0) + c(0.5, 0, 0, 0))
final_mod <- extend(final_mod0, extent(final_mod00) + 1.75)
occ_cal <- read.csv('N_cepedianus_autumn.csv')
# Threshold (%5 minimum training presence)
mtp5 <- sdm_threshold.5(final_mod, occ_cal[, c('longitude', 'latitude')], 'p05', binary = F)
bin_5_autumn <- final_mod
bin_5_autumn[bin_5_autumn < minValue(mtp5)] <- NA
bin_5_autumn[bin_5_autumn >= minValue(mtp5)] <- 1
# Threshold (%10 minimum training presence)
mtp10 <- sdm_threshold.10(final_mod, occ_cal[, c('longitude', 'latitude')], 'p10', binary = F)
bin_10_autumn <- final_mod
bin_10_autumn[bin_10_autumn < minValue(mtp10)] <- NA
bin_10_autumn[bin_10_autumn >= minValue(mtp10)] <- 1

# Winter
final_mod0 <- raster('N_cepedianus_winter.tif') 
final_mod0 <- crop(final_mod0, extent(final_mod0) + c(0.5, 0, 0, 0))
final_mod <- extend(final_mod0, extent(final_mod00) + 1.75)
occ_cal <- read.csv('N_cepedianus_winter.csv')
# Threshold (%5 minimum training presence)
mtp5 <- sdm_threshold.5(final_mod, occ_cal[, c('longitude', 'latitude')], 'p05', binary = F)
bin_5_winter <- final_mod
bin_5_winter[bin_5_winter < minValue(mtp5)] <- NA
bin_5_winter[bin_5_winter >= minValue(mtp5)] <- 1
# Threshold (%10 minimum training presence)
mtp10 <- sdm_threshold.10(final_mod, occ_cal[, c('longitude', 'latitude')], 'p10', binary = F)
bin_10_winter <- final_mod
bin_10_winter[bin_10_winter < minValue(mtp10)] <- NA
bin_10_winter[bin_10_winter >= minValue(mtp10)] <- 1

# Spring
final_mod0 <- raster('N_cepedianus_spring.tif')
final_mod0 <- crop(final_mod0, extent(final_mod0) + c(0.5, 0, 0, 0))
final_mod <- extend(final_mod0, extent(final_mod00) + 1.75)
occ_cal <- read.csv('N_cepedianus_spring.csv')
# Threshold (%5 minimum training presence)
mtp5 <- sdm_threshold.5(final_mod, occ_cal[, c('longitude', 'latitude')], 'p05', binary = F)
bin_5_spring <- final_mod
bin_5_spring[bin_5_spring < minValue(mtp5)] <- NA
bin_5_spring[bin_5_spring >= minValue(mtp5)] <- 1
# Threshold (%10 minimum training presence)
mtp10 <- sdm_threshold.10(final_mod, occ_cal[, c('longitude', 'latitude')], 'p10', binary = F)
bin_10_spring <- final_mod
bin_10_spring[bin_10_spring < minValue(mtp10)] <- NA
bin_10_spring[bin_10_spring >= minValue(mtp10)] <- 1

Col <- c('#440154FF', '#3B528BFF', '#21908CFF', '#5DC863FF', '#FDE725FF')

# Mosaic 10% MTP
mod_mosaic_10 <- mosaic(bg_binary, bin_10_summer, bin_10_autumn, bin_10_winter, bin_10_spring, fun = sum)
mod_mosaic_10[mod_mosaic_10 < 2] <- NA
df_10 <- data.frame(coordinates(mod_mosaic_10), as.data.frame(mod_mosaic_10))

ggplot() +
  geom_tile(data = df_10, aes(x = x, y = y, fill = as.factor(layer))) + 
  scale_fill_manual(values = Col, na.value = 'grey95') + 
  geom_polygon(data = M.buff, aes(x = long, y = lat, group = group), linewidth = 0.75, color = 'grey5', fill = NA) +
  geom_polygon(data = df_rdp, aes(x = x, y = y), color = 'grey5', fill = 'grey95', linewidth = 0.75) +
  geom_polygon(data = df_lagoa, aes(x = x, y = y), color = 'grey5', fill = 'grey95',linewidth = 0.1) +
  geom_polygon(data = coast0, aes(x = long, y = lat, group = group), color = 'grey50', fill = 'white', linewidth = 0.15) +
  geom_rect(data = df_mar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'grey95') +
  geom_rect(data = df_tierra, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'white') +
  scale_y_continuous(name = NULL, breaks = c(-55, -45, -35, -25), labels = c('55º', '45º', '35º', '25º')) + 
  scale_x_continuous(name = NULL, breaks = c(-70, -60, -50, -40), labels = c('70º', '60º', '50º', '40º')) +
  ggsn::scalebar(x.min = min(df_10$x), x.max = max(df_10$x), y.min = min(df_10$y), y.max = max(df_10$y), transform = T,
                 dist = 200, st.size = 2.5, height = 0.013, model = 'WGS84', dist_unit = 'km',
                 border.size = 0.5, anchor = c(x = -40.5, y = -57.35)) +
  coord_equal(xlim = c(-70.2, -38.5), ylim = c(-59, -22), expand = 0) + 
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_blank(), legend.position = 'none',
        panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.5))
ggsave('Figure S1.5a.tif', dpi = 900, width = 10.2, height = 10.6, units = 'cm', device = grDevices::tiff)

# Mosaic 5% MTP
mod_mosaic_5 <- mosaic(bg_binary, bin_5_summer, bin_5_autumn, bin_5_winter, bin_5_spring, fun = sum)
mod_mosaic_5[mod_mosaic_5 < 2] <- NA
df_5 <- data.frame(coordinates(mod_mosaic_5), as.data.frame(mod_mosaic_5))

ggplot() +
  geom_tile(data = df_5, aes(x = x, y = y, fill = as.factor(layer))) + 
  scale_fill_manual(values = Col, na.value = 'grey95') + 
  geom_polygon(data = M.buff, aes(x = long, y = lat, group = group), linewidth = 0.75, color = 'grey5', fill = NA) +
  geom_polygon(data = df_rdp, aes(x = x, y = y), color = 'grey5', fill = 'grey95', linewidth = 0.75) +
  geom_polygon(data = df_lagoa, aes(x = x, y = y), color = 'grey5', fill = 'grey95',linewidth = 0.1) +
  geom_polygon(data = coast0, aes(x = long, y = lat, group = group), color = 'grey50', fill = 'white', linewidth = 0.15) +
  geom_rect(data = df_mar, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'grey95') +
  geom_rect(data = df_tierra, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = 'white') +
  scale_y_continuous(name = NULL, breaks = c(-55, -45, -35, -25), labels = c('55º', '45º', '35º', '25º')) + 
  scale_x_continuous(name = NULL, breaks = c(-70, -60, -50, -40), labels = c('70º', '60º', '50º', '40º')) +
  coord_equal(xlim = c(-70.2, -38.5), ylim = c(-59, -22), expand = 0) + 
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_blank(), legend.position = 'none',
        panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.5)) 
ggsave('Figure S1.5b.tif', dpi = 900, width = 10.2, height = 10.6, units = 'cm', device = grDevices::tiff)


#------------------------------------ Figure S3.1 ---------------------------------------

# Discrepancies in the depth variable

# Read and name predictors
env <- stack('global_predictors.tif')
var_names <- c('Temperature', 'Surface_temperature', 'Primary_productivity', 'Kd490', 'Salinity',
               'Bathymetry', 'Distance_to_coast', 'Slope', 'Thermal_fronts')
names(env) <- var_names

# Galeorhinus galeus
# Read occurrences
dat <- read.csv('Free_occurrence_records.csv') # Not all of them are available since some are not freely accessible
dat <- subset(dat, Species == 'Galeorhinus galeus')
dat <- subset(dat, Source1 %in% c('GBIF', 'OBIS', 'Government data', 'Published literature', 'Grey literature', 'Social media', "Professional contact"))

# Delete duplicates
dups <- duplicated(dat[c('Longitude', 'Latitude')])
occ_cal <- dat[!dups, ]

# Extract values
valores <- data.frame(Profundidad = extract(env[['Bathymetry']], SpatialPoints(occ_cal[, c('Longitude', 'Latitude')], crs)))

ggplot(data = valores, aes(sample(seq_along(Profundidad)), Profundidad)) +
  geom_point(col = "#00204DFF", fill = "#FFEA46FF", shape = 21, stroke = 0.75) +
  geom_hline(yintercept = min(-dat$Depth, na.rm = T)) +
  ylab('Depth (m)') + xlab('Number of records') +
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_line(color = 'grey95'),
        panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.25)) 
ggsave('Figure S3.1a.pdf', width = 15, height = 10, units = 'cm')

# Notorynchus cepedianus
# Read occurrences
dat <- read.csv('Free_occurrence_records.csv') # Not all of them are available since some are not freely accessible
dat <- subset(dat, Species == 'Notorynchus cepedianus')
dat <- subset(dat, Source1 %in% c('GBIF', 'OBIS', 'Government data', 'Published literature', 'Grey literature', 'Social media', "Professional contact"))

# Delete duplicates
dups <- duplicated(dat[c('Longitude', 'Latitude')])
occ_cal <- dat[!dups, ]

# Extract values
valores <- data.frame(Profundidad = extract(env[['Bathymetry']], SpatialPoints(occ_cal[, c('Longitude', 'Latitude')], crs)))

ggplot(data = valores, aes(sample(seq_along(Profundidad)), Profundidad)) +
  geom_point(col = "#440154FF", fill = "#FDE725FF", shape = 21, stroke = 0.75) +
  geom_hline(yintercept = min(-dat$Depth, na.rm = T)) +
  ylab('Depth (m)') + xlab('Number of records') +
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_line(color = 'grey95'),
        panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.25)) 
ggsave('Figure S3.1b.pdf', width = 15, height = 10, units = 'cm')


#------------------------------------ Figure S3.2 ---------------------------------------

# Calibration area + calibration points
library(stars)

# Galeorhinus galeus
occ_cal <- read.csv('G_galeus_global.csv') 
env.M <- read_stars('G_galeus_global_Calibration_areas.tif')
env_buff <- env.M > -Inf
env_buff <- st_as_sf(env_buff, as_points = F, merge = T)

ggplot() +
  geom_sf(data = env_buff, col = 'black', fill = '#31446BFF', linewidth = 0.5) +
  geom_sf(data = coast, col = 'grey30', fill = 'grey50', linewidth = 0.1) +
  geom_point(data = occ_cal, aes(x = Longitude, y = Latitude), shape = 21, size = 0.5, color = '#FFEA46FF', fill = '#00204DFF') +
  scale_fill_manual(values = '#31446BFF', na.value = 'grey95') + coord_equal(expand = 0) +
  scale_y_continuous(name = NULL, breaks = c(-50, 0, 50), labels = c('50ºS', '0º', '50ºN')) + 
  scale_x_continuous(name = NULL, breaks = c(-100, 0, 100), labels = c('100ºW', '0º', '100ºE')) +
  coord_sf(xlim = c(-180, 180), ylim = c(-68.9, 83.6), expand = F) +
  theme(panel.background = element_rect(fill = NULL), panel.grid = element_blank(),
        legend.position = 'none', axis.text = element_text(size = 8),
        panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.5))
ggsave('Figure S3.2a.tiff', dpi = 900, width = 20, height = 8.5, units = 'cm', device = grDevices::tiff)

# Notorynchus cepedianus
occ_cal <- read.csv('N_cepedianus_global.csv') 
env.M <- read_stars('N_cepedianus_global_Calibration_areas.tif')
env_buff <- env.M > -Inf
env_buff <- st_as_sf(env_buff, as_points = F, merge = T)

ggplot() +
  geom_sf(data = env_buff, col = 'black', fill = '#21908CFF', linewidth = 0.5) +
  geom_sf(data = coast, col = 'grey30', fill = 'grey50', linewidth = 0.1) +
  geom_point(data = occ_cal, aes(x = Longitude, y = Latitude), shape = 21, size = 0.5, color = '#FDE725FF', fill = '#440154FF') +
  scale_fill_manual(values = '#21908CFF', na.value = 'grey95') + coord_equal(expand = 0) +
  scale_y_continuous(name = NULL, breaks = c(-50, 0, 50), labels = c('50ºS', '0º', '50ºN')) + 
  scale_x_continuous(name = NULL, breaks = c(-100, 0, 100), labels = c('100ºW', '0º', '100ºE')) +
  coord_sf(xlim = c(-180, 180), ylim = c(-68.9, 83.6), expand = F) +
  theme(panel.background = element_rect(fill = NULL), panel.grid = element_blank(),
        legend.position = 'none', axis.text = element_text(size = 8),
        panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.5))
ggsave('Figure S3.2b.tiff', dpi = 900, width = 20, height = 8.5, units = 'cm', device = grDevices::tiff)


#------------------------------------ Figure S3.3 ---------------------------------------

# Continuos annual habitat suitability at a global scale

# Galeorhinus galeus
pred <- raster('G_galeus_global_Predictions.tif')

# Strict extrapolation areas
global_mop <- raster('G_galeus_global_Mop.tif') 
global_mop[global_mop > 0] <- NA
global_mop[global_mop == 0] <- 2
pred <- crop(pred, global_mop)

# Delete MOP from prediction
pred_mop <- mosaic(pred, global_mop, fun = max) 
pred_mop[pred_mop > 1] <- NA

df <- data.frame(coordinates(pred_mop), as.data.frame(pred_mop))

ggplot() +
  geom_tile(data = df, aes(x = x, y = y, fill = layer)) + 
  geom_sf(data = coast, col = 'grey10', fill = 'grey20', linewidth = 0.1) +
  scale_fill_viridis(option = 'E', na.value = '#00204DFF') + coord_sf(expand = F) +
  scale_y_continuous(name = NULL, breaks = c(-50, 0, 50), labels = c('50ºS', '0º', '50ºN')) + 
  scale_x_continuous(name = NULL, breaks = c(-100, 0, 100), labels = c('100ºW', '0º', '100ºE')) +
  theme(panel.background = element_rect(fill = NULL), panel.grid = element_blank(),
        legend.position = 'none', axis.text = element_text(size = 8),
        panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.5))
ggsave('Figure S3.3a.tiff', dpi = 900, width = 20, height = 8.5, units = 'cm', device = grDevices::tiff)

# Notorynchus cepedianus
pred <- raster('N_cepedianus_global_Predictions.tif')

# Strict extrapolation areas
global_mop <- raster('N_cepedianus_global_Mop.tif') 
global_mop[global_mop > 0] <- NA
global_mop[global_mop == 0] <- 2
pred <- crop(pred, global_mop)

# Delete MOP from prediction
pred_mop <- mosaic(pred, global_mop, fun = max) 
pred_mop[pred_mop > 1] <- NA

df <- data.frame(coordinates(pred_mop), as.data.frame(pred_mop))

ggplot() +
  geom_tile(data = df, aes(x = x, y = y, fill = layer)) + 
  geom_sf(data = coast, col = 'grey10', fill = 'grey20', linewidth = 0.1) +
  scale_fill_viridis(option = 'D', na.value = '#440154FF') + coord_sf(expand = F) +
  scale_y_continuous(name = NULL, breaks = c(-50, 0, 50), labels = c('50ºS', '0º', '50ºN')) + 
  scale_x_continuous(name = NULL, breaks = c(-100, 0, 100), labels = c('100ºW', '0º', '100ºE')) +
  theme(panel.background = element_rect(fill = NULL), panel.grid = element_blank(),
        legend.position = 'none', axis.text = element_text(size = 8),
        panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.5))
ggsave('Figure S3.3b.tiff', dpi = 900, width = 20, height = 8.5, units = 'cm', device = grDevices::tiff)


#------------------------------------ Figure S3.4 ---------------------------------------

# Annual model response plots

# Galeorhinus galeus
# Selected feature class and regularization multiplier
fc <- 'lq' 
rm <- 0.1

occ_cal <- read.csv('G_galeus_global.csv') 
occ_cal <- occ_cal[, c('Longitude', 'Latitude')]
env.M <- stack('G_galeus_global_Calibration_areas.tif') 
var_set <- c('Temperature', 'Surface_temperature', 'Primary_productivity', 'Kd490', 'Salinity', 'Distance_to_coast', 'Thermal_fronts')
names(env.M) <- var_set

p_df = data.frame()
a_df = data.frame()
plot_data_df = data.frame()

source('prepareSWD_adw.R') # function from the 'SDMtune' package with a corrected bug

for(j in names(env.M)) {
  
  # prepare data
  env <- env.M[[j]]
  env <- stack(env)
  Vars <- j
  
  # background points
  set.seed(111)
  notna <- which(complete.cases(values(env)))
  samp <- sample(notna, 10000, replace = F)
  samplocs <- as.data.frame(xyFromCell(env, samp))
  
  # SWD objet
  data <- prepareSWD_adw(species = 'Galeorhinus galeus', p = occ_cal, a = samplocs, env = env)
  
  # run MaxEnt replicates
  folds = randomFolds(data, k = 10, only_presence = T, seed = 111)
  default_model <- train(method = 'Maxent', data = data, fc = fc, reg = rm, iter = 1000, folds = folds)
  
  # Presences and pseudo-absences with environmental data
  p <- .get_presence(default_model@data)
  p$var <- j
  names(p)[names(p) == j] <- 'values'
  a <- .get_absence(default_model@data)
  a$var <- j
  names(a)[names(a) == j] <- 'values'
  
  pred <- as.data.frame(matrix(data = NA, nrow = dim(data@data)[1], ncol = 10))
  for(i in 1:10){
    pred[, i] <- predict(default_model@models[[i]], data = data@data, type = 'logistic')
  }
  
  # plot data
  plot_data <- as.data.frame(matrix(data = NA, nrow = dim(data@data)[1], ncol = 4))
  names(plot_data) <- c('mean', 'sd', 'max', 'min')
  plot_data$mean <- rowMeans(pred)
  plot_data$sd <- apply(pred, 1, sd)
  plot_data$max <- plot_data$mean + plot_data$sd
  plot_data$min <- plot_data$mean - plot_data$sd
  plot_data$var <- j
  plot_data$values <- data@data[, j]
  
  # data frames
  p_df <- rbind(p_df, p)
  a_df <- rbind(a_df, a)
  plot_data_df <- rbind(plot_data_df, plot_data)
  
}

# Re-order
order_i_want1 <- c('Distance_to_coast', 'Temperature', 'Surface_temperature', 'Salinity', 'Kd490', 'Primary_productivity', 'Thermal_fronts')
p_df <- p_df[!(p_df$var == 'Salinity' & p_df$values < 20), ]
p_df <- p_df[!(p_df$var == 'Distance_to_coast' & p_df$values > 750), ]
p_df <- p_df[!(p_df$var == 'Kd490' & p_df$values > 0.5), ]
p_df <- p_df[!(p_df$var == 'Primary_productivity' & p_df$values > 0.07), ]
p_df <- p_df[!(p_df$var == 'Thermal_fronts' & p_df$values > 0.6), ]
p_df <- transform(p_df, var = factor(var, levels = order_i_want1))

a_df <- a_df[!(a_df$var == 'Salinity' & a_df$values < 20), ]
a_df <- a_df[!(a_df$var == 'Distance_to_coast' & a_df$values > 750), ]
a_df <- a_df[!(a_df$var == 'Kd490' & a_df$values > 0.5), ]
a_df <- a_df[!(a_df$var == 'Primary_productivity' & a_df$values > 0.07), ]
a_df <- a_df[!(a_df$var == 'Thermal_fronts' & a_df$values > 0.6), ]
a_df <- transform(a_df, var = factor(var, levels = order_i_want1))

plot_data_df <- plot_data_df[!(plot_data_df$var == 'Salinity' & plot_data_df$values < 20), ]
plot_data_df <- plot_data_df[!(plot_data_df$var == 'Distance_to_coast' & plot_data_df$values > 750), ]
plot_data_df <- plot_data_df[!(plot_data_df$var == 'Kd490' & plot_data_df$values > 0.5), ]
plot_data_df <- plot_data_df[!(plot_data_df$var == 'Primary_productivity' & plot_data_df$values > 0.07), ]
plot_data_df <- plot_data_df[!(plot_data_df$var == 'Thermal_fronts' & plot_data_df$values > 0.6), ]
plot_data_df <- transform(plot_data_df, var = factor(var, levels = order_i_want1))

# Labeller
VAR_names = as_labeller(c(Temperature = 'Bottom~temperature~(ºC)', Distance_to_coast = 'Distance~to~coast~(km)',
                          Surface_temperature = 'Surface~temperature~(ºC)', Kd490 = 'Coefficiente~Kd490~(m^-1)', 
                          Salinity = 'Bottom~salinity~(UPS)', Primary_productivity = 'Primary~prod.~(gCm^-2~day^-1)',
                          Thermal_fronts = 'Thermal~fronts~(ºC)'), default = label_parsed)

# Plot
ggplot(data = plot_data_df, aes(x = values, y = mean, ymin = min, ymax = max)) + 
  geom_line(colour = '#00204DFF') + 
  geom_ribbon(fill = '#00204DFF', alpha = 0.2) +
  geom_rug(data = p_df, inherit.aes = F, aes(values), sides = 't', color = '#FFEA46FF', linewidth = 0.3) + 
  geom_rug(data = a_df, inherit.aes = F, aes(values), sides = 'b', color = '#7C7B78FF', linewidth = 0.3) + 
  labs(x = NULL, y = 'Logistic output') + ylim(0, 1) +
  theme(panel.background = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.2),
        panel.grid.major = element_line(linewidth = 0.2, colour = 'grey90'),
        strip.background = element_rect(fill = 'transparent'),
        strip.text = element_text(vjust = -0.5, size = 9),
        plot.margin = unit(c(0, 0, 0.2, 0.2), 'cm'),
        axis.title = element_text(size = 10)) +
  facet_wrap(~var, scales = 'free_x', labeller = VAR_names, nrow = 2) 
ggsave('Figure S3.4a.pdf', width = 20, height = 12, units = 'cm')


# Notorynchus cepedianus
# Selected feature class and regularization multiplier
fc <- 'lq' 
rm <- 0.1

occ_cal <- read.csv('C:/Users/User1/Desktop/D/[D] - Research/Ongoing/Other species world/Notorynchus cepedianus/Calibration_points.csv') 
occ_cal <- occ_cal[, c('Longitude', 'Latitude')]
env.M <- stack('C:/Users/User1/Desktop/D/[D] - Research/Ongoing/Other species world/Notorynchus cepedianus/Calibration_areas.tif') 
var_set <- c('Temperature', 'Surface_temperature', 'Primary_productivity', 'Kd490', 'Salinity', 'Distance_to_coast', 'Thermal_fronts')
names(env.M) <- var_set

p_df = data.frame()
a_df = data.frame()
plot_data_df = data.frame()

source('prepareSWD_adw.R') # function from the 'SDMtune' package with a corrected bug

for(j in names(env.M)) {
  
  # prepare data
  env <- env.M[[j]]
  env <- stack(env)
  Vars <- j
  
  # background points
  set.seed(111)
  notna <- which(complete.cases(values(env)))
  samp <- sample(notna, 10000, replace = F)
  samplocs <- as.data.frame(xyFromCell(env, samp))
  
  # SWD objet
  data <- prepareSWD_adw(species = 'Notorynchus cepedianus', p = occ_cal, a = samplocs, env = env)
  
  # run MaxEnt replicates
  folds = randomFolds(data, k = 10, only_presence = T, seed = 111)
  default_model <- train(method = 'Maxent', data = data, fc = fc, reg = rm, iter = 1000, folds = folds)
  
  # Presences and pseudo-absences with environmental data
  p <- .get_presence(default_model@data)
  p$var <- j
  names(p)[names(p) == j] <- 'values'
  a <- .get_absence(default_model@data)
  a$var <- j
  names(a)[names(a) == j] <- 'values'
  
  pred <- as.data.frame(matrix(data = NA, nrow = dim(data@data)[1], ncol = 10))
  for(i in 1:10){
    pred[, i] <- predict(default_model@models[[i]], data = data@data, type = 'logistic')
  }
  
  # plot data
  plot_data <- as.data.frame(matrix(data = NA, nrow = dim(data@data)[1], ncol = 4))
  names(plot_data) <- c('mean', 'sd', 'max', 'min')
  plot_data$mean <- rowMeans(pred)
  plot_data$sd <- apply(pred, 1, sd)
  plot_data$max <- plot_data$mean + plot_data$sd
  plot_data$min <- plot_data$mean - plot_data$sd
  plot_data$var <- j
  plot_data$values <- data@data[, j]
  
  # data frames
  p_df <- rbind(p_df, p)
  a_df <- rbind(a_df, a)
  plot_data_df <- rbind(plot_data_df, plot_data)
  
}

# Re-order
order_i_want1 <- c('Distance_to_coast', 'Temperature', 'Surface_temperature', 'Salinity', 'Kd490', 'Primary_productivity', 'Thermal_fronts')
p_df <- p_df[!(p_df$var == 'Salinity' & p_df$values < 32), ]
p_df <- p_df[!(p_df$var == 'Surface_temperature' & p_df$values < 5), ]
p_df <- p_df[!(p_df$var == 'Surface_temperature' & p_df$values > 28), ]
p_df <- p_df[!(p_df$var == 'Distance_to_coast' & p_df$values > 750), ]
p_df <- p_df[!(p_df$var == 'Primary_productivity' & p_df$values > 0.06), ]
p_df <- p_df[!(p_df$var == 'Thermal_fronts' & p_df$values > 0.7), ]
p_df <- transform(p_df, var = factor(var, levels = order_i_want1))

a_df <- a_df[!(a_df$var == 'Salinity' & a_df$values < 32), ]
a_df <- a_df[!(a_df$var == 'Surface_temperature' & a_df$values < 5), ]
a_df <- a_df[!(a_df$var == 'Surface_temperature' & a_df$values > 28), ]
a_df <- a_df[!(a_df$var == 'Distance_to_coast' & a_df$values > 750), ]
a_df <- a_df[!(a_df$var == 'Primary_productivity' & a_df$values > 0.06), ]
a_df <- a_df[!(a_df$var == 'Thermal_fronts' & a_df$values > 0.7), ]
a_df <- transform(a_df, var = factor(var, levels = order_i_want1))

plot_data_df <- plot_data_df[!(plot_data_df$var == 'Salinity' & plot_data_df$values < 32), ]
plot_data_df <- plot_data_df[!(plot_data_df$var == 'Surface_temperature' & plot_data_df$values < 5), ]
plot_data_df <- plot_data_df[!(plot_data_df$var == 'Surface_temperature' & plot_data_df$values > 28), ]
plot_data_df <- plot_data_df[!(plot_data_df$var == 'Distance_to_coast' & plot_data_df$values > 750), ]
plot_data_df <- plot_data_df[!(plot_data_df$var == 'Primary_productivity' & plot_data_df$values > 0.06), ]
plot_data_df <- plot_data_df[!(plot_data_df$var == 'Thermal_fronts' & plot_data_df$values > 0.7), ]
plot_data_df <- transform(plot_data_df, var = factor(var, levels = order_i_want1))

# Labeller
VAR_names = as_labeller(c(Temperature = 'Bottom~temperature~(ºC)', Distance_to_coast = 'Distance~to~coast~(km)',
                          Surface_temperature = 'Surface~temperature~(ºC)', Kd490 = 'Coefficiente~Kd490~(m^-1)', 
                          Salinity = 'Bottom~salinity~(UPS)', Primary_productivity = 'Primary~prod.~(gCm^-2~day^-1)',
                          Thermal_fronts = 'Thermal~fronts~(ºC)'), default = label_parsed)

# Plot
ggplot(data = plot_data_df, aes(x = values, y = mean, ymin = min, ymax = max)) + 
  geom_line(colour = '#440154FF') + 
  geom_ribbon(fill = '#440154FF', alpha = 0.2) +
  geom_rug(data = p_df, inherit.aes = F, aes(values), sides = 't', color = '#FDE725FF', linewidth = 0.3) + 
  geom_rug(data = a_df, inherit.aes = F, aes(values), sides = 'b', color = '#21908CFF', linewidth = 0.3) + 
  labs(x = NULL, y = 'Logistic output') + ylim(0, 1) +
  theme(panel.background = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = 'black', fill = NA, linewidth = 0.2),
        panel.grid.major = element_line(linewidth = 0.2, colour = 'grey90'),
        strip.background = element_rect(fill = 'transparent'),
        strip.text = element_text(vjust = -0.5, size = 9),
        plot.margin = unit(c(0, 0, 0.2, 0.2), 'cm'),
        axis.title = element_text(size = 10)) +
  facet_wrap(~var, scales = 'free_x', labeller = VAR_names, nrow = 2) 
ggsave('Figure S3.4b.pdf', width = 20, height = 12, units = 'cm')


#------------------------------------ END ---------------------------------------




