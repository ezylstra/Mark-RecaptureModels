# Thinning the number of winter points based in daymet grid
# Gaby Samaniego
# gabysamaniego@arizona.edu
# 2024-12-20

# Load packages
library(tidyverse)
library(terra)
library(tidyterra)
library(ggplot2)
library(sf)
library(daymetr)

# Clear environment
rm(list = ls())

# -------------------------- DEFINE WINTER LOCATIONS ------------------------- #

# Winter months are December, January, and February

# Load winter sites
winter.sites <- read.csv('output/GBIF-Mexico-data/filtered-GBIF-BTLH-winter-sites-with-names.csv',
                         strip.white = TRUE)

# Filter data set to include just observations that occurred during winter months
winter <- winter.sites %>% 
  filter(month %in% c(12, 1, 2))

# Convert filtered winter sites to an sf object 
# crs WGS84 so it matches the tile_outlines later in the code
locs <- st_as_sf(winter,
                 coords = c('longitude', 'latitude'), 
                 crs = 4326) # WGS84

# Thin the number of points based on daymet cells to avoid duplicates

# 1) Download all daymet tiles in winter area

# First create a bounding box using all the winter to identify the tiles I need
bb <- st_bbox(locs)

# Convert the bounding bbox to an sf polygon
bb.poly <- st_as_sfc(bb, crs = st_crs(locs))

# Find Which tiles intersect the bounding box
tiles.needed <- tile_outlines[st_intersects(tile_outlines, 
                                            bb.poly, 
                                            sparse = FALSE), ]
# tile_outlines is a function built in the daymetr package containing the outlines
# of all daymet tiles and their projection information

# Separate the tile's numbers
tile.ids <- tiles.needed$TileID

# Loop to download the tiles based on the tiles' IDs 
for (tid in tile.ids) {
  download_daymet_tiles(
    tiles =  tid, 
    start  = 2002, 
    end    = 2002,
    param  = 'tmax',
    path   = 'output/weather-data/daymet-tiles-winter/')
}

# 2) Create a raster collection with all downloaded tiles

# Read all tiles
tile.list <- list.files('output/weather-data/daymet-tiles-winter/', 
                        pattern = "\\.nc$", 
                        full.names = TRUE)

# Create a list to add all the rasters
ras.list  <- list()

# Loop trough the tiles to read them as rasters
for (t in tile.list) {
  ras <- terra::rast(t)
  # Select first day of data
  ras <- ras[[1]]
  # Assign daymet LCC projection
  ras <- terra::project(ras, '+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +datum=WGS84 +units=m')
  ras.list[[t]] <- ras
}

# Convert the list of rasters to a SpatRaster Collection
ras.collection <- terra::sprc(ras.list)

# Merge everything into a single raster
ras.all.merge <- terra::merge(ras.collection)

# Plot
plot(ras.all.merge)

################################################################################ 
# Using the loop I tried before
full.daymet.rast <- ras.list[[1]]
for (i in 2:length(ras.list)) {
  full.daymet.rast <- mosaic(full.daymet.rast, 
                             ras.list[[i]])}
plot(full.daymet.rast)
# The loop ran fast but the plot doesn't look completely right 
################################################################################

# 3) Thin the winter points 

# Convert filtered winter sites to a SpatVector
locs.vect <- vect(winter,
                  geom = c('longitude', 'latitude'), 
                  crs = crs(ras.all.merge))

# Get the cell number 
cell.number <- extract(ras.all.merge,
                       locs.vect, 
                       cell = TRUE)

# Plot
ggplot() + 
  geom_spatraster(data = ras.all.merge, fill = "lightblue") +
  geom_spatvector(data = locs.vect, color = 'red') 

crs(locs.vect) == crs(ras.all.merge)
