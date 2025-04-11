# Thinning the number of winter and summer points based in daymet cell ID
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

#---------------------------------- Winter Points -----------------------------#

# Winter months are December, January, and February

# Load winter sites
winter.sites <- read.csv('data/GBIF-download-BTLH-Mexico/filtered-GBIF-BTLH-winter-sites-with-names.csv',
                         strip.white = TRUE)

# Filter data set to include just observations that occurred during winter months
winter <- winter.sites %>% 
  filter(month %in% c(12, 1, 2))

# Convert filtered winter sites to an sf object 
# crs WGS84 so it matches the tile_outlines later in the code
locs.winter <- st_as_sf(winter,
                        coords = c('longitude', 'latitude'), 
                        crs = 4326) # WGS84

# Thin the number of points based on daymet cells to avoid duplicated weather data

# 1) Download all daymet tiles in winter area

# First create a bounding box using all of the winter points to identify the 
# needed tiles
bb.winter <- st_bbox(locs.winter)

# Convert the bounding bbox to an sf polygon
bb.poly.winter <- st_as_sfc(bb.winter, crs = st_crs(locs.winter))

# Find Which tiles intersect the bounding box
tiles.needed.winter <- tile_outlines[st_intersects(tile_outlines,
                                                   bb.poly.winter, 
                                                   sparse = FALSE), ]
# tile_outlines is a function built in the daymetr package containing the outlines
# of all daymet tiles and their projection information

# Separate the tile's numbers
tile.ids.winter <- tiles.needed.winter$TileID

# Loop to download the tiles based on the tiles' IDs 
for (tid in tile.ids.winter) {
  download_daymet_tiles(
    tiles =  tid, 
    start  = 2002, 
    end    = 2002,
    param  = 'tmax',
    path   = 'output/weather-data/daymet-tiles-winter/')
}

# 2) Create a raster collection with all downloaded tiles

# Read all tiles
tile.list.winter <- list.files('output/weather-data/daymet-tiles-winter/', 
                               pattern = "\\.nc$", 
                               full.names = TRUE)

# Create a list to add all the rasters
ras.list.winter  <- list()

# Loop trough the tiles to read them as rasters
for (t in tile.list.winter) {
  ras <- terra::rast(t)
  # Select first day of data
  ras <- ras[[1]]
  # Assign daymet LCC projection
  ras <- terra::project(ras, '+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +datum=WGS84 +units=m')
  ras.list.winter[[t]] <- ras
}

# Convert the list of rasters to a SpatRaster Collection
ras.collection.winter <- terra::sprc(ras.list.winter)

# Merge everything into a single raster
ras.all.merge <- terra::merge(ras.collection.winter)

# Plot
plot(ras.all.merge)

# 3) Thin the winter points 

# Convert filtered winter sites to a SpatVector
locs.vect.winter <- vect(winter,
                         geom = c('longitude', 'latitude'), 
                         crs = 'EPSG:4326')

# Reproject points to daymet crs
points.winter <- project(locs.vect.winter,
                         '+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +datum=WGS84 +units=m')

# Plot to check 
ggplot() + 
  geom_spatraster(data = ras.all.merge, fill = "lightblue") +
  geom_spatvector(data = points.winter, color = 'red') 

# Add the cell number to data frame
cell.number.winter <- extract(ras.all.merge,
                              points.winter,
                              cell = TRUE,
                              ID = FALSE)

# Bind the cell numbers to the original dataframe 
winter <- cbind(winter, cell = cell.number.winter$cell)

# Thin the points 
thinned.points.winter <- winter %>%
  group_by(cell) %>%
  slice(1) %>%  
  ungroup()

# Export list of thinned points
write.csv(thinned.points.winter, 'data/sites-BTLH-range-map/thinned-winter-sites.csv',
          row.names = F)

#----------------------------------- Summer Sites -----------------------------#

# Load summer sites
summer.sites <- read.csv('data/sites-BTLH-range-map/RMNP-sites-data.csv',
                         strip.white = TRUE) %>% 
  filter(!site %in% c('CLP', 'BGMD'))

# Convert summer sites to an sf object 
# crs WGS84 so it matches the tile_outlines later in the code
locs.summer <- st_as_sf(summer.sites, 
                        coords = c('longitude', 'latitude'), 
                        crs = 4326) # WGS84

# Explore if summer points are in the same daymet cell

# 1) Download daymet tiles in summer area

# First create a bounding box using all summer sites to identify the needed tiles
bb.summer <- st_bbox(locs.summer)

# Convert the bounding bbox to an sf polygon
bb.poly.summer <- st_as_sfc(bb.summer, crs = st_crs(locs.summer))

# Find Which tiles intersect the bounding box
tiles.needed.summer <- tile_outlines[st_intersects(tile_outlines,
                                                   bb.poly.summer, 
                                                   sparse = FALSE), ]

# Separate the one tile ID that contains all summer sites
tile.id.summer <- tiles.needed.summer$TileID

# Download tile from Daymet
download_daymet_tiles(tiles = tile.id.summer,
                      start = 2002, 
                      end = 2002,
                      param = 'tmax', 
                      path = 'output/weather-data/daymet-tiles-summer/') 

# Read the tile as a raster
tile.raster.summer <- terra::rast('output/weather-data/daymet-tiles-summer/tmax_2002_11738.nc',
                                  lyrs = 1) # Just first day of data

# Assign the LCC Projection (the correct daymet projection parameters):
crs(tile.raster.summer) <- '+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +datum=WGS84 +units=m'

# 3) Explore if the summer sites are in the same daymet cells 

# Convert summer sites to a SpatVector
locs.vect.summer <- vect(summer.sites,
                         geom = c('longitude', 'latitude'), 
                         crs = 'EPSG:4326')

# Reproject points to daymet crs
summer.points <- project(locs.vect.summer,
                         '+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +datum=WGS84 +units=m')

# Plot to check 
ggplot() + 
  geom_spatraster(data = tile.raster.summer, fill = "lightpink") +
  geom_spatvector(data = summer.points, color = 'red') 

# Add the cell number to data frame
cell.number.summer <- extract(tile.raster.summer,
                              summer.points, 
                              cell = TRUE,
                              ID = FALSE)

# Bind the cell numbers to the original data frame 
locs.summer <- cbind(summer.sites, cell = cell.number.summer$cell)

# Thin the points 
thinned.summer.locs <- locs.summer %>%
  group_by(cell) %>%
  slice(1) %>%  
  ungroup()
# Just one location removed WB2 

# Export list of thinned points
write.csv(thinned.summer.locs, 'data/sites-BTLH-range-map/thinned-summer-sites.csv',
          row.names = F)
