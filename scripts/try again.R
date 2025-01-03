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

# Define lat and lon for point 
lat <- winter[10, ] %>% pull(latitude)  
lon <- winter[10, ] %>% pull(longitude)

# Download tile from Daymet
tile <- download_daymet_tiles(location = c(lat, lon),
                              start = 2002, 
                              end = 2002,
                              param = 'tmax', 
                              path = 'output/weather-data/') 

# Read the netCDF file downloaded in line 72 as a raster
tile.raster <- terra::rast('output/weather-data/tmax_2002_9758.nc')

# Assign the LCC Projection (the correct daymet projection parameters):
crs(tile.raster) <- '+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +datum=WGS84 +units=m'

# Convert the points to an sf object
locs <- st_as_sf(winter,
                 coords = c('longitude', 'latitude'), 
                 crs = 4326)

# Transform points to the LCC Projection used by daymet data
points.lcc <- st_transform(locs,
  crs = '+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +datum=WGS84 +units=m')

# Are the tile and the points in the same crs
crs(tile.raster) == crs(points.lcc)

# Create a bounding box for all points in winter grounds
points.bbox <- st_bbox(points.lcc)
points.bbox
# This gives c(xmin, ymin, xmax, ymax)

# Create a dummy empty SpatRaster with daymet’s 1x1km alignment that covers the 
# bbox of all points, has the same resolution that the downloaded daymet tile, 
# and uses the daymet LCC projection
# This should make the grid the same as daymet data, right?
dummy.raster <- rast(
  xmin = points.bbox["xmin"],
  xmax = points.bbox["xmax"],
  ymin = points.bbox["ymin"],
  ymax = points.bbox["ymax"],
  resolution = res(tile.raster),            # (1000, 1000)
  crs = crs(tile.raster))                   # the LCC projection

# Force the origin so it matches exactly the tile origin
# Sets the coordinates of the point of origin of a SpatRaster.
origin(dummy.raster) <- origin(tile.raster)

# Convert the raster to a 1x1 polygons
daymet.grid.poly <- as.polygons(dummy.raster, 
                                dissolve = FALSE)

# Convert grid to a sf object
daymet.grid.sf  <- st_as_sf(daymet.grid.poly)

# Assign the points to the daymet cells so we know which daymet cell they fall into 
points.lcc$daymet_id <- st_within(points.lcc, daymet.grid.sf) %>% 
  as.integer()

# Thin the points so there is just one per 1x1 km cell
thinned.points <- points.lcc %>%
  group_by(daymet_id) %>%
  slice(1) %>%  # Keep just the first point in each cell
  ungroup()

# Export list of thinned points
write.csv(thinned.points, 'output/weather-data/thinned-points-try.csv')

# Plot results
ggplot() +
  geom_sf(data = daymet.grid.sf, fill = NA, color = "grey") +
  geom_sf(data = thinned.points, color = "blue") +
  theme_minimal()


