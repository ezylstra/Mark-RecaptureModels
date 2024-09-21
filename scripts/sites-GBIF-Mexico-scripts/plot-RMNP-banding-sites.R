# Plot RMNP banding sites and calculate pairwise distances between points 
# Gaby Samaniego
# gabysamaniego@arizona.edu
# 2024-01-04

# Load packages
library(terra)
library(tidyterra)
library(ggplot2)
library(ggspatial) # To add scale bar on map
library(sf)

# Clear environment
rm(list = ls())

# Import breeding range of Broad-tailed Hummingbird from eBird
range <- vect("data/sites-BTLH-range-map-dem/brthum_range_2021.gpkg")
data.frame(range)
breeding <- subset(range, range$season == 'breeding')

# Quick look at breeding range
plot(breeding, col = 'gray')

# Import all sampling locations
sites <- read.csv('data/sites-BTLH-range-map-dem/RMNP-sites-data.csv')

# Code to fix location of the label on map, from Erin on a different script 
sites <- sites %>%
  mutate(nudge_lat = latitude + 0.01,
         nudge_log = longitude + 0.02)

# Remove sites that won't be part of the analysis
sites <- sites %>% 
  filter(site != 'BGMD', # Site has one Rufus Hummingbird banded
         site != 'CLP') # Site doesn't have any banded birds
          
# Convert site locations to a SpatVector (spatial object)
locs <- vect(sites, geom = c('longitude', 'latitude'), crs = crs(breeding))

# Plot RMNP banding sites
ggplot() + 
  geom_spatvector(data = breeding, fill = 'lightsalmon', color = NA) +
  borders("world", colour = 'black') +
  borders("state", colour = 'black') +
  geom_spatvector(data = locs) +
  coord_sf(expand = FALSE, xlim = c(-105.4, -106),  ylim = c(40.1, 40.6)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 9)) +
  labs(title = 'Broad-tailed hummingbird breeding distribution range',
       subtitle = 'including RMNP banding sites') +
  geom_text(data = sites,
            aes(x = nudge_log, 
                y = nudge_lat, # Moves the label, so it is not on top of the point 
                group = NULL,
                fill = NULL,
                label = site),
            color = 'black',
            size = 3) +
  theme(plot.title = element_text(color = 'black', size = 12, hjust = 0.5),
        plot.subtitle = element_text(color = 'black', size = 12, hjust = 0.5)) +
  annotation_scale()

# Calculate pairwise distances between points 
# Code from Erin

# Add a site number to help organize things later
sites$site_num <- 1:nrow(sites)

# Convert site locations to a SpatVector (spatial object)
locations <- vect(sites, geom = c('longitude', 'latitude'), crs = 'EPSG:4326')

# Calculate distance between all pairs of sites, in km
dists <- distance(locations, unit = 'km', pairs = TRUE, symmetrical = FALSE)

# Note: by setting symmetrical = FALSE we're including pair once in 
# the data frame (ie, distance between A and B is included, but not distance
# between B and A). Could set = TRUE if you want to include both. 
dists <- as.data.frame(dists)

# Add site names to distance data frame for convenience
dists$from_name <- sites$site[match(dists$from, sites$site_num)]
dists$to_name <- sites$site[match(dists$to, sites$site_num)]

# Explore distance data
summary(dists)

# Max distance between all points is 41.5 km and minimum is 0.1 km, the average 
# distance between all sites is 19.7 km. Sites are really close together. 

# Add site location (east or west) to distance data frame
dists$location_from <- sites$location[match(dists$from, sites$site_num)]
dists$location_to <- sites$location[match(dists$to, sites$site_num)]

# Explore distances at each side of the park: east and west to the Continental
# Divide

# East sites
east_sites <- sites %>% 
  filter(location == 'east')

# Add a site number to help organize things
east_sites$site_num <- 1:nrow(east_sites)

# Convert east site locations to a SpatVector (spatial object)
locs_east <- vect(east_sites, geom = c('longitude', 'latitude'), crs = 'EPSG:4326')

# Calculate distance between all pairs of sites, in km
dists_east <- distance(locs_east, unit = 'km', pairs = TRUE, symmetrical = FALSE)
dists_east <- as.data.frame(dists_east)

# Add site names to distance data frame for convenience
dists_east$from_name <- east_sites$site[match(dists_east$from, east_sites$site_num)]
dists_east$to_name <- east_sites$site[match(dists_east$to, east_sites$site_num)]

# Explore distance data (very basic)
summary(dists_east)

# Max distance between points on the east part of the park is 31.7 km and minimum 
# is 0.1 km, the average distance between these sites is 14.6 km 

# From the cluster of points in the east site:

# Sites WB1 and WB2 are 0.1 km apart, basically they are at the same location
# Sites HPK1 and HPK2 are 1.4 km apart, and these sites to site MP1 are 1.5 and 
# 2 km apart respectably.   
# Sites CC2 and MCGC are 1.3 km apart, and these sites to site to WC1 are 2.4 and
# 2.2 km apart respectably.
# Sites CC2 and CC3 are 0.3 km apart, very close.
# Site CC3 and MCGC ara 1.6 km apart. 

# West sites
west_sites <- sites %>% 
  filter(location == 'west')

# Add a site number to help organize things
west_sites$site_num <- 1:nrow(west_sites)

# Convert east site locations to a SpatVector (spatial object)
locs_west <- vect(west_sites, geom = c('longitude', 'latitude'), crs = 'EPSG:4326')

# Calculate distance between all pairs of sites, in km
dists_west <- distance(locs_west, unit = 'km', pairs = TRUE, symmetrical = FALSE)
dists_west <- as.data.frame(dists_west)

# Add site names to distance data frame for convenience
dists_west$from_name <- west_sites$site[match(dists_west$from, west_sites$site_num)]
dists_west$to_name <- west_sites$site[match(dists_west$to, west_sites$site_num)]

# Explore distance data (very basic)
summary(dists_west)

# Max distance between points on the west part of the park is 25.4 km and minimum 
# is 4.4 km, the average distance between these sites is 12.6 km 
# In general these sites are farther apart from each other than some of the east
# sites, but still they are pretty close together. 
# These sites are not clustered as some of the east sites.

# For now we are going to include all data from all sites in the survival analysis 
# ignoring their location in the park and effort at each site 

