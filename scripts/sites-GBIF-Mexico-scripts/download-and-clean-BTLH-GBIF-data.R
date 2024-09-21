# Download and clean GBIF data for BTLH sightings in Mexico
# Gaby Samaniego
# gabysamaniego@arizona.edu
# 2024-05-29

# Load packages
library(tidyverse)
library(rgbif)
library(CoordinateCleaner) # https://www.gbif.org/data-use/5lkEzTJaUPAZyCGUC3PDKC/coordinatecleaner-fast-and-standardized-cleaning-of-species-occurrence-data

# Clear environment
rm(list = ls()) 

#------------------------------------------------------------------------------#
# Code between lines 23 and 56 needs to be run just once per user

# 1. You will need your GBIF credential (email, username and password)
# 2. To avoid downloading the data from GBIF every time we run this code, you can
# run lines 29 to 57 once to download the data on your end, or you can load the 
# csv file I downloaded by running line 64

# 1. Run lines 23 and 24 just once to allow rgbif access to GBIF credentials 
install.packages('usethis')
usethis::edit_r_environ()

# 2. Download GBIF occurrence data for Broad-tailed Hummingbird (BTLH) in Mexico

# Get taxon key for BTLH
BTLH <- name_backbone('Selasphorus platycercus')$usageKey

# Download data
BTLH.download <- occ_download(
  pred_default(),
  pred('taxonKey', BTLH),
  format = 'SIMPLE_CSV',
  pred_gte("year", 1940), # Records after year 1940
  pred('country','MX')) # Occurrences in Mexico
  
# pred_default is equal to running:
# pred_and(
# pred("HAS_GEOSPATIAL_ISSUE",FALSE), Remove default geospatial issues 
# pred("HAS_COORDINATE",TRUE), Keep only records with coordinates
# pred("OCCURRENCE_STATUS","PRESENT"), Remove absent records
# pred_not(pred_in("BASIS_OF_RECORD",
#                 c("FOSSIL_SPECIMEN","LIVING_SPECIMEN")))) Remove fossils and living specimens

# Refer to this link for more information about pred_default filter
# https://docs.ropensci.org/rgbif/articles/getting_occurrence_data.html

# Check for download status
occ_download_wait(BTLH.download)

# Retrieve download data 
dat<- BTLH.download %>% 
  occ_download_get() %>%
  occ_download_import()

# Export csv with downloaded data
write.csv(dat, 'output/GBIF-Mexico-data/GBIF-BTLH-all-sightings-Mexico-raw-data.csv',
          row.names = FALSE)

#------------------------------------------------------------------------------#

# Load downloaded data from output folder
dat <- read.csv('output/GBIF-Mexico-data/GBIF-BTLH-all-sightings-Mexico-raw-data.csv')

# Filter data set using recommended filters in:
# https://data-blog.gbif.org/post/gbif-filtering-guide/
dat1 <- dat %>% 
  filter(coordinatePrecision < 0.01 | is.na(coordinatePrecision)) %>% # don't really need it for this data set, all coordinatePrecison values are NA. Removes 0 records
  filter(coordinateUncertaintyInMeters < 10000 | is.na(coordinateUncertaintyInMeters)) %>% # not sure about this one. Removes 12 records
  filter(!coordinateUncertaintyInMeters %in% c(301,3036,999,9999)) %>%  # Known inaccurate default values https://github.com/ropensci/CoordinateCleaner/issues/55. Removes 11 records
  filter(!decimalLatitude == 0 | !decimalLongitude == 0) %>% # removes points plotted along the prime meridian or equator, don't need it. Removes 0 records
  cc_cen(buffer = 2000) %>% # Removes country centroids within 2km. Here 9 records 
  cc_cap(buffer = 2000) %>% # Removes capitals centroids within 2km. Here 0 records
  cc_inst(buffer = 2000) %>% # Removes zoo and herbaria within 2km. Here 125 records
  cc_sea() %>% # Removes from ocean. Here 13 records 
  distinct(decimalLongitude, decimalLatitude, .keep_all = TRUE) # Removes location duplicates. Here 5786 records

# Check for elevation data
dat1 %>% 
  count(elevation)
# 2759 records don't have elevation data and 64 have 0 as elevation 

# Add elevation data to all points using elevation() from rgbif. 

# The function uses the GeoNames web service, to which we need a username. 
# See help page for details and link to website
?elevation()

# The function can use one of four different elevation models.
# I was curious to see what was the difference between the elevation data between 
# models, so I downloaded all four and merged and compared them in a new data frame

# The process of downloading the data can take some time, so I'm reading the csv
# with the merged information for the four models and all GBIF locations
elevation_comparison <- read.csv('data/GBIF-download-BTLH-Mexico/elevation-models-comparison.csv')

# I just looked at the data frame and the difference of the resulting elevations
# between models is minimal

# For now, I'm using the default srtm3 model. Sample area ca 90m x 90m
dat_model_1 <- elevation(dat1, username = 'gabysamaniego')

# Filter data to reflect BTLH occurrence in its wintering grounds  
dat2 <- dat_model_1 %>% 
  filter(elevation_geonames >= 1500, # BTLH elevation range in winter grounds, Graham et al. 2016
         elevation_geonames != -32768, # Removes ocean areas created by elevation()
         month %in% c(11,12,1,2,3), # November to March. For now consider full months
         year %in% c(1940:2012)) # Until 2012, this is the last year we have data 
        
count(dat2, year)

# Export the full data frame including elevation for all points
write.csv(dat2, 'output/GBIF-Mexico-data/GBIF-BTLH-winter-sightings-Mexico-with-elevation.csv',
          row.names = FALSE)

# Plot GBIF points in BTLH wintering grounds in Mexico

# Load packages
library(terra)
library(tidyterra)
library(ggplot2)
library(sf)

# Import non breeding range of BTLH from eBird
range <- vect("data/sites-BTLH-range-map-dem/brthum_range_2021.gpkg")
data.frame(range)
nonbreeding <- subset(range, range$season == 'nonbreeding')

# Quick look at non breeding range
plot(nonbreeding, col = "gray")

# Get points
locs <- vect(dat2, 
             geom = c('longitude', 'latitude'), 
             crs = crs(nonbreeding))

# Plot points
ggplot() + 
  geom_spatvector(data = nonbreeding, fill = "lightblue", color = NA) +
  borders("world", colour = 'black') +
  borders("state", colour = 'black') +
  geom_spatvector(data = locs) +
  coord_sf(expand = FALSE, xlim = c(-120, -86),  ylim = c(35, 10)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 9)) +
  labs(title = 'Broad-tailed hummingbird wintering distribution range',
       subtitle = 'including GBIF winter observations') +
  theme(plot.title = element_text(color = "black", size = 12, hjust = 0.5),
        plot.subtitle = element_text(color = "black", size = 12, hjust = 0.5))

# Export data points to use to download weather variables
points <- dat2 %>% 
  select(locality, latitude, longitude, elevation_geonames)

write.csv(points, 'output/GBIF-Mexico-data/BTLH-locations-MX.csv',
          row.names = FALSE)
