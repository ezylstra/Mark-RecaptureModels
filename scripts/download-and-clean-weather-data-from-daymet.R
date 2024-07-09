# Download weather data from Daymet for BTLH sightings in Mexico
# Gaby Samaniego
# gabysamaniego@arizona.edu
# 2024-07-03

# Load packages
library(tidyverse)
library(terra)
library(daymetr)
library(FedData)
library(sp)
library(raster)
library(exactextractr) # Used to extract or summarize values of raster cells

# Clear environment
rm(list = ls())

# Using this website:
# https://tmieno2.github.io/R-as-GIS-for-Economists/download-data.html
# for the example code to download Daymet data

# Daymet data consists of “tiles,” each of which consisting of raster cells of 
# 1km by 1km.

# List of climate variables:
# vapor pressure
# minimum and maximum temperature
# snow water equivalent
# solar radiation
# precipitation
# day length

# Load points data 
sites <- read.csv('output/GBIF-BTLH-winter-sightings-Mexico-with-elevation.csv')

# Clean up data set
sites <- sites %>% 
  dplyr::select(latitude, longitude) %>% 
  mutate(code = paste0('s', 1:235)) %>% 
  filter(code %in% c('s1','s2')) # just to try the code

# Download Daymet data for points

# Loop through sites (points)
get_daymet <- function(i) {
  lat <- sites[i, ] %>% pull(latitude)
  lon <- sites[i, ] %>% pull(longitude)
  site <- sites[i, ] %>% pull(code)
  
  daymet <- download_daymet(
    lat = lat,
    lon = lon,
    start = 2001, # Used two years to try the code
    end = 2002
  ) %>%
    .$data %>% # Just get climate data
    mutate(site = site) %>% # Assign site_id so we know which record is for which site
    mutate(date = as.Date(paste(year, yday, sep = "-"), "%Y-%j")) # Get date from day of the year
  
    return(daymet)
}

daymet_dat <- lapply(1:nrow(sites), get_daymet) %>%
  bind_rows() # Combine the list of data.frames into a single data.frame

# My first tries of downloading the data worked, but I didn't export them. 
# I tried to download it again but I got an error:
# Error in curl::curl_fetch_disk(url, x$path, handle = handle) : 
# Timeout was reached: [daymet.ornl.gov] SSL/TLS connection timeout
# I googled the error, but it was not clear to me what is happening. It might be
# that my connection is not great. I used the UA VPN, but it didn't help. 
# You might be able to download the data at your end, my internet is not good

# Update: This morning I was able to download the data very fast and without
# any errors. I still hink that I should go to campus to use a better internet 
# coneccion. 

# Clean data set
daymet_clean <- daymet_dat %>% 
  dplyr::select(-yday) %>% 
  mutate(month = as.numeric(str_sub(date, 6, 7)),
         day = as.numeric(str_sub(date, 9, 10))) %>% 
  rename( dayl = dayl..s., # day length s/day
          prcp = prcp..mm.day., # precipitation mm/day
          srad = srad..W.m.2., # shortwave radiation W/m2
          swe = swe..kg.m.2., # snow water equivalent kg/m2
          tmax = tmax..deg.c., # maximum air temperature C
          tmin = tmin..deg.c., # minimum air temperature C
          vp = vp..Pa.) %>%  # water vapor pressure Pa 
  relocate(site, date, year, month, day)

# Calculate the annual values for the model covariates 
climate_cov <- daymet_clean %>% 
  filter(between(date, 
                 as.Date('2001-11-01'), # November
                 as.Date('2002-03-31'))) %>% # March
  group_by(year) %>% 
  summarise(an_mean_prcp = mean(prcp),
            an_mean_tmin = mean(tmin),
            an_mean_tmax = mean(tmax),
            an_min_temp = min(tmin),
            ann_max_tem = max(tmax))

# This is my first exploration of the climate data, therefore these covariates
# are not the final ones. I need to think better how to group the data, because 
# the winter months overlap two years (November 2001 to March 2002) so grouping
# by year won't work. I should filter by date first (between dates) and then assign
# a new name to those filtered rows. Something like 'winter period' '2001-2002'
# For now I'm going to leave this script like this.
  
# For now, I exported the clean data 
write.csv(daymet_clean, 'output/cleaned-daymet-data-BTLH-sightings-Mexico.csv')

# ---------------------------------------------------------------------------- #

# Original code from Erin Zylstra
# 2023-06-02 
# I edited to use the sites data frame (s1 to s3)

# Create SpatVectors (data type) with site locations
allsites <- terra::vect(sites, 
                        geom = c('longitude', 'latitude'), 
                        crs = "epsg:4326")

# Create buffered SpatVectors (polygons) 
# Calculate a buffer around the geometries of the SpatVector allsites
# 3 km was HMN's requirement based on their banding sites. Do we need a buffer?
# I don't think for our purposes we need it since we have a lot of points in the
# wintering grounds. I might be wrong, What do you think?
allsites_b <- terra::buffer(allsites, 
                            width = 3000)

# Convert SpatVectors (polygons) to sp objects
allsites_b_sp <- as(allsites_b, "Spatial")

# Years
years <- 2001 # one year just to try the code

#------------------------------------------------------------------------------#
# Extract data from daymet
#------------------------------------------------------------------------------#

variables <- c('prcp', 'tmin', 'tmax')
# prcp = Precipitation (mm)
# tmin = Minimum 2-m air temperature (C)
# tmax = Maximum 2-m air temperature (C)

# Can use FedData::get_daymet to download and crop daymet data.
# Will want to do this for each buffered area separately since if I use the 
# layer with all polygons, it crops to the extent across all polygons, resulting 
# in an unwieldy raster.

# Loop through sites/polygons
for (site in allsites$code) {
  buffer <- allsites_b_sp[allsites_b_sp@data$code == site, ]
  
  dm_extract <- FedData::get_daymet(template = buffer,
                                    label = site,
                                    elements = c("prcp", "tmin", "tmax"), 
                                    years = years, 
                                    extraction.dir = paste0("daymet/", site, "/"),
                                    force.redo = TRUE)
  # Results in a list of RasterStacks, one for each variable
  # Each brick will have few raster cells (~ 8 x 8), but a layer for each day
  
  dm_dates <- names(dm_extract[[1]])
  dm_year <- as.numeric(str_sub(dm_dates, 1, 4))
  dm_month <- as.numeric(str_sub(dm_dates, 6, 7))
  dm_day <- as.numeric(str_sub(dm_dates, 9, 10))
  
  for (i in 1:length(dm_extract)) {
    # Calculating mean daily values across the polygon
    # Suppressing warning messages about transforming polygon CRS
    dm_var <- suppressWarnings(exact_extract(dm_extract[[i]], buffer, "mean"))
    # Results in a dataframe with 1 row, column for each date
    dm_data <- data.frame(code = site,
                          var = as.vector(t(dm_var)),
                          year = dm_year,
                          month = dm_month,
                          day = dm_day)
    
    names(dm_data)[names(dm_data) == "var"] <- variables[i]
    
    if (i == 1) {
      dm_allvar <- dm_data
    } else {
      dm_allvar <- left_join(dm_allvar, 
                             dm_data, 
                             by = c("code", "year", "month", "day"))
    }
  }
  if (site == allsites$code[1]) {
    daymet <- dm_allvar
  } else {
    daymet <- rbind(daymet, dm_allvar)
  }
}

# Rearrange, rename climate columns
daymet <- daymet %>% 
  rename(precip.mm = prcp,
         temp.min.C = tmin,
         temp.max.C = tmax) %>%
  left_join(sites[, c("code", "latitude", "longitude")], by = c("code")) %>%
  relocate(latitude, .after = code) %>%
  relocate(longitude, .after = latitude) %>%
  relocate(any_of(c("year", "month", "day")), .after = longitude)

# Write daymet data to file
write.csv(daymet, 'output/daymet-data-polygons.csv', row.names = FALSE)

# It took quite some time to download the data for one year and two sites

# To download the data, either way, if we need to create the buffer around our 
# points (polygons) or just use the points, we have a lot of points in the wintering 
# grounds (235).

# Do you think it is worth exploring the points to reduce their number based on 
# distance between them? I think my connection is the problem for downloading the 
# data. I could try the code again at campus on Thursday 
