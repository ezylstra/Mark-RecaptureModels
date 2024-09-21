# Download weather data from Daymet and NDVI from MODIS
# Gaby Samaniego
# gabysamaniego@arizona.edu
# 2024-07-03

# Load packages
library(tidyverse)
library(terra)
library(daymetr)
library(FedData)
library(MODISTools) 

# Clear environment
rm(list = ls())

# Load points data 
sites_winter <- read.csv('output/GBIF-Mexico-data/GBIF-BTLH-winter-sightings-Mexico-with-elevation.csv')
sites_summer <-read.csv('data/sites-BTLH-range-map-dem/RMNP-sites-data.csv')

# Clean up data sets
sites_winter <- sites_winter %>% 
  dplyr::select(latitude, longitude) %>% 
  mutate(code = paste0('ws', 1:235)) # ws = winter sites

sites_summer <- sites_summer %>% 
  dplyr::select(latitude, longitude, site) %>% 
  filter(!site %in% c('CC2', 'HPK2', 'WB1')) %>%  
  # These sites are < 1 km to the nearest site, removing them to avoid duplicates
  mutate(code = paste0('ss', 1:17))  # ss = summer sites

# Merge data sets
sites <- full_join(sites_winter, sites_summer)

# ------------------------------ Daymet data --------------------------------- #

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

# Download Daymet data for points

# For points data, download_daymet() will directly return weather values to the 
# point of interest. Internally, it finds the cell in which the point is located, 
# and then return the values of the cell for the specified length of period.

# Loop through sites (points)
get_daymet <- function(i) {
  lat <- sites[i, ] %>% pull(latitude)
  lon <- sites[i, ] %>% pull(longitude)
  site <- sites[i, ] %>% pull(code)
  
  daymet <- download_daymet(
    lat = lat,
    lon = lon,
    start = 2002,
    end = 2012
  ) %>%
    .$data %>% # Just get climate data
    mutate(site = site) %>% # Assign site_id so we know which record is for which site
    mutate(date = as.Date(paste(year, yday, sep = "-"), "%Y-%j")) # Get date from day of the year
   
    return(daymet)
}

daymet_dat <- lapply(1:nrow(sites), get_daymet) %>%
  bind_rows() # Combine the list of data.frames into a single data.frame

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

# Create csv with downloaded data 
write.csv(daymet_clean, 'output/weather-data/cleaned-daymet-data-all-sites.csv',
          row.names = FALSE)

# --------------------------------- MODIS data ------------------------------- #

# Original code by Jeff Oliver 
# jcoliver@arizona.edu
# 2023-06-14

# Edited by Gaby Samaniego
# 2024-07-17

# Product and band for NDVI
ndvi_product <- "MOD13Q1"
ndvi_band <- "250m_16_days_NDVI"

# MODISTools needs lat and lon
# We also bound the search by the beginning of the first active year (Jan 1) 
# and the end of the last active year (Dec 31)
modis_sites <- sites %>% 
  mutate(sdate = '2001-01-01',
         edate = '2012-12-31')

# Loop to run query for each row in sites data frame. Yes, we hate loops, but 
# they aren't the time suck here. Querying via MODISTools is glacial.

# For development, just use the first two rows of data
# query_sites <- modis_sites[1:2, ]

# The full data set
query_sites <- modis_sites[51:252, ] # I used chunks of points to download the data
ndvi_list <- vector(mode = "list", length = nrow(query_sites))
for (site_i in 1:nrow(query_sites)) {
  code <- query_sites$code[site_i]
  message("Running query for site ", code, 
          " (", site_i, " of ", nrow(query_sites), ")")
  # Run the query
  ndvi_query <- mt_subset(product = ndvi_product,
                          band = ndvi_band,
                          lat = query_sites$latitude[site_i],
                          lon = query_sites$longitude[site_i],
                          start = query_sites$sdate[site_i],
                          end = query_sites$edate[site_i])
  # Extract date and re-scale NDVI scores
  # scale column comes back as numeric, so wrap in as.numeric
  result <- ndvi_query %>%
    mutate(ndvi = as.numeric(scale) * value) %>%
    mutate(site_name = code) %>%
    select(site_name, calendar_date, ndvi)
  rownames(result) <- NULL
  ndvi_list[[site_i]] <- result
  # Should be unnecessary, but for some reason paranoid about memory today?
  rm(ndvi_query, result)
  gc()
}

# Bundle all the results back together
all_ndvi <- ndvi_list %>%
  bind_rows()
rownames(all_ndvi) <- NULL

# Create csv with downloaded data
write.csv(all_ndvi, 'output/weather-data/ndvi-data-50.2-sites.csv',
          row.names = FALSE)

