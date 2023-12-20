################################################################################
# Trying to extract environmental data for HMN banding locations

# Updated 2 June 2023
################################################################################

library(tidyverse)
library(lubridate)
library(terra)
library(downloader)
library(FedData)
library(exactextractr) # Used to extract or summarize values of raster cells
library(raster)

rm(list = ls())

# Using this website:
# https://tmieno2.github.io/R-as-GIS-for-Economists/download-data.html
# for a lot of example code for downloading gridmet and daymet data

# Read in site data (got this from Kira's files with HMN locations)
sites <- read.csv("sites.csv", header = TRUE)

# Create SpatVectors with site locations
allsites <- terra::vect(sites, geom = c("lon", "lat"), crs = "epsg:4326")
usonly <- terra::subset(allsites, !allsites$state %in% c("MX", "BC"))

# Create buffered SpatVectors (polygons)
allsites_b <- terra::buffer(allsites, width = 3000)
usonly_b <- terra::buffer(usonly, width = 3000)

# Convert SpatVectors (polygons) to sp objects
allsites_b_sp <- as(allsites_b, "Spatial")
usonly_b_sp <- as(usonly_b, "Spatial")

# Years
yrs <- 2001:2022

#------------------------------------------------------------------------------#
# Extract data from gridmet
#------------------------------------------------------------------------------#

variables <- c("pr", "tmmn", "tmmx", "vs", "pet")
  # pr = Precipitation (mm)
  # tmmn = Minimum Near-Surface Air Temperature (K)
  # tmmx = Maximum Near-Surface Air Temperature (K)
  # vs = wind speed at 10 m (meters per sec)
  # pet = potential evapotraspiration (mm)

base_url <- "http://www.northwestknowledge.net/metdata/data/"
base_destination <- "gridmet/"

# Download all the relevant gridmet files (5 * 22 = 110 files)
# Takes a few seconds for each file
for (variable in variables) {
  for (yr in yrs) {
    filename <- paste0(variable, "_", yr)
    url <- paste0(base_url, filename, ".nc")
    destination_file <- paste0(base_destination, filename, ".nc")
    
    # Download .nc file
    downloader::download(url = url,
                         destfile = destination_file,
                         mode = "wb")
  }
}

# For each file, extract the mean value of the covariate in each polygon
for (variable in variables) {
  for (yr in yrs) {
    # Read in data
    varyr <- paste0(variable, "_", yr)
    temprast <- rast(paste0("gridmet/", variable, "_", yr, ".nc"))

    # Get mean value in each polygon (= buffered location) in US
    means <- exact_extract(temprast, usonly_b_sp, "mean")
    # This results in a 40 x 365 dataframe where each row is a polygon, each 
    # column a day of the year (day = 5-digit number, days since Jan 1, 1900).
    # Mean: will provide the mean cell value, weighted by the fraction of each 
    # cell that is covered by each polygon
    
    # Add site codes and put into long form
    means_l <- means %>%
      mutate(code = usonly_b$code) %>%
      pivot_longer(cols = -c(code), 
                   names_to = "dayno", 
                   values_to = variable) %>%
      mutate(dayno = as.numeric(str_sub(dayno, -5, -1)),
             date = dayno + lubridate::ymd("1900-01-01")) %>%
      dplyr::select(-dayno) %>%
      data.frame()
      # dataframe with site * day rows
    
    # Convert K to C
    if (variable == "tmmn") {
      means_l <- means_l %>% mutate(tmmn = tmmn - 273.15)
    }
    if (variable == "tmmx") {
      means_l <- means_l %>% mutate(tmmx = tmmx - 273.15)
    }
    
    if (yr == yrs[1]) {
      all_years <- means_l
    } else {
      all_years <- rbind(all_years, means_l)
    }
    
    if (yr == yrs[length(yrs)]) {
      assign(paste0("all_years_", variable), all_years)
    }
  }
}

# Merge data types, create new date and loc columns, rename climate columns
list_df <- mget(paste0("all_years_", variables))
gridmet <- list_df %>% 
  reduce(inner_join, by = c("code", "date")) %>%
  mutate(year = year(date),
         month = month(date),
         day = day(date)) %>%
  dplyr::select(-date) %>%
  rename(precip.mm = pr,    # Precipitation (mm)
         temp.min.C = tmmn, # Minimum Near-Surface Air Temperature (degC)
         temp.max.C = tmmx, # Maximum Near-Surface Air Temperature (degC)
         wind.mps = vs,     # Wind speed at 10 m (meters per sec)
         pet.mm = pet) %>%  # Potential evapotranspiration (mm)
  left_join(sites[, c("code", "lat", "lon")], by = c("code")) %>%
  relocate(lat, .after = code) %>%
  relocate(lon, .after = lat) %>%
  relocate(any_of(c("year", "month", "day")), .after = lon)

# Write gridmet data to file
# write.csv(gridmet, "gridmet.csv", row.names = FALSE)

#------------------------------------------------------------------------------#
# Extract data from daymet
#------------------------------------------------------------------------------#

variables <- c("prcp", "tmin", "tmax")
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
                                    years = 2001:2022, 
                                    extraction.dir = paste0("daymet/", site, "/"),
                                    force.redo = TRUE)
  # Results in a list of RasterStacks, one for each variable
  # Each brick will have few raster cells (~ 8 x 8), but a layer for each day
  
  dm_dates <- names(dm_extract[[1]])
  dm_year <- as.numeric(str_sub(dm_dates, 2, 5))
  dm_month <- as.numeric(str_sub(dm_dates, 7, 8))
  dm_day <- as.numeric(str_sub(dm_dates, 10, 11))

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
  left_join(sites[, c("code", "lat", "lon")], by = c("code")) %>%
  relocate(lat, .after = code) %>%
  relocate(lon, .after = lat) %>%
  relocate(any_of(c("year", "month", "day")), .after = lon)

# Write daymet data to file
# write.csv(daymet, "daymet.csv", row.names = FALSE)

#------------------------------------------------------------------------------#
# Combine data from both sources
#------------------------------------------------------------------------------#

daymet <- read.csv("daymet.csv", header = TRUE)
str(daymet)
summary(daymet)
  # 425,590 rows (53 sites * 22 yrs * 365 days)
  # 9 columns (3 climate vars)
  # Note that Daymet removes observations for December 31 in leap years to make 
  # the time dimension 365 consistently across years. 
  
gridmet <- read.csv("gridmet.csv", header = TRUE)
str(gridmet)
summary(gridmet)
  # 321,400 rows (40 sites * (22 yrs * 365 days + 5 leap))
  # 11 columns (5 climate vars)

# Neither data source has any missing data

# Merge data from both sources
gridmet <- gridmet %>%
  dplyr::select(-c(lat, lon)) %>%
  rename(precip.g = precip.mm,
         temp.min.g = temp.min.C,
         temp.max.g = temp.max.C,
         wind.g = wind.mps,
         pet.g = pet.mm)
daymet <- daymet %>%
  rename(precip.d = precip.mm,
         temp.min.d = temp.min.C,
         temp.max.d = temp.max.C)
alldata <- full_join(daymet, gridmet, 
                     by = c("code", "year", "month", "day"))
str(alldata)
  # 425,790 rows (53 sites * 22 yrs * 365 days + 40 sites * 5 leap))
  # 14 columns (8 climate vars)

# How do daymet data look compared to gridmet?
  cor_site <- alldata %>%
    group_by(code) %>%
    summarize(precip = cor(precip.d, precip.g, use = "pairwise.complete.obs"),
              temp.min = cor(temp.min.d, temp.min.g, use = "pairwise.complete.obs"),
              temp.max = cor(temp.max.d, temp.max.g, use = "pairwise.complete.obs")) %>%
    filter(!is.na(precip)) %>%
    mutate(across(precip:temp.max, \(x) round(x, 2))) %>%
    data.frame()

  # Correlations by year
  cor_yr <- alldata %>%
    group_by(year) %>%
    summarize(precip = cor(precip.d, precip.g, use = "pairwise.complete.obs"),
              temp.min = cor(temp.min.d, temp.min.g, use = "pairwise.complete.obs"),
              temp.max = cor(temp.max.d, temp.max.g, use = "pairwise.complete.obs")) %>%
    filter(!is.na(precip)) %>%
    mutate(across(precip:temp.max, \(x) round(x, 2))) %>%
    data.frame()
  
  # Correlation by year and site
  cor_siteyr <- alldata %>%
    group_by(code, year) %>%
    summarize(precip = cor(precip.d, precip.g, use = "pairwise.complete.obs"),
              temp.min = cor(temp.min.d, temp.min.g, use = "pairwise.complete.obs"),
              temp.max = cor(temp.max.d, temp.max.g, use = "pairwise.complete.obs"),
              .groups = "keep") %>%
    filter(!is.na(precip)) %>%
    mutate(across(precip:temp.max, \(x) round(x, 2))) %>%
    data.frame()
  
  summary(cor_site)
  summary(cor_yr)
  summary(cor_siteyr)
  
  # Temperatures always highly correlated, but not precipitation!
  cor_siteyr %>%
    filter(precip < 0.5)
  # 30 site-yrs where correlations range from 0.04 - 0.49

# Format combined dataset and write to file
alldata <- alldata %>%
  arrange(code, year, month, day) %>%
  rename(site = code)
# write.csv(alldata, "enviro-data.csv", row.names = FALSE)
  
#------------------------------------------------------------------------------#
# Add in NDVI data after the fact
#------------------------------------------------------------------------------#

dat <- read.csv("enviro-data.csv")
ndvi <- read.csv("NDVI-data.csv")

ndvi <- ndvi %>%
  mutate(year = as.numeric(str_sub(calendar_date, 1, 4)),
         month = as.numeric(str_sub(calendar_date, 6, 7)),
         day = as.numeric(str_sub(calendar_date, 9, 10))) %>%
  dplyr::select(-calendar_date) %>%
  rename(site = site_name)

dat <- left_join(dat, ndvi, by = c("site", "year", "month", "day"))

dat <- dat %>%
  arrange(site, year, month, day)
write.csv(dat, "enviro-data-with-ndvi.csv", row.names = FALSE)
