# Create covariates for survival analysis
# NDVI
# Temperature
# Precipitation

# Gaby Samaniego
# gabysamaniego@arizona.edu
# 2024-10-07

# The script first cleans and summarizes daymet and MODIS data to create the 
# covariates 

# Load packages
library(tidyverse)

# Clear environment
rm(list = ls())

# Winter months are December, January, and February
# Summer months are May, June, July, and August

# ------------------------------- Define Winter ------------------------------ #

# Load winter sites 
winter.sites <- read.csv('output/GBIF-Mexico-data/filtered-GBIF-BTLH-winter-sites-with-names.csv',
                         strip.white = TRUE)

# Edit and filter data set to include just winter months
winter <- winter.sites %>% 
  filter(month %in% c(12, 1, 2))

# PLot winter points

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
locs <- vect(winter, 
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

# I think we should remove the point/s in Guatemala. There is a resident population
# that doesn't migrate. It is considered a subspecies.  

# The winter are we are looking at is quite big. 

# ----------------------------------- NDVI ----------------------------------- #

# Load data files and merge them. 
# These files have NDVI values for winter and summer sites
NDVI1 <- read.csv('output/weather-data/ndvi-data-10-sites.csv')
NDVI2 <- read.csv('output/weather-data/ndvi-data-50.1-sites.csv')
NDVI3 <- read.csv('output/weather-data/ndvi-data-50.2-sites.csv')

# Merge and edit data sets
ndvi <- full_join(NDVI1, NDVI2) %>% 
  full_join(NDVI3) %>% 
  select(-X) %>%
  mutate(calendar_date = as.Date(calendar_date),
         year = year(calendar_date),
         month = month(calendar_date))

# Prepare winter data set before summarizing data
ndvi.dat.winter <- ndvi %>% 
  filter(month %in% c(12, 1, 2),
         site_name %in% winter$code) %>% 
  mutate(year_plus = ifelse(month == 12, year+1, year),
         year_less = year_plus-1) %>% 
  unite(winter_period, c(year_less, year_plus), sep = '-', remove = T) %>% 
  filter(!winter_period %in% c('2000-2001', '2012-2013'))

# Plot raw data
boxplot(ndvi ~ winter_period,
        data = ndvi.dat.winter, 
        horizontal = FALSE, 
        col = "steelblue")

# Is the graphic telling us there is a lot of variability in the data, or are
# there a lot of outliers? Since we are using points in a big area, should we 
# expect to see this?

boxplot(ndvi ~ site_name,
        data = ndvi.dat.winter, 
        horizontal = FALSE, 
        col = "steelblue")

# Exploring the graphic of all sites individually, there is a big variability 
# as well and a lot of outliers. Should we transform the data? I reviwed the 
# Graham paper and this is what it said:

# 'Given that these observations were not evenly spread across a given wintering 
# season, simply taking an average might bias our estimate of NDVI. Therefore, 
# we interpolated the daily NDVI values for each day and 1-km cell using univariate 
# Akima interpolation (Akima 1991). Resource availability was then estimated as 
# the average observed daily NDVI value across all grid cells with broad-tailed 
# hummingbird observations during the winter season.

# Summarize data for winter
ndvi.winter <- ndvi.dat.winter %>% 
  group_by(winter_period) %>% 
  summarize(aver_ndvi = mean(ndvi),
            min_ndvi = min(ndvi),
            max_ndvi = max(ndvi)) %>% 
  filter(!winter_period == '2001-2002')

# Filter NDVI data for summer months and sites and calculate mean value per year
ndvi.summer <- filter(ndvi, str_detect(site_name, 'ss')) %>% 
  filter(month %in% c(5, 6, 7, 8),
         !year %in% c(2001, 2002)) %>% 
  group_by(year) %>% 
  summarize(aver_ndvi = mean(ndvi),
            min_ndvi = min(ndvi),
            max_ndvi = max(ndvi))

# -------------------------------- Temperature ------------------------------- #

# Load data 
daymet <- read.csv('output/weather-data/cleaned-daymet-data-all-sites.csv') %>% 
  select(-X)

# dayl = day length s/day
# prcp = precipitation mm/day
# srad = shortwave radiation W/m2
# swe = snow water equivalent kg/m2
# tmax = maximum air temperature C
# tmin = minimum air temperature C
# vp = water vapor pressure Pa 

# Prepare winter data set and calculate daily mean temperature
temp.dat.winter <- daymet %>%
  select(-c(dayl,prcp,srad,swe,vp)) %>% 
  filter(month %in% c(12, 1, 2),
         site %in% winter$code) %>% 
  mutate(year_plus = ifelse(month == 12, year+1, year),
         year_less = year_plus-1) %>% 
  unite(winter_period, c(year_less, year_plus), sep = '-', remove = T) %>% 
  filter(!winter_period %in% c('2001-2002', '2012-2013')) %>% 
  mutate(tmean = (tmax + tmin) / 2)  #Daily mean temperature)

# Plot raw data
boxplot(tmean ~ winter_period,
        data = temp.dat.winter, 
        horizontal = FALSE, 
        col = "steelblue")

boxplot(tmean ~ site,
        data = temp.dat.winter, 
        horizontal = FALSE, 
        col = "steelblue")

# plot one site
ws99 <- temp.dat.winter %>% 
  filter(site == 'ws99')

boxplot(tmean ~ site,
        data = ws99, 
        horizontal = FALSE, 
        col = "steelblue")

# Similar that with NDVI, Is this a lot of variation? Not sure if I'm looking at 
# this data correctly

# Summarize data
temp.winter <- temp.dat.winter %>% 
  group_by(winter_period) %>% 
  summarize(aver_max_temp = mean(tmax),
            aver_min_temp = mean(tmin),
            aver_mean_temp = mean(tmean),
            max_temp = max(tmax),
            min_temp = min(tmin))

unique(temp.winter$site)

# ------------------------------- Precipitation ------------------------------ #

# Filter precipitation data for winter months and sites and summarize data
precip.dat.winter <- daymet %>%
  select(-c(dayl,srad,swe,vp,tmax,tmin)) %>% 
  filter(month %in% c(12, 1, 2),
         site %in% winter$code) %>% 
  mutate(year_plus = ifelse(month == 12, year+1, year),
         year_less = year_plus-1) %>% 
  unite(winter_period, c(year_less, year_plus), sep = '-', remove = T) %>% 
  filter(!winter_period %in% c('2001-2002', '2012-2013'))

# Summarize data
precip.winter <- precip.dat.winter %>% 
  group_by(winter_period) %>% 
  summarize(aver_precip = mean(prcp),
            accum_precip = sum(prcp))

