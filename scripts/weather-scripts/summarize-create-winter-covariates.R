# Create winter covariates for survival analysis
# Gaby Samaniego
# gabysamaniego@arizona.edu
# 2024-10-18

# Load packages
library(tidyverse)
library(terra)
library(tidyterra)
library(ggplot2)
library(sf)
library(dbscan) # To work with clusters in map

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

# Plot BTLH winter grounds
# Non breeding range of BTLH from eBird
range <- vect("data/sites-BTLH-range-map-dem/brthum_range_2021.gpkg")
data.frame(range)
nonbreeding <- subset(range, range$season == 'nonbreeding')

# Quick look at non breeding range
plot(nonbreeding, col = "lightblue")

# Convert filtered winter sites to a SpatVector
locs <- vect(winter, 
             geom = c('longitude', 'latitude'), 
             crs = crs(nonbreeding))

# Plot all winter points in Mexico
ggplot() + 
  geom_spatvector(data = nonbreeding, fill = "lightblue", color = NA) +
  borders('world', regions = 'Mexico', colour = 'black') +
  geom_spatvector(data = locs) +
  coord_sf(expand = FALSE, xlim = c(-107, -90),  ylim = c(25, 14)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 9)) +
  labs(title = 'Broad-tailed hummingbird wintering distribution range',
       subtitle = 'and 138 GBIF winter observations') +
  theme(plot.title = element_text(color = "black", size = 12, hjust = 0.5),
        plot.subtitle = element_text(color = "black", size = 12, hjust = 0.5)) 

# Before averaging weather variables we need to thin the number of points because 
# we don't want to average over climate values at all sites when some of those 
# sites are in really close proximity to each other and depending on the spatial 
# resolution of the climate data, probably have identical NDVI or temperature 
# values (from Erin's email)

# To identify the sites that are too close together I tried some options:

# 1) Calculating the distance between points

# Calculate pairwise distances between points 

# Add a site number to help organize things later
winter$site_num <- 1:nrow(winter)

# Calculate distance between all pairs of sites, in m
dists <- distance(locs, 
                  unit = 'm', 
                  pairs = TRUE, 
                  symmetrical = FALSE)
dists <- as.data.frame(dists) %>% 
  arrange(value)

# Note: by setting symmetrical = TRUE it includes the distance between a 
# pair only once in the data frame (ie, distance between A and B is included, but 
# not distance between B and A) 

# Add site names to distance data frame for convenience
dists$from_name <- winter$code[match(dists$from, winter$site_num)]
dists$to_name <- winter$code[match(dists$to, winter$site_num)]

head(dists)

# Some points are in very close proximity to each other and they might have the 
# same or very similar coordinates. 
# Check: ws206 and ws223, ws28 and ws148, ws8 and ws115
filter(winter, code %in% c('ws206', 'ws223')) 
filter(winter, code %in% c('ws28', 'ws148')) 
filter(winter, code %in% c('ws81', 'ws222'))

# Why the distinct() function didn't catch these duplicated coordinates in the 
# script I used to download the data from GBIF? Because they are indeed different.
# I didn't notice the difference in the coordinates, because my RStudio was not 
# set up to show all decimals in the output or in the data frames, so I was seeing 
# just the first three decimals of the coordinates, therefore they apparently 
# were duplicates when they were not. I set up the decimals in R 
# studio to 15 digits by running: options(digits = 15)

# Now I can see all the decimals in the coordinates and know why distinct() kept 
# all these 'duplicated' sites.

# After reading a bit about rounding decimals in coordinates, I decided to round 
# theme to 3 decimals to remove the duplicates
winter.new <- winter %>% 
  mutate(latitude = round(latitude, 3),
         longitude = round(longitude, 3)) %>%
  distinct(latitude, longitude, .keep_all = TRUE) %>% 
  select(-site_num)
# The code removes five sites 

# Add new site number to help organize things later
winter.new$site_num <- 1:nrow(winter.new)

# Fin the distance between points again

# Convert filtered winter sites to a SpatVector
locs.new <- vect(winter.new, 
             geom = c('longitude', 'latitude'), 
             crs = crs(nonbreeding))

# Calculate distance between new winter sites
dists.new <- distance(locs.new,
                      unit = 'm', 
                      pairs = TRUE, 
                      symmetrical = TRUE)
dists.new <- as.data.frame(dists.new) %>%
  arrange(value) %>% 
  mutate(value = round(value, 2))
  
# Add site names to distance data frame for convenience
dists.new$from_name <- winter.new$code[match(dists.new$from, winter.new$site_num)]
dists.new$to_name <- winter.new$code[match(dists.new$to, winter.new$site_num)]

# Next step with this approach:
# Filter the sites based on the 'value' column in data frame 'dists.new' to less 
# or equal to xxx. I need to define the xxx distance value. 

# MODIS spatial resolution is 250x250 m
# Daymet spatial resolution is 1000x1000 m 

# Is it OK to filter the points based in the distance between them? If I use 250m 
# between points, then some of them will still be too close together for the 
# daymet data. For now, I'm going to use 1000m to thin the number of points.

# Use 1000 m to filter the points that are close together
close.sites <- dists.new %>% 
  filter(value <= 1000) %>% 
  arrange(from_name) 

far.sites <- dists.new %>% 
  filter(value >= 1000) %>% 
  arrange(from_name)
  
# Plot close sites

# Add geographic information to close sites 
close.sites.full <- close.sites %>% 
  select(from_name) %>% 
  distinct() %>% 
  rename(code = from_name) %>% 
  left_join(winter.new, by = 'code')

# Convert close sites to a SpatVector
locs.close <- vect(close.sites.full, 
                   geom = c('longitude', 'latitude'), 
                   crs = crs(nonbreeding))

# Plot sites that are close together <= to 1000 m of distance
ggplot() + 
  geom_spatvector(data = nonbreeding, fill = "lightblue", color = NA) +
  borders('world', regions = 'Mexico', colour = 'black') +
  geom_spatvector(data = locs.close) +
  coord_sf(expand = FALSE, xlim = c(-104, -96),  ylim = c(20, 17)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 9)) +
  labs(title = 'Broad-tailed hummingbird wintering distribution range',
       subtitle = 'and 34 close GBIF winter observations') +
  theme(plot.title = element_text(color = "black", size = 12, hjust = 0.5),
        plot.subtitle = element_text(color = "black", size = 12, hjust = 0.5)) 

# To thin the points that are too close together, I thought I could delete either 
# the column 'from_name' or 'to_name' in the close.sites data frame and keep just 
# one of the paired points that are less than 1000 m apart. But this approach won't 
# work as I realize have clusters of points that are close together, not just 
# pairs of points.So I tried the next approach.

# 2) Thinning points based on clusters of points. I used ChatGPT to build this 
# code

# We need to use the points as an sf object, transform SpatVectors to sf objects 
sf.points <- sf::st_as_sf(locs.new)

# Transform coordinates from degrees to UTM
locs.UTM <- st_transform(sf.points,
                         crs = 32614) # UTM zone 14N for central Mexico

# Extract coordinates from the sf object and transform the points into a numeric 
# matrix to use dbscan package to find clusters based on a 1000 m distance 
# threshold
coords <- st_coordinates(locs.UTM)
# There are 133 coordinates representing the 133 points in winter.new. Good

# Perform clustering
# dbscan: Density-based Spatial Clustering of Applications with Noise
# epsilon neighborhood: set of points that lie within a specified distance
clusters <- dbscan(coords, # A data matrix 
                   eps = 1000, # Radius of the epsilon neighborhood
                   minPts = 1) # Number of minimum points required in the eps neighborhood

# Group the points by their cluster ID and keep one point from each cluster

# Add cluster IDs to the sf object
locs.UTM$cluster <- clusters$cluster

# Retain one point per cluster (the first one encountered)
unique.points.sf <- locs.UTM[!duplicated(locs.UTM$cluster), ]

# Output summary
print(paste("Original number of points:", nrow(locs.UTM)))
print(paste("Number of unique points after clustering:", nrow(unique.points.sf)))
# This code reduces the winter sites to 112

# Plot new points
ggplot() + 
  geom_spatvector(data = nonbreeding, fill = "lightblue", color = NA) +
  borders('world', regions = 'Mexico', colour = 'black') +
  geom_spatvector(data = locs.UTM) +
  coord_sf(expand = FALSE, xlim = c(-107, -90),  ylim = c(25, 14)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 9)) +
  labs(title = 'Broad-tailed hummingbird wintering distribution range',
       subtitle = 'and 112 GBIF winter observations after point thinning using clusters') +
  theme(plot.title = element_text(color = "black", size = 12, hjust = 0.5),
        plot.subtitle = element_text(color = "black", size = 12, hjust = 0.5)) 

# Create list of final points to use to summarize the weather data
points.final <- unique.points.sf %>% 
  as.data.frame() %>% 
  select(code)
length(unique(points.final$code))  

# I'm still not sure this is the best approach to thin the points, because 
# considering that the diagonal in 1 km2 is a bit over 1400 meters, potentially 
# I could still have two points in the same daymet grid cell even if they are 
# 1000 meters apart (cluster threshold). So, I could increase the threshold 
# > 1400 meters, or try the next approach.

# 3) Using a grid to thin the points

# Using the spatial resolution of the daymet data, I thought to add a 
# grid over the map and ask R to thin the points based on where on the map the 
# points fall. Again, I used ChatGPT to build this code after I couldn't use the
# code I gathered by using Google searchs (lines xxx to xxx)

# First, I need to use an sf object
sf.points.for.grid <- sf::st_as_sf(locs.UTM)

# Create a Daymet grid

# Create a bbox object to define the extent of the points
# bbox = bounding box 
bbox <- st_bbox(sf.points.for.grid) 

# Create a 1 km x 1 km grid over the extent of the points
grid <- st_make_grid(
  st_as_sfc(bbox, crs = st_crs(sf.points.for.grid)), # Use the bounding box of the points
  cellsize = 1000, # Daymet grid resolution in meters
  square = TRUE) # If FALSE, creates an hexagonal grid

# Convert the grid to an sf object
sf.grid <- st_sf(geometry = grid)

# Adjust the points to the grid
# Find the grid cell each point falls into
sf.points.for.grid$grid_id <- st_within(sf.points.for.grid, sf.grid) %>% as.integer()
sf.points.for.grid <- sf.points.for.grid %>% 
  arrange(cluster)

head(sf.points.for.grid, 10)
# It is interesting how some points that where in the same cluster now are not
# in the same cell grid (ws1 and ws201) and how points that were in the same cluster
# are also in the same grid (ws5 and 145). Still wondering which way is the best
# way to thinned the points. I would like to hear your thoughts :) 

# If points fall outside the grid, they won't have a match. These will have NA 
# as their grid_id. Check if there are any NA values
which(is.na(sf.points.for.grid$grid_id))

# Points might fall outside the grid if:
# 1) There is a bounding box misalignment, because the grid is created with a 
# fixed cell size (1km x 1km). If the bounding box dimensions are not divisible 
# evenly by the cell size, the grid might not fully cover the bounding box's 
# exact boundaries. Bbox is created based on the points.
# 2) Minor floating-point errors can sometimes cause coordinates to fall just 
# outside the intended grid due to precision issues.
# 3) Small discrepancies can arise from the transformation process (coordinates, 
# crs).
# 4) Points that lie exactly on the edges of the bounding box might not fall into 
# any grid cell.

# There are some ways to add this points to the grid, but before doing that I'm
# going to explore the points with NA values

# First, make sure the crs in the grid and the points is the same
st_crs(sf.points.for.grid) == st_crs(sf.grid)
# They are the same

# Filter NA values
na.values.grid <- sf.points.for.grid %>% 
  filter(is.na(grid_id))

# Plot points with NA values
sf.na.values <- sf::st_as_sf(na.values.grid)
ggplot() + 
  geom_sf(data = sf.points.for.grid, color = 'blue', size = 0.5) +
  geom_sf(data = na.values.grid, color = 'red', size = 0.1) +
  geom_sf_text(data = na.values.grid, 
               aes(label = code), 
               nudge_y = 40000, 
               nudge_x = 40000,
               size = 3)

# Before deciding if these points need to be added to the grid, I'm running the 
# next step in the thinning process to keep only one point per grid cell
thinned.points <- sf.points.for.grid %>%
  group_by(grid_id) %>%
  slice(1) %>%
  ungroup()

# Check if the two points with NA values in grid_id are in the thinned.points
# data set. After looking at the map, my guess is that the point ws36 won't be
# in the data set because it is really close to other points, so if this point
# is not included in the final 'thinned.points' it is fine. On the other hand, 
# ws20 will be in the data set because there are no other points near it
c('ws36', 'ws20') %in% thinned.points$code

# This is true. It is fine if ws36 is not in the new data set, because this point 
# is already being represented by another near by point. Now, ws20 it is included 
# but I think we should leave it outside the analysis because it is near Guatemala
# where there is a resident population of BTLH. We can discuss more about this.
# For now I'm going to leave ws20 out.
thinned.points <- thinned.points %>% 
  filter(code != 'ws20')

# Plot thinned points
thinned.points <- vect(thinned.points)
ggplot() + 
  geom_spatvector(data = nonbreeding, fill = "lightblue", color = NA) +
  borders('world', regions = 'Mexico', colour = 'black') +
  geom_spatvector(data = thinned.points) +
  coord_sf(expand = FALSE, xlim = c(-107, -90),  ylim = c(25, 14)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 9)) +
  labs(title = 'Broad-tailed hummingbird wintering distribution range',
       subtitle = 'and 119 GBIF winter observations after point thinning using a grid') +
  theme(plot.title = element_text(color = "black", size = 12, hjust = 0.5),
        plot.subtitle = element_text(color = "black", size = 12, hjust = 0.5)) 


################################################################################
# I tried this code before using chatGPT but I don't know if it works because it 
# never finished running. I think using the entire winter range and a small grid 
# was too much for my computer.

# Transform SpatVectors to sf objects first
sf.points <- sf::st_as_sf(locs.new)
sf.winter.range <- sf::st_as_sf(nonbreeding)

# Transform coordinates from degrees to UTM
locs.UTM <- st_transform(sf.points,
                         crs = 32614) # UTM zone 14N for central Mexico

range.UTM <- st_transform(sf.winter.range,
                          crs = 32614) # UTM zone 14N for central Mexico

# Establish a 1000 x 1000 m grid over winter range
grid.spacing <- 1000  # size of squares, in units of the CRS 

winter.grid <- st_make_grid(range.UTM, # This is too big, it is not working!
                            square = T, 
                            cellsize = c(grid.spacing, grid.spacing)) %>% # the grid, covering bounding box
  st_sf() # not really required, but makes the grid nicer to work with later

plot(winter.grid, col = 'white')
plot(st_geometry(winter.meters), add = T)
# Just left this code here as a reference to my first attempt at using a grid to 
# thin the points. It is basically the same as the code ChatGPT suggested, which 
# made me feel less like a 'cheater' :)
################################################################################

# --------------------------- Summarize daymet data -------------------------- #

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

# Prepare data set to analyze data for winter months and winter periods. 

# For now I'm going to use the 119 points after thinning the number of winter
# points with the grid system.
daymet.dat <- daymet %>%
  filter(month %in% c(12, 1, 2),
         site %in% thinned.points$code) %>% 
  mutate(year_plus = ifelse(month == 12, year+1, year),
         year_less = year_plus-1) %>% 
  unite(winter_period, c(year_less, year_plus), sep = '-', remove = T) %>% 
  filter(!winter_period %in% c('2001-2002', '2012-2013'))

# -------------------------------- Temperature ------------------------------- #

# Calculate a seasonal value for each site before doing any averaging (from Erin) 

# 1) Overall Minimum temperature

# Extract minimum temperature over each site and winter period
min.temp <- daymet.dat %>% 
  select(site, tmin, winter_period) %>% 
  group_by(site, winter_period) %>% 
  summarise(min_temp = min(tmin))

# Explore minimum temperature data per sites 
min.temp.summary <- min.temp %>% 
  group_by(site) %>% 
  summarize(min = min(min_temp),
            mean = mean(min_temp),
            max = max(min_temp))
summary(min.temp.summary)

# Plot 10 random sites
set.seed(24) # for reproducibility

# Subset sites
sample.sites.min.temp <- sample(unique(min.temp$site), 10)

# Create data frame with information for subset sites
subset.points.min.temp <- min.temp[min.temp$site %in% sample.sites.min.temp, ]

# Plot them
ggplot(subset.points.min.temp, aes(x = winter_period, 
                                   y = min_temp, 
                                   group = site, 
                                   color = site)) +
  geom_line(alpha = 0.7, size = 1) +
  labs(title = "Variation of Minimum Temperature Over Time (Sampled Sites)",
       x = "Winter Period",
       y = "Minimum Temperature (°C)",
       color = "Site") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Explore the data per winter period and site
pivot.min.temp <- pivot_wider(min.temp,
                             names_from = site,
                             values_from = min_temp)

# Average across sites to get an annual value
min.temp.aver <- min.temp %>% 
  group_by(winter_period) %>% 
  summarise(ave_min_temp = mean(min_temp)) %>%  
  mutate(ave_min_temp = round(ave_min_temp, 2))

# Exploring monthly min temperature

# Extract monthly minimum temperature over each site and winter period
monthly.min.temp <- daymet.dat %>% 
  select(site, tmin, winter_period, month) %>% 
  group_by(site, winter_period, month) %>% 
  summarise(min_temp = min(tmin))

# Explore monthly minimum temperature data per sites 
monthly.min.temp.summary <- monthly.min.temp %>% 
  group_by(site) %>% 
  summarize(min = min(min_temp),
            mean = mean(min_temp),
            max = max(min_temp))
summary(monthly.min.temp.summary)

# Subset sites
sample.sites.monthly.min.temp <- sample(unique(monthly.min.temp$site), 6)

# Create data frame with information for subset sites
subset.points.monthly.min.temp <- monthly.min.temp[monthly.min.temp$site %in% 
                                                     sample.sites.monthly.min.temp, ] %>% 
  mutate(month = case_when(month == 1 ~ 'January',
                             month == 2 ~ 'February',
                             month == 12 ~ 'December'))

# Plot them
ggplot(subset.points.monthly.min.temp, 
       aes(x = month, 
           y = min_temp, 
           group = winter_period, 
           color = as.factor(winter_period))) +
  geom_line(size = 1, alpha = 0.7) +
  geom_point(size = 2) +
  facet_wrap(~ site, 
             scales = 'free_y') +
  labs(title = 'Monthly Minimum Temperature for Winter Months (Faceted by Site)',
    x = 'Month',
    y = 'Minimum Temperature (°C)',
    color = 'Winter Period') + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
  
# Average across sites to get an annual value
monthly.min.temp.aver <- monthly.min.temp %>% 
  group_by(winter_period) %>% 
  summarise(ave_monthly_min_temp = mean(min_temp)) %>%  
  mutate(ave_monthly_min_temp = round(ave_monthly_min_temp, 2))

# 2) Daily minimum temperature

# Average daily minimum temperature over each site and winter period
aver.daily.min.temp <- daymet.dat %>% 
  select(site, tmin, winter_period) %>% 
  group_by(site, winter_period) %>% 
  summarise(aver_min_temp = mean(tmin)) %>% 
  mutate(aver_min_temp = round(aver_min_temp, 2))

# Explore average daily minimum temperature data per sites
aver.daily.min.temp.summary <- aver.daily.min.temp %>% 
  group_by(site) %>% 
  summarize(min = min(aver_min_temp),
            mean = mean(aver_min_temp),
            max = max(aver_min_temp))
summary(aver.daily.min.temp.summary)

# Plot 10 random sites

# Subset sites
sample.sites.aver.min.temp <- sample(unique(aver.daily.min.temp$site), 10)

# Create data frame with information for subset sites
subset.points.aver.min.temp <- aver.daily.min.temp[aver.daily.min.temp$site %in% 
                                                     sample.sites.aver.min.temp, ]

# Plot them
ggplot(subset.points.aver.min.temp, aes(x = winter_period,
                                        y = aver_min_temp, 
                                        group = site, 
                                        color = site)) +
  geom_line(alpha = 0.7, size = 1) +
  labs(title = 'Variation of Average Minimum Temperature Over Time (Sampled Sites)',
       x = 'Winter Period',
       y = 'Average Minimum Temperature (°C)',
       color = 'Site') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Explore the data per winter period and site
pivot.aver.min.temp <- pivot_wider(aver.daily.min.temp,
                                   names_from = site,
                                   values_from = aver_min_temp)

# Average across sites to get an annual value
aver.min.temp.aver <- aver.daily.min.temp %>% 
  group_by(winter_period) %>% 
  summarise(aver_daily_min_temp = mean(aver_min_temp)) %>%  
  mutate(aver_daily_min_temp = round(aver_daily_min_temp, 2))

# Create data set with min temp, monthly aver min temp, and aver min temp per 
# winter period to compare
min.temp.full <- min.temp.aver %>% 
  left_join(aver.min.temp.aver, by = 'winter_period') %>% 
  left_join(monthly.min.temp.aver, by = 'winter_period')

# 3) Defining cold days

# Some papers I read about winter conditions affecting survival of other
# bird species used a measure of 'extreme weather event (ewe).' In some cases, these
# events were the number of days certain variable was too low (e.g temperature). 
# To define the threshold for the ewe, researchers used the 10th percentile of 
# the variable they were measuring, which can highlight rare or extreme conditions 
# experienced by the birds. 

# We discussed using the number of days the temperature was too cold in our
# winter grounds, so I'm going to count the number of days the temperature was equal 
# or under a 'threshold.' I need to read more about this threshold in BTLH or 
# other hummingbird species.For now I'm going to use the 10th percentile of the min
# temperature as the threshold and see what I get.

# The question here is, what data should I use to calculate 10th percentile?
# 

# Define cold threshold  
(threshold.1 <- quantile(min.temp$min_temp,
                                probs = 0.1, 
                                na.rm = TRUE))
# -7.608

(threshold.2 <- quantile(monthly.min.temp$min_temp,
                         probs = 0.1, 
                         na.rm = TRUE))
# -5.38

(threshold.3 <- quantile(aver.daily.min.temp$aver_min_temp,
                         probs = 0.1, 
                         na.rm = TRUE))

# 0.155


# Count cold days per site and date first
cold.days.per.site <- min.temp %>%
  select(site, date, tmin, winter_period) %>% 
  mutate(is_cold = if_else(tmin <= cold.threshold, 1, 0)) %>%
  group_by(winter_period, date) %>% 
  summarize(num_cold_days = sum(is_cold))
  
  
# Calculate number of cold days
num.cold.days <- daymet.dat %>%
  group_by(winter_period) %>%
  summarize(num_cold_days = sum(tmin <= cold.threshold, na.rm = TRUE))






# --------------------------------- NDVI DATA -------------------------------- #

# Ignore this code, it is old

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
  filter(!winter_period %in% c('2000-2001', '2012-2013')) %>% 
  rename(code = site_name)

length(unique(ndvi.dat.winter$site_name))
# ndvi.dat.winter has 138 points 

# We need to use the 112 thinned points we identified previously 
ndvi.dat.final <- points.final %>% 
  dplyr::left_join(ndvi.dat.winter, by = 'code')

length(unique(ndvi.dat.final$code))
# There are 112 points now






