# Exploring BTLH movements between banding sites at RMNP 
# Original Code by Erin Zylstra
# Edited by Gaby Samaniego
# gabysamaniego@arizona.edu
# 2024-09-27

# Load packages
library(tidyverse)
library(terra)

# Clear environment
rm(list = ls())

# Load data
raw.dat <- read.csv('output/capture-data/cleanded-capture-data-RMNP-full.csv')
sites.df <- read.csv('data/sites-BTLH-range-map-dem/RMNP-sites-data.csv')

# Add two new sites identified in script 'merge-cleaned-banded-recaptured-data-RMNP' 
# where birds were recaptured 
sites.to.add <- data.frame(site = c("YMCA", "HPK3"),
                           latitude = NA,
                           longitude = NA,
                           elevation = NA,
                           location = "east")
sites.df <- rbind(sites.df, sites.to.add)

# For each bird, calculate the number of unique sites captured at (obssite) as
# well as the number of movements (n_moves)
bird.sites <- raw.dat %>%
  arrange(band, obsdate) %>%
  group_by(band, band_site, band_year) %>%
  summarize(n_sites = length(unique(obssite)),
            n_moves = length(rle(obssite)$lengths) - 1,
            site2 = ifelse(n_moves > 0, rle(obssite)$values[2], NA),
            site3 = ifelse(n_moves > 1, rle(obssite)$values[3], NA),
            site4 = ifelse(n_moves > 2, rle(obssite)$values[4], NA),
            site5 = ifelse(n_moves > 3, rle(obssite)$values[5], NA),
            .groups = 'keep') %>%
  data.frame()

# How many birds moved how many times?
count(bird.sites, n_moves) 
# 9772 didn't move
# 250 birds moved at least once
# 89 birds moved twice 
# 4 birds moved three times
# 11 birds moved four times

# How many different sites did birds move between?
count(bird.sites, n_sites) 
# 340 birds used 2 sites
# 14 used 3 sites

# Create data set of movements
movements <- raw.dat %>%
  filter(band %in% bird.sites$band[bird.sites$n_moves > 0]) %>%
  arrange(band, band_status, year, obsdate) %>%
  mutate(site_previous = NA,
         time = NA)

for (i in 2:nrow(movements)) {
  if (movements$band[i] == movements$band[i - 1]) {
    movements$site_previous[i] <- movements$obssite[i - 1]
    movements$time[i] <- movements$year[i] - movements$year[i - 1]
  } else {
    movements$site_previous[i] <- NA
    movements$time[i] <- NA
  }
}

# Filter data 
movements <- movements %>%
  filter(obssite != site_previous) %>%
  mutate(same_yr = if_else(time > 0, 0, 1)) %>%
  select(-c(year, band_status, time))

# Summarize data
nrow(movements)                # 484 movements
length(unique(movements$band)) # 354 birds
count(movements, same_yr)      # 243 movements within year, 241 across years

# Get distances between sites

# # Convert sites to a SpatVector
locs <- vect(sites.df, 
             geom = c('longitude', 'latitude'), 
             crs = 'EPSG:4326')

# Give each site a number
sites.df$site_num <- 1:nrow(sites.df)

# Calculate distance between all pairs of sites, in km
dists <- distance(locs, 
                  unit = 'km', 
                  pairs = TRUE, 
                  symmetrical = FALSE) %>% 
  as.data.frame()

# Add site names to distance data frame for convenience
dists$from_name <- sites.df$site[match(dists$from, sites.df$site_num)]
dists$to_name <- sites.df$site[match(dists$to, sites.df$site_num)]

# Edit data set
dists <- dists %>%
  select(-c(from, to)) %>%
  rename(dist = value,
         from = from_name,
         to = to_name)

# Merge data sets to reflect movement's starting site (site_previous) and ending 
# site (obssite)
movements <- left_join(movements, dists, 
                       join_by('site_previous' == 'from', 
                               'obssite' == 'to')) 

# Add sites location (east or west) to identify if birds flew over the divide
movements$site_ew <- sites.df$location[match(movements$obssite, sites.df$site)]
movements$site_previous_ew <- sites.df$location[match(movements$site_previous, 
                                                      sites.df$site)]

# Create indicator column to identify birds that flew over the divide
movements <- movements %>%
  mutate(over_ridge = if_else(site_ew != site_previous_ew, 1, 0)) %>%
  select(-c(site_ew, site_previous_ew))

# Filter for over the divide birds
filter(movements, over_ridge == 1)
filter(raw.dat, band == unique(movements$band[movements$over_ridge == 1])[1])
# Just a single bird was recaptured on both sides of the divide 9000-90568 AHY
# M originally banded on west site HOLZ in 2008, then captured on east site MCGC 
# in May 2009 and then back at HOLZ in July 2009

# Summary of all movements for which distances are known, so excludes YMCA 
# and HPK3, but only movements to/from these sites are to others nearby (MP1,
# HPK1, HPK2)
filter(movements, obssite %in% c('YMCA', 'HPK3') | 
         site_previous %in% c('YMCA', 'HPK3'))

# Filter NA values in distance out
movements.all <- movements %>% 
  filter(!is.na(dist))

# Count the number of movements between sites and sort by distance
count(movements.all, site_previous, obssite) %>%
  left_join(dists, by = c('site_previous' = 'from', 
                          'obssite' = 'to')) %>%
  arrange(dist)

# How many movements happened within a short distance
(n.dists.small <-  sum(movements.all$dist < 2.5))
n.dists.small / nrow(movements.all)
# 396 movements (84% of known movement distances) < 2.5 km

# How many movements happened within a larger distance
(n.dists.big <-  sum(movements.all$dist > 10))
n.dists.big / nrow(movements.all)
# 14 movements (3% of known movements distances) > 10 km

# Just looking at inter-year movements

# Create data set
movements.inter.year <- movements %>%
  filter(!is.na(dist)) %>%
  filter(same_yr == 0)

# How many movements happened within a short distance
(n.dists.iy.small <- sum(movements.inter.year$dist < 2.5)) 
n.dists.iy.small / nrow(movements.inter.year)
# 197 movements (85% of known movement distances) < 2.5 km

# How many movements happened within a larger distance
(n.dists.iy.big <- sum(movements.inter.year$dist > 10)) 
n.dists.iy.big / nrow(movements.inter.year)
# 8 movements (4% of known movement distances) > 10 km

# How many times did birds get recaptured in different years?
recap.events <- raw.dat %>%
  select(band, year, band_status) %>%
  distinct(band, year, .keep_all = TRUE)
n.recap.events <- sum(recap.events$band_status == 'R') 
# 2844 recapture events

# How many recapture events did not happen at site of previous capture? 
# (though many very close)
nrow(movements.inter.year) / n.recap.events 
# 8% of recapture events did not occur at site of previous capture 

# What were common movements (regardless of direction)
for (i in 1:nrow(movements)) {
  sites_row <- sort(c(movements$obssite[i], movements$site_previous[i]))
  movements$sitea[i] <- sites_row[1]
  movements$siteb[i] <- sites_row[2]
}

(common.moves <- count(movements, sitea, siteb, dist) %>%
  arrange(desc(n), sitea))

# Most common move is between CC3 and MCGC (same cluster), then
# HPK1 AND MP1 (same cluster)
# CC2 and MCGC (same cluster)
# HPK1 and HPK2 (same cluster)
# GNMTN and HOLZ
# HPK2 and MP1 (same cluster)
# all other pairs each occurred < 24 times

# Lack of movements between east and west side suggests that maybe it's worth 
# separating the two areas
