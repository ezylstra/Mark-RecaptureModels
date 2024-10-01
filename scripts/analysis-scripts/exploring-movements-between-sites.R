# Exploring birds' movements between banding sites at RMNP 
# Original Code by Erin Zylstra
# Edited by Gaby Samaniego
# gabysamaniego@arizona.edu
# 2024-09-27

# Load packages
library(tidyverse)

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
bird_sites <- raw.dat %>%
  arrange(band, obsdate) %>%
  group_by(band, band_site, band_year) %>%
  summarize(n_sites = length(unique(obssite)),
            n_moves = length(rle(obssite)$lengths) - 1,
            site2 = ifelse(n_moves > 0, rle(obssite)$values[2], NA),
            site3 = ifelse(n_moves > 1, rle(obssite)$values[3], NA),
            site4 = ifelse(n_moves > 2, rle(obssite)$values[4], NA),
            site5 = ifelse(n_moves > 3, rle(obssite)$values[5], NA),
            .groups = "keep") %>%
  data.frame()
count(bird_sites, n_moves) # 250 birds moved at least once
count(bird_sites, n_sites) # 340 birds used 2 sites, 14 used 3 sites

# Create data set of movements
movements <- raw.dat %>%
  filter(band %in% bird_sites$band[bird_sites$n_moves > 0]) %>%
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

movements <- movements %>%
  filter(obssite != site_previous) %>%
  mutate(same_yr = if_else(time > 0, 0, 1)) %>%
  select(-c(year, band_status, time))

nrow(movements)                # 484 movements
length(unique(movements$band)) # 354 birds
count(movements, same_yr)      # 243 movements within year, 241 across years

# Get distances between sites
locs <- vect(sites.df, geom = c("longitude", "latitude"), crs = "EPSG:4326")
sites.df$site_num <- 1:nrow(sites.df)
dists <- distance(locs, unit = "km", pairs = TRUE, symmetrical = FALSE) %>% 
  as.data.frame()
dists$from_name <- sites.df$site[match(dists$from, sites.df$site_num)]
dists$to_name <- sites.df$site[match(dists$to, sites.df$site_num)]
dists <- dists %>%
  select(-c(from, to)) %>%
  rename(dist = value,
         from = from_name,
         to = to_name)


#### left here.... 

movements <- left_join(movements, dists, 
                       join_by("site_previous" == "from", "obssite" == "to")) 
movements$site_ew <- sites.df$location[match(movements$site, sites.df$site)]
movements$site_previous_ew <- sites.df$location[match(movements$site_previous, sites.df$site)]
movements <- movements %>%
  mutate(over_ridge = if_else(site_ew != site_previous_ew, 1, 0)) %>%
  select(-c(site_ew, site_previous_ew))

filter(movements, over_ridge == 1)
filter(raw.dat, band == unique(movements$band[movements$over_ridge == 1])[1])
# Just a single bird was recaptured on both sides of the ridge (9000-90568)
# Banded on west site (HOLZ) in 2008, then captured on east site (MCGC) in 
# May 2009 and then back at HOLZ in July 2009.

# Summary of all movements (for which distances are known, so excludes YMCA 
# and HPK3, but only movements to/from these sites are to others nearby [MP1,
# HPK1, HPK2])
filter(movements, site %in% c("YMCA", "HPK3") | site_previous %in% c("YMCA", "HPK3"))

move_all <- movements %>% 
  filter(!is.na(dist))

count(move_all, site_previous, site) %>%
  left_join(dists, by = c("site_previous" = "from", 
                          "site" = "to")) %>%
  arrange(dist)

(n_dists_small <-  sum(move_all$dist < 2.5))
n_dists_small / nrow(move_all)
# 376 movements (84% of known movement distances) < 2.5 km
(n_dists_big <-  sum(move_all$dist > 10))
n_dists_big / nrow(move_all)
# 14 movements (3% of known movements distances) > 10 km

# Just looking at inter-year movements
move_iy <- movements %>%
  filter(!is.na(dist)) %>%
  filter(same_yr == 0)
(n_dists_iy_small <- sum(move_iy$dist < 2.5)) 
n_dists_iy_small / nrow(move_iy)
# 182 movements (85% of known movement distances) < 2.5 km
(n_dists_iy_big <- sum(move_iy$dist > 10)) 
n_dists_iy_big / nrow(move_iy)
# 8 movements (4% of known movement distances) > 10 km

# How many times did birds get recaptured in different years?
recap_events <- allcaps %>%
  select(band, year, band_status) %>%
  distinct(band, year, .keep_all = TRUE)
n_recap_events <- sum(recap_events$band_status == "R") # 2458 recapture events

nrow(move_iy) / n_recap_events 
# 8% of recapture events did not occur at site of previous capture (though many
# very close)

# Common movements (regardless of direction)
for (i in 1:nrow(movements)) {
  sites_row <- sort(c(movements$site[i], movements$site_previous[i]))
  movements$sitea[i] <- sites_row[1]
  movements$siteb[i] <- sites_row[2]
}
common_moves <- count(movements, sitea, siteb, dist) %>%
  arrange(desc(n), sitea)
common_moves
# Most common move is between CC3 and MCGC (same cluster), then
# HPK1 AND MP1 (same cluster)
# CC2 and MCGC (same cluster)
# HPK1 and HPK2 (same cluster)
# GNMTN and HOLZ
# HPK2 and MP1 (same cluster)
# all other pairs each occurred < 24 times

# Lack of movements between east and west side suggests that maybe it's worth 
# separating the two areas.
