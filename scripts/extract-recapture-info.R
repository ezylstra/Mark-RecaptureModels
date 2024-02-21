# Seeing if/how we can extract location (and date?) of recaptures
# 20 Feb 2024

# Load packages
library(tidyverse)
# library(data.table)
# library(janitor)
library(terra)

rm(list = ls())

# Run clean-RMNP-recaptured-data.R through line 561, then the following
# (just needed to do this once)

  # recap <- recaptured.dat %>% 
  #   filter(species == 'BTLH') %>%   
  #   select(UBI_band, band_status, recapture_year, fixed_sex, original_site, 
  #          paste0("recapture_", 1:17), comments) %>%
  #   rename(band = UBI_band, 
  #          recap_yr = recapture_year,
  #          site_orig = original_site,
  #          sex = fixed_sex)
  # colnames(recap)[6:22] <- paste0("r", 1:17)
  # 
  # # Export csv of new data frame
  # write.csv(recap, 
  #           "output/cleaned-recaptured-data-RMNP-full.csv",
  #           row.names = FALSE) 

# Load cleaned, full recap file:
recap <- read.csv("output/cleaned-recaptured-data-RMNP-full.csv",
                  na.strings = c(NA, ""))
colnames(recap)[colnames(recap) %in% paste0("r", 1:17)] <- 
  paste0("r", str_pad(1:17, width = 2, pad = "0"))

# Load cleaned banded file:
banded <- read.csv("output/cleaned-banded-data-RMNP.csv") %>%
  select(-c(X, species)) %>%
  rename(sex = fixed_sex,
         band = UBI_band)

# Load information about sites
sites_df <- read.csv("data/RMNP-sites-data.csv")
sites <- sort(unique(sites_df$site))

# Load letter codes for reference
letters <- read.csv("data/BBL_letter_codes.csv")

# Before I do anything else, need to fix or remove problematic band numbers
# from recap dataset (see merge-cleaned-banded-recapture-data-RMNP.R)
band_remove <- read.csv("data/recap-band-remove.csv")
band_fix <- read.csv("data/recap-band-fix.csv")

recap <- recap %>%
  filter(!band %in% band_remove$band_to_remove) %>%
  left_join(band_fix, join_by("band" == "old")) %>%
  mutate(band = if_else(is.na(new), band, new)) %>%
  select(-new)
# Check
# recap$band[!recap$band %in% banded$band]

# Is there an entry in the first recapture column for every bird/yr?  
sum(is.na(recap$r01)) # No, 7 NAs
filter(recap, is.na(r01))
# There are 7 rows with no entry in recapture columns
# (foreign, rebands, found dead)
  # 2 dead, will remove
  # 4 with NEW BAND and former number in comments, but this recapture info is in
  # banded, so can remove here too.
  # One with no comments (9000-12585). Maybe didn't enter info, but ok?
recap <- filter(recap, !(is.na(r01) & !is.na(comments)))
# Going to enter fake date into r1 column for remaining row (9000-12585 in 2010) 
# so that the recap info doesn't disappear if I convert to long form
recap$r01[is.na(recap$r01)] <- "7/15"

# Move information about rebanding events from banded to recap ----------------#

band_first <- banded %>%
  filter(band_status == "1") %>%
  select(-band_status) %>%
  rename(band_yr = year,
         band_age = age, 
         band_sex = sex,
         band_site = site)

rebands <- banded %>%
  filter(band_status == "R") %>%
  rename(reband_yr = year,
         reband_sex = sex,
         reband_age = age,
         reband_site = site) %>%
  left_join(band_first, by = "band")

filter(rebands, reband_yr == band_yr) # 5 rebands in same year, always at same site
# Delete these from rebands dataset
rebands <- rebands %>%
  filter(reband_yr != band_yr)

rebands_to_recap <- rebands %>%
  select(-c(reband_age, band_yr, band_age, band_sex)) %>%
  rename(recap_yr = reband_yr,
         sex = reband_sex,  # checked that sex is always same (orig, reband)
         r01 = reband_site,
         site_orig = band_site) %>%
  relocate(site_orig, .before = r01) %>%
  mutate(r02 = NA, r03 = NA, r04 = NA, r05 = NA, r06 = NA, r07 = NA, r08 = NA, 
         r09 = NA, r10 = NA, r11 = NA, r12 = NA, r13 = NA, r14 = NA, r15 = NA, 
         r16 = NA, r17 = NA, comments = NA)

recap <- rbind(recap, rebands_to_recap)

# Convert to long form --------------------------------------------------------#

recapl <- recap %>%
  pivot_longer(cols = r01:r17, 
               names_to = "recap_num",
               values_to = "entry") %>%
  data.frame() %>%
  filter(!is.na(entry)) %>%
  distinct()

# Merge this info with that in banded dataset (just original banding events)
recapl <- left_join(recapl, band_first, by = "band")

# Check that band_site (banded) == site_orig (recapl)
filter(recapl, site_orig != band_site) 
# Not always the same, so replace site_orig with band_site
recapl <- recapl %>%
  select(-site_orig)

# Check that sex == band_sex
filter(recapl, sex != band_sex)
# Not always the same (and surprisingly, not only when first captured as HY)
# Will likely want to rely sex assigned at least capture, but can deal with this
# later

# Explore sites ---------------------------------------------------------------#

# Load DEM
dem <- rast("data/dem.tif")

plot(dem)
points(latitude ~ longitude, data = sites_df, pch = 19, cex = 0.7)

plot(dem, xlim = c(-105.9, -105.4), ylim = c(40.1, 40.55))
points(latitude ~ longitude, data = sites_df, pch = 19, cex = 0.7)
text(latitude ~ longitude, labels = site, data = sites_df, pos = 3)

# Adding East/West delineation to sites (based on DEM)
west <- "WPK1|CLP|SHIP|HOLZ|GNMTN|BGMD|KV1|POLC"
sites_df$ew <- ifelse(str_detect(sites_df$site, west), "west", "east")

# Sites -----------------------------------------------------------------------#

sites_search <- paste0(c(sites, tolower(sites)), collapse = "|")
recapl <- recapl %>%
  mutate(site = toupper(str_extract(entry, pattern = sites_search))) %>%
  mutate(at = if_else(str_detect(entry, "at"), 1, 0))

filter(recapl, at == 1 & is.na(site))
# Other locs? YMCA (n = 6); HPK3 (6); Peaceful Valley, CO (1); ARIZONA (1)
# According to Google Maps, YMCA is close to MP1/HPK2/HPK1 complex. 
# Assuming HPK3 near other HPKs. 
# Will remove Peaceful Valley and ARIZONA since they're outside park or unknown

# Adding a few sites (that may or may not have been regular sampling locations)
sites_search <- paste0(c(sites_search, "YMCA", "HPK3"), collapse = "|")
sites_to_add <- data.frame(site = c("YMCA", "HPK3"),
                           latitude = NA,
                           longitude = NA,
                           elevation = NA,
                           ew = "east")
sites_df <- rbind(sites_df, sites_to_add)

# Remove recaps at other sites, create recap_site column and indicator if
# recap is in banding year (first_yr)
recapl <- recapl %>%
  filter(!str_detect(entry, "Peaceful Valley|ARIZONA")) %>%
  mutate(site = toupper(str_extract(entry, pattern = sites_search))) %>%
  mutate(recap_site = if_else(is.na(site), band_site, site)) %>%
  mutate(first_yr = if_else(recap_yr == band_yr, 1, 0)) %>%
  select(-c(at, site))

# Create data set with band number, sites captured at
band_site <- band_first %>%
  select(band, band_yr, band_site) %>%
  rename(yr = band_yr,
         site = band_site) %>%
  mutate(band_status = "1") %>%
  mutate(recap_num = "r00")

recap_site <- recapl %>%
  select(band, recap_yr, recap_site, band_status, recap_num) %>%
  rename(yr = recap_yr,
         site = recap_site)

# Combine banding and recap info, removing duplicates (and R if in the first
# year at the same site)
cap_sites <- rbind(band_site, recap_site) %>%
  distinct() %>%
  arrange(band, band_status, yr, recap_num, site) %>%
  distinct(band, yr, site, .keep_all = TRUE)

# Movements between sites
  # For each bird, calculate the number of unique sites captured at (n_sites) as
  # well as the number of movements (n_moves)
  bird_sites <- cap_sites %>%
    group_by(band) %>%
    summarize(band_site = site[1],
              band_yr = yr[1],
              n_sites = length(unique(site)),
              n_moves = length(rle(site)$lengths) - 1,
              site2 = ifelse(n_moves > 0, rle(site)$values[2], NA),
              site3 = ifelse(n_moves > 1, rle(site)$values[3], NA),
              site4 = ifelse(n_moves > 2, rle(site)$values[4], NA),
              site5 = ifelse(n_moves > 3, rle(site)$values[5], NA),
              .groups = "keep") %>%
    data.frame()
  count(bird_sites, n_moves) # 355 birds moved at least once
  count(bird_sites, n_sites) # 341 birds used 2 sites, 14 used 3 sites

  # Create data set of movements
  movements <- cap_sites %>%
    filter(band %in% bird_sites$band[bird_sites$n_moves > 0]) %>%
    arrange(band, band_status, yr, recap_num) %>%
    mutate(site_previous = NA,
           time = NA)
  
  for (i in 2:nrow(movements)) {
    if (movements$band[i] == movements$band[i - 1]) {
      movements$site_previous[i] <- movements$site[i - 1]
      movements$time[i] <- movements$yr[i] - movements$yr[i - 1]
    } else {
      movements$site_previous[i] <- NA
      movements$time[i] <- NA
    }
  }
  
  movements <- movements %>%
    filter(site != site_previous) %>%
    mutate(same_yr = if_else(time > 0, 0, 1)) %>%
    select(-c(yr, band_status, time))
  
  nrow(movements)                # 447 movements
  length(unique(movements$band)) # 355 birds
  count(movements, same_yr)      # 205 movements within year, 242 across years
  
  # Get distances between sites
  locs <- vect(sites_df, geom = c("longitude", "latitude"), crs = "EPSG:4326")
  sites_df$site_num <- 1:nrow(sites_df)
  dists <- distance(locs, unit = "km", pairs = TRUE, symmetrical = FALSE)
  dists <- as.data.frame(dists)
  dists$from_name <- sites_df$site[match(dists$from, sites_df$site_num)]
  dists$to_name <- sites_df$site[match(dists$to, sites_df$site_num)]
  dists <- dists %>%
    select(-c(from, to)) %>%
    rename(dist = value,
           from = from_name,
           to = to_name)

  movements <- left_join(movements, dists, 
                         join_by("site_previous" == "from", "site" == "to")) 
  movements$site_ew <- sites_df$ew[match(movements$site, sites_df$site)]
  movements$site_previous_ew <- sites_df$ew[match(movements$site_previous, sites_df$site)]
  movements <- movements %>%
    mutate(over_ridge = if_else(site_ew != site_previous_ew, 1, 0)) %>%
    select(-c(site_ew, site_previous_ew))

  filter(movements, over_ridge == 1)
  filter(banded, band == movements$band[movements$over_ridge == 1])
  filter(recap, band == movements$band[movements$over_ridge == 1])
  # Just a single bird was recaptured on both sides of the ridge
  # Banded on west site (HOLZ) in 2008, then captured on east site (MCGC) in 
  # May 2009 and then back at HOLZ in July 2009.

  # Summary of all movements (for which distances are known, so excludes YMCA 
  # and HPK3, but only movements to/from these sites are to others nearby [MP1,
  # HPK1, HPK2])
  filter(movements, site %in% c("YMCA", "HPK3") | site_previous %in% c("YMCA", "HPK3"))
  
  move_all <- movements %>% 
    filter(!is.na(dist))
  (n_dists_small <-  sum(move_all$dist < 2.5))
  n_dists_small / nrow(move_all)
    # 224 movements (52% of known movements distances) < 2.5 km
  (n_dists_big <-  sum(move_all$dist > 10))
  n_dists_big / nrow(move_all)
    # 157 movements (36% of known movements distances) > 10 km
  
  # Just looking at inter-year movements
  move_iy <- movements %>%
    filter(!is.na(dist)) %>%
    filter(same_yr == 0)
  (n_dists_iy_small <- sum(move_iy$dist < 2.5)) 
  n_dists_iy_small / nrow(move_iy)
  # 139 movements (60% of known movement distances) < 2.5 km
  (n_dists_iy_big <- sum(move_iy$dist > 10)) 
  n_dists_iy_big / nrow(move_iy)
  # 67 movements (29% of known movement distances) > 10 km
  
  # How many times did birds get recaptured in different years?
  recap_events <- cap_sites %>%
    select(band, yr, band_status) %>%
    distinct(band, yr, .keep_all = TRUE)
  n_recap_events <- sum(recap_events$band_status == "R") # 2465 recapture events
  
  n_dists_iy_big / n_recap_events 
  # 3% of recapture events did not occur at site of previous capture

  # Common movements (regarless of direction)
  for (i in 1:nrow(movements)) {
    sites_row <- sort(c(movements$site[i], movements$site_previous[i]))
    movements$sitea[i] <- sites_row[1]
    movements$siteb[i] <- sites_row[2]
  }
  common_moves <- count(movements, sitea, siteb, dist) %>%
    arrange(desc(n), sitea)
  common_moves
  # Most common move is between CC3 and MCGC, then
  # HPK1 AND MP1 (same cluster)
  # CC2 and MCGC (same cluster)
  # HPK1 and HPK2 (same cluster)
  # HPK2 and MP1 (same cluster)
  # GNMTN and HOLZ
  # all other pairs occurred < 22 times

  # Questioning whether the location for CC3 is correct given 
  # 1) its far from CC2 (almost 18 km), and 
  # 2) how many movements between CC3 and MCGC given 18.3 km distance.

# Lack of movements between east and west side suggests that maybe it's worth 
# separating the two areas.

# Dates -----------------------------------------------------------------------#

# Looks like many dates have m/dd format or dd-MON format
# (plus a few June/July/August dates entered as 6.X or 7.X)
mmdd <- "[0-9]{2}/[0-9]{2}"
mdd <- "[0-9]/[0-9]{2}"
mmd <- "[0-9]{2}/[0-9]"
md <- "[0-9]/[0-9]"
ddMON <- "[0-9]{2}-[A-Za-z]{3}"
dMON <- "[0-9]-[A-Za-z]{3}"
m.dd <- "[6-8][.][0-9]{2}"
m.d <- "[6-8][.][0-9]"
mdd2 <- "[0-9]-[0-9]{2}"
md2 <- "[0-9]-[0-9]"
dates <- paste0(c(mmdd, mdd, mmd, md, ddMON, dMON, m.dd, m.d, mdd2, md2), 
                collapse = "|")

# Extract date (in any format) from r1 column
datesd <- str_extract(recap$r01, dates)
sum(is.na(datesd)) 
# 66 entries where a date wasn't extracted (almost all are those I moved from
# banded dataset)

sort(unique(datesd)) # Most look ok (but need to check 4/16, 61/7, 7/63, 9/22)

# Check all other recapture columns
r217 <- as.matrix(recap[,paste0("r", str_pad(2:17, width = 2, pad = "0"))])
# Create a vector (reads down r02 column, then r03 column, etc)
recap217 <- as.vector(ifelse(is.na(r217), 0, 1))
# Extract dates
dates217 <- str_extract(r217, dates)
sort(unique(dates217)) # Most look ok (but need to check 3/16, 6/64, 7/63, 8/83)

recap217_check <- which(recap217 == 1 & is.na(dates217))
# Find rows with missing date entries
rows_missingdates <- arrayInd(recap217_check, .dim = dim(r217))[,1]
recap[rows_missingdates,]
