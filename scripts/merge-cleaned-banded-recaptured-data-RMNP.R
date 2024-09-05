# Merge banded and recaptured data for RMNP project
# Gaby Samaniego
# gabysamaniego@arizona.edu
# 2024-01-02

# Load packages
library(tidyverse)

# Clear environment
rm(list = ls())

# Read csv files to merge
banded <- read.csv('output/cleaned-banded-data-RMNP.csv')
recap <- read.csv('output/cleaned-recaptured-data-RMNP-full.csv',
                       na.strings = c(NA, ""))
recap.12 <- read.csv('output/cleaned-recaptured-data-2012-RMNP.csv')

# Clean data sets
banded <- banded %>% 
  select(-X) %>% 
  rename(band = UBI_band,
         sex = fixed_sex)

recap <- recap %>% 
  rename(recap_year = recap_yr)

recap.12 <- recap.12 %>% 
  select(-X) %>% 
  rename(recap_year = recap_yr,
         sex = recaptured_sex,
         site_orig = site_recap) %>% 
  select(-recapture_age)

# Rename recapture columns
colnames(recap)[colnames(recap) %in% paste0("r", 1:17)] <- 
  paste0("r", str_pad(1:17, width = 2, pad = "0"))

colnames(recap.12)[colnames(recap.12) %in% paste0("r", 1)] <- 
  paste0("r", str_pad(1, width = 2, pad = "0"))

# Merge recapture data sets
recap <- full_join(recap, recap.12)

# Move information about rebanding events from banded to recap ----------------#

# When a bird was rebanded, the new band number was entered in the banded data set,
# but this is a recapture event, so here we are moving the recapture events to the 
# recap data set. Change the column names to differentiate them in the new data sets

# Select all bands with status 1 = first time applied
band_first <- banded %>%
  filter(band_status == "1") %>%
  select(-band_status) %>%
  rename(band_year = year,
         band_age = age, 
         band_sex = sex,
         band_site = site)

# Extract rebanded birds from banded data frame and merge with band_first. Now we 
# have information for the first time the band was applied and the reband event
rebanded <- banded %>% 
  filter(band_status == 'R') %>% 
  rename(reband_site = site,
         reband_year = year,
         reband_sex = sex,
         reband_age = age) %>%
  left_join(band_first, by = 'band')

# Check if band year and reband year are the same
filter(rebanded, reband_year == band_year) 
# 5 rebands in same year, always at same site
# Delete these from rebanded data set. 
rebanded <- rebanded %>%
  filter(reband_year != band_year)

# Create new data frame with all rebands that need to be added to the recapture
# data set. Change column names and format to match recap data set
rebands_to_recap <- rebanded %>%
  select(-c(reband_age, band_year, band_age, band_sex)) %>% 
  rename(recap_year = reband_year, # Rename columns to match recap data set
         sex = reband_sex,  # checked that sex is always same (orig, reband)
         r01 = reband_site,
         site_orig = band_site) %>%
  relocate(site_orig, .before = r01) %>%
  mutate(r02 = NA, r03 = NA, r04 = NA, r05 = NA, r06 = NA, r07 = NA, r08 = NA, 
         r09 = NA, r10 = NA, r11 = NA, r12 = NA, r13 = NA, r14 = NA, r15 = NA, 
         r16 = NA, r17 = NA, comments = NA)

# Merge selected bands to recap data set
recap <- rbind(recap, rebands_to_recap)

# Checks

# Check if there is an entry in the first recapture column for each bird

# Are there any NA values in r01?
sum(is.na(recap$r01)) # Yes, 7 NAs
filter(recap, is.na(r01))

# There are 7 rows with no entry in recapture columns (foreign, rebands, dead)

# 2 dead, will remove
# 4 with NEW BAND and former number in comments, but this recapture info is in
# banded, so can remove here too
recap <- filter(recap, !(is.na(r01) & !is.na(comments)))

# 1 with no comments (9000-12585). Maybe didn't enter info, but seems ok.
# Going to enter fake date into r1 column for remaining row (9000-12585 in 2010) 
# so that the recap info doesn't disappear when convert to long form
recap$r01[is.na(recap$r01)] <- "7/15"

# Extract information from each recapture event to obtain site when different
# than original band site ---------------------------------------------------- #

# First convert to long form. Create new columns to store information, remove
# entries with NA values, and remove duplicates
recaplong <- recap %>%
  pivot_longer(cols = r01:r17, 
               names_to = "recap_num",
               values_to = "entry") %>%
  data.frame() %>%
  filter(!is.na(entry)) %>%
  distinct()

# Merge this info with that in banded data set (just original banding events)
recaplong <- left_join(recaplong, band_first, by = 'band')

# Check that band_site (banded) == site_orig (recaplong)
filter(recaplong, site_orig != band_site) 
# Not always the same, so replace site_orig with band_site
recaplong <- recaplong %>%
  select(-site_orig)

# Load information about sites
sites_df <- read.csv("data/RMNP-sites-data.csv")
sites <- sort(unique(sites_df$site))

# Search for sites' names in recapture comments (entry), extract this 
# information in a new column and create indicator column when word 'at' was 
# detected
sites_search <- paste0(c(sites, tolower(sites)), collapse = "|")
recaplong <- recaplong %>%
  mutate(site = toupper(str_extract(entry, pattern = sites_search))) %>%
  mutate(at = if_else(str_detect(entry, "at"), 1, 0))

# Explore column site
unique(recaplong$site)

# Explore results when site was NA and column at = 1
filter(recaplong, at == 1 & is.na(site))
# Other locations found:
# YMCA (n = 6); HPK3 (6); Peaceful Valley, CO (1); ARIZONA (1)

# Explore site YMCA
filter(recaplong, str_detect(entry, "YMCA"))
# According to Google Maps, YMCA is close to MP1/HPK2/HPK1 complex. 
# According with Fred's reports YMCA banding site is approximately 1 mile to MP1
# All six records were recaptured by Scott Rashid who bands at YMCA. 
# Fred included these records in his data because these birds were originally 
# banded at the RMNP project banding sites. 

# Explore site HPK3
filter(recaplong, str_detect(entry, "HPK3"))
# According to Fred's reports: site HPK3 was active just in 2011 as part of
# a class. From his report: 'The Hollowell Park 3 site was located approximately 
# 200 meters west of HPK2'
# In Fred's report, the coordinates for HPK3 are the same as HPK2 and there are 
# no records of banded birds at HPK3, just recaptures

# Explore site ARIZONA and CO
filter(recaplong, str_detect(entry, 'ARIZONA|CO'))

# Remove Peaceful Valley and ARIZONA sites since they're outside park or unknown
recaplong <- recaplong %>%
  filter(!str_detect(entry, "Peaceful Valley|ARIZONA"))

# -----------
# Since we are removing the recaptured record because these birds were recaptured 
# outside the park, I think we should remove these two bands from banded too. 
# Or, since we know these birds were alive, should we create a fictitious entry
# at their original band site to keep the year their were recaptured?
# What do you think is best? 
# 9000-40083 banded in 2007 at MP1, recaptured in 2009 in Peaceful Valley, CO
# 9000-89901 banded in 2008 at HPK1, recaptured in 2011 in AZ
# It is just two bands, so probably they won't make a big difference.
# -----------

# Add two new sites where birds were recaptured
sites_search <- paste0(c(sites_search, "YMCA", "HPK3"), collapse = "|")
sites_to_add <- data.frame(site = c("YMCA", "HPK3"),
                           latitude = NA,
                           longitude = NA,
                           elevation = NA,
                           location = "east")
sites_df <- rbind(sites_df, sites_to_add)

# Create recap_site column and indicator column if recap year is the same as 
# banding year 
recaplong <- recaplong %>%
  mutate(site = toupper(str_extract(entry, pattern = sites_search))) %>%
  mutate(recap_site = if_else(is.na(site), band_site, site)) %>%
  mutate(first_year = if_else(recap_year == band_year, 1, 0)) %>%
  select(-c(at, site))

# Check that sex == band_sex
filter(recaplong, sex != band_sex)

# There are xx band numbers associated to two different sexes:
# 4000-47657 first capture HY in 2003 as M then as F in 2004: change sex to F
# 5000-29371 first capture HY in 2003 as F then as M in 2005: delete
# 4000-47693 first capture HY in 2003 as M then as F in 2005: change sex to F
# 5000-96694 first capture HY in 2004 as M then as F in 2005: delete
# 6000-81183 first capture AHY in 2006 as M then as F in 2009: delete
# 9000-40183 first capture HY in 2007 as M, then 2009 as F, then 2010 as M: change to M
# 3100-41634 first capture AHY in 2009 as M then as F in 2010: delete
# 3100-42083 first capture AHY in 2009 as F then as M in 2010: delete
# 4100-59872 first capture HY in 2011 as F then as AHY M in 2012: change to M
# 4100-08734 first capture AHY in 2010 as M then as AHY F in 2012: delete
# 4100-59950 first capture HY in 2011 as M then as AHY F in 2012: change to F
# 4100-59879 first capture HY in 2011 as M then as AHY F in 2012: delete

# After checking these bands in the original Excel files provided by Fred, some 
# have enough information to fix the misidentified sex and some need to be deleted

# Change sex from F to M for two records
bands.M <- recaplong %>% 
  filter(band %in% c('9000-40183','4100-59872')) %>% 
  mutate(new_sex = 'M') %>% 
  select(-sex) %>% 
  rename(sex = new_sex) %>% 
  relocate(sex, .after = recap_year)

# Add fixed band number to recaplong
recaplong <- bind_rows(recaplong, bands.M)

# Change sex from M to F for three records
bands.F <- recaplong %>% 
  filter(band %in% c('4000-47657','4000-47693', '4100-59950')) %>% 
  mutate(new_sex = 'F') %>% 
  select(-sex) %>% 
  rename(sex = new_sex) %>% 
  relocate(sex, .after = recap_year)

# Add fixed band number to recaplong
recaplong <- bind_rows(recaplong, bands.F)

# Remove bands with two sexes that can't be fixed
recaplong <- recaplong %>% 
  filter(!band %in% c('5000-29371', '5000-96694', '6000-81183', '3100-41634',
                      '3100-42083', '4100-08734', '4100-59879'))
                      
# Create data sets to indicate the number of capture for each band, year, site,
# and band status for band_first and recaplong 
band_site <- band_first %>%
  select(band, band_year, band_site) %>%
  rename(year = band_year,
         site = band_site) %>%
  mutate(band_status = "1") %>%
  mutate(recap_num = "r00")

recap_site <- recaplong %>%
  select(band, recap_year, recap_site, band_status, recap_num) %>%
  rename(year = recap_year,
         site = recap_site)

# Combine banding and recap info, removing duplicates (and R if in the first
# year at the same site)
cap_sites <- rbind(band_site, recap_site) %>%
  distinct() %>%
  arrange(band, band_status, year, recap_num, site) %>%
  distinct(band, year, site, .keep_all = TRUE) %>% 
  rename(cap_site = site)

# Merge cap_sites with band_first 
# For now I'm going to remove two bands recaptured outside RMNP that we removed
# from the recaptured data set in lines 159-161
dat <- cap_sites %>% 
  left_join(band_first, by = 'band') %>% 
  select(-c(recap_num, band_year)) %>% 
  rename(site_recap = cap_site,
         age_fc = band_age,
         sex = band_sex,
         site_fc = band_site) %>% 
  relocate(band, band_status, year, age_fc, sex, site_fc, site_recap) %>% 
  filter(!band %in% c('9000-40083', '9000-89901'))

# Check data set

# 1)

# Verify that first use of a band number corresponds to band status 1 (new),
# and following captures correspond to band status R (recapture) 

# Extract unique band numbers. Length 10213
unique.bands <- unique(dat$band)

# Create new "best_band_status"
dat$best_band_status <- NA

# Create a capture number column and add sequence of captures
dat$cap_number <- sequence(from = 1, rle(dat$band)$lengths)

# Fill in best_band_status with 1 and R  
dat$best_band_status <- ifelse(dat$cap_number == 1, "1", "R")
count(dat, band_status, best_band_status)
# Looks like there are 84 instances with wrong status

# Identify the errors
dat$check <- ifelse(dat$best_band_status != dat$band_status, '1', '0')
filter(dat, check == 1)

# Errors and fixes: 

# I searched each band (84) individually in the clean-RMNP-banded-data.R and 
# clean-RMNP-recaptured-data.R scripts, and in the banded and recaptured data
# sets to identify the problems with the bands. Most of them needed to be updated
# to the replaced band number. 

#1 Band 3100-42456 is former, need to change to the new one 9100-22920
dat$band[dat$band == '3100-42456'] <- '9100-22920'

#2 Band 3100-42457 not found in banded.dat, but the recaptured information in 
# Excel file says it was originally banded on 6/18/2010 @ KV1, then recaptured
# in 2011. Delete record.
dat <- dat %>% 
  filter(band != '3100-42457')

#3 Band 3100-42458 not found in banded.dat, but the recaptured information in 
# Excel file says it was originally banded on 6/19/2010 @ MP1, then recaptured
# in 2011. Delete record.
dat <- dat %>% 
  filter(band != '3100-42458')

#4 Band 3100-42514 is former, need to change to the new one 4100-59564
dat$band[dat$band == '3100-42514'] <- '4100-59564'

#5 Band 3100-42840 is former, need to change to the new one 4100-08515
dat$band[dat$band == '3100-42840'] <- '4100-08515'

#6 Band 4000-47686 is former, need to change to the new one 9000-12075
dat$band[dat$band == '4000-47686'] <- '9000-12075'

#7 Band 4000-47796 is former, need to change to the new one 9000-90377
dat$band[dat$band == '4000-47796'] <- '9000-90377'

#8 Band 4100-08231 is former, need to change to the new one 4100-58648
dat$band[dat$band == '4100-08231'] <- '4100-58648'

#9 Band 4100-08319 is former, need to change to the new one 4100-59347
dat$band[dat$band == '4100-08319'] <- '4100-59347'

#10 Band 4100-13781 couldn't find in banded data. Delete record
dat <- dat %>% 
  filter(band != '4100-13781')

#11 Band 4100-14147 couldn't find in banded data. Delete record
dat <- dat %>% 
  filter(band != '4100-14147')

#12 Band 4100-23221 couldn't find in banded data. Delete record
dat <- dat %>% 
  filter(band != '4100-23221')

#13 Band 4100-42504 couldn't find in banded data. Delete record
dat <- dat %>% 
  filter(band != '4100-42504')

#14 Band 4100-57631 couldn't find in banded data. Delete record
dat <- dat %>% 
  filter(band != '4100-57631')

#15 Band 4100-58654 has just one recapture record because first capture was
# identified as RUHU. Delete this record
dat <- dat %>% 
  filter(band != '4100-58654')

#16 Band 4100-58885 is former, need to change to the new one 9100-22970
dat$band[dat$band == '4100-58885'] <- '9100-22970'

#17 Band 4100-58937 is former, need to change to the new one 9100-23557
dat$band[dat$band == '4100-58937'] <- '9100-23557'

#18 Band 4100-59259 is former, need to change to the new one 9100-22805
dat$band[dat$band == '4100-59259'] <- '9100-22805'

#19 Band 4100-59326 is former, need to change to the new one 9100-23579
dat$band[dat$band == '4100-59326'] <- '9100-23579'

#20 Band 4100-59797 is a RUHU hummingbird. Delete record
dat <- dat %>% 
  filter(band != '4100-59797')

#21 Band 5000-11757 is former, need to change to the new one 4100-08484
dat$band[dat$band == '5000-11757'] <- '4100-08484'

#22 Band 5000-11768 is former, need to change to the new one 9000-12256
dat$band[dat$band == '5000-11768'] <- '9000-12256'

#23 Band 5000-29452 is former, need to change to the new one 3100-42459
dat$band[dat$band == '5000-29452'] <- '3100-42459'

#24 Band 5000-29478 is former, need to change to the new one 9000-90216
dat$band[dat$band == '5000-29478'] <- '9000-90216' 

#25 Band 5000-29486 is former, need to change to the new one 3100-42126
dat$band[dat$band == '5000-29486'] <- '3100-42126' 

#26 Band 5000-29552 is former, need to change to the new one 9000-39955
dat$band[dat$band == '5000-29552'] <- '9000-39955' 

#27 Band 5000-29595 is former, need to change to the new one 9000-90883
dat$band[dat$band == '5000-29595'] <- '9000-90883'

#28 Band 5000-96732 is former, need to change to the new one 3100-41814
dat$band[dat$band == '5000-96732'] <- '3100-41814'

#29 Band 5000-96838 is former, need to change to the new one 4100-58824
dat$band[dat$band == '5000-96838'] <- '4100-58824'

#30 Band 5000-96917 not found in banded.dat, but the recaptured information in 
# Excel file says it was originally banded on 8/20/2004 @ MP1, then recaptured
# in 2005. Delete record
dat <- dat %>% 
  filter(band != '5000-96917')

#31 Band 6000-23412 is former, need to change to the new one 3100-41738
dat$band[dat$band == '6000-23412'] <- '3100-41738'

#32 Band 6000-23503 is former, need to change to the new one 3100-41595
dat$band[dat$band == '6000-23503'] <- '3100-41595'

#33 Band 6000-23694 is former, need to change to the new one 9000-90908
dat$band[dat$band == '6000-23694'] <- '9000-90908'

#34 Band 6000-23745 is former, need to change to the new one 9000-90414
dat$band[dat$band == '6000-23745'] <- '9000-90414'

#35 Band 6000-23805 is former, need to change to the new one 9000-91047
dat$band[dat$band == '6000-23805'] <- '9000-91047'

#36 Band 6000-53578 is former, need to change to the new one 9000-11915
dat$band[dat$band == '6000-53578'] <- '9000-11915'

#37 Band 6000-53587 is former, need to change to the new one 9000-39914
dat$band[dat$band == '6000-53587'] <- '9000-39914'

#38 Band 6000-53656 is former, need to change to the new one 4100-08685
dat$band[dat$band == '6000-53656'] <- '4100-08685'

#39 Band 6000-53668 is former, need to change to the new one 3100-41591
dat$band[dat$band == '6000-53668'] <- '3100-41591'

#40 Band 6000-80944 is former, need to change to the new one 4100-08367
dat$band[dat$band == '6000-80944'] <- '4100-08367'

#41 Band 6000-80978 is former, need to change to the new one 9000-90273
dat$band[dat$band == '6000-80978'] <- '9000-90273'

#42 Band 9000-11812 is former, need to change to the new one 3100-41563
dat$band[dat$band == '9000-11812'] <- '3100-41563'

#43 Band 9000-11917 is former, need to change to the new one 9000-91152
dat$band[dat$band == '9000-11917'] <- '9000-91152'

#44 Band 9000-11985 is former, need to change to the new one 3100-41493
dat$band[dat$band == '9000-11985'] <- '3100-41493'

#45 Band 9000-11994 is former, need to change to the new one 9000-90668
dat$band[dat$band == '9000-11994'] <- '9000-90668'

#46 Band 9000-12042 is former, need to change to the new one 3100-41757
dat$band[dat$band == '9000-12042'] <- '3100-41757'

#47 Band 9000-12081 is former, need to change to the new one 9000-39458
dat$band[dat$band == '9000-12081'] <- '9000-39458'

#48 Band 9000-12462 is former, need to change to the new one 3100-42019
dat$band[dat$band == '9000-12462'] <- '3100-42019'

#49 Band 9000-23006 couldn't find in banded data. Delete record
dat <- dat %>% 
  filter(band != '9000-23006')

#50 Band 9000-29930 not found in banded.dat, but the recaptured information in 
# Excel file says it was originally banded on 7/31/2007 @ MP1, then recaptured
# in 2008. Delete record.
dat <- dat %>% 
  filter(band != '9000-29930')

#51 Band 9000-39004 not found in banded.dat, but the recaptured information in 
# Excel file says it was originally banded on 9/13/2006 @ NCOS, then recaptured
# in 2007 @ MCGC. Delete record.
dat <- dat %>% 
  filter(band != '9000-39004')

#52 Band 9000-39250 is former, need to change to the new one 9000-90316
dat$band[dat$band == '9000-39250'] <- '9000-90316'

#53 Band 9000-39397 is former, need to change to the new one 4100-08644
dat$band[dat$band == '9000-39397'] <- '4100-08644'

#54 Band 9000-39491 is former, need to change to the new one 9000-40106
dat$band[dat$band == '9000-39491'] <- '9000-40106'

#55 Band 9000-39509 is former, need to change to the new one 3100-42251
dat$band[dat$band == '9000-39509'] <- '3100-42251'

#56 Band 9000-39562 is former, need to change to the new one 3100-41632
dat$band[dat$band == '9000-39562'] <- '3100-41632'

#57 Band 9000-39584 is former, need to change to the new one 3100-41887
dat$band[dat$band == '9000-39584'] <- '3100-41887'

#58 Band 9000-39900 is former, need to change to the new one 4100-08151
dat$band[dat$band == '9000-39900'] <- '4100-08151'

#59 Band 9000-39959 is former, need to change to the new one 3100-42079
dat$band[dat$band == '9000-39959'] <- '3100-42079'

#60 Band 9000-41405 not found in banded.dat, but the recaptured information in 
# Excel file says it was originally banded on 8/26/2008 @ MP1, then recaptured
# in 2009. Delete record. 
dat <- dat %>% 
  filter(band != '9000-41405')

#61 Band 9000-41411 not found in banded.dat, but the recaptured information in 
# Excel file says it was originally banded on 8/26/2008 @ MP1, then recaptured
# in 2010. Delet record.
dat <- dat %>% 
  filter(band != '9000-41411')

#62 Band 9000-41412 not found in banded.dat, but the recaptured information in 
# Excel file says it was originally banded on 8/26/2008 @ MP1, then recaptured
# in 2011. Delete record.
dat <- dat %>% 
  filter(band != '9000-41412')

#63 Band 9000-58723 couldn't find in banded data. Delete record
dat <- dat %>% 
  filter(band != '9000-58723')

#64 Band 9000-90129 is former, need to change to the new one 9100-24000
dat$band[dat$band == '9000-90129'] <- '9100-24000'

#65 Band 9000-90282 is former, need to change to the new one 3100-42160
dat$band[dat$band == '9000-90282'] <- '3100-42160'

#66 Band 9000-90309 is former, need to change to the new one 3100-41731
dat$band[dat$band == '9000-90309'] <- '3100-41731'

#67 Band 9000-90323 is former, need to change to the new one 4100-59034
dat$band[dat$band == '9000-90323'] <- '4100-59034'

#68 Band 9000-90910 is former, need to change to the new one 4100-59344
dat$band[dat$band == '9000-90910'] <- '4100-59344'

#69 Band 9000-91361 is former, need to change to the new one 9100-23297
dat$band[dat$band == '9000-91361'] <- '9100-23297'

#70 Band 9000-91954 couldn't find in banded data. Delete record
dat <- dat %>% 
  filter(band != '9000-91954')

#71 Band 9000-93691 couldn't find in banded data. Delete record
dat <- dat %>% 
  filter(band != '9000-93691')

#72 Band 9000-93765 couldn't find in banded data. Delete record
dat <- dat %>% 
  filter(band != '9000-93765')

#73 Band 9100-08236 couldn't find in banded data. Delete record
dat <- dat %>% 
  filter(band != '9100-08236')

#74 Band 9100-23136 couldn't find in banded data. Delete record
dat <- dat %>% 
  filter(band != '9100-23136')

#75 Band 9100-23336 is a RUHU hummingbird. Delete record
dat <- dat %>% 
  filter(band != '9100-23336')

#76 Band 9100-23337 couldn't find in banded data. Delete record
dat <- dat %>% 
  filter(band != '9100-23337')

#77 Band 9100-24072 is a RUHU hummingbird. Delete record
dat <- dat %>% 
  filter(band != '9100-24072')

#78 Band 9100-24760 couldn't find in banded data. Delete record
dat <- dat %>% 
  filter(band != '9100-24760')

#79 Band 9100-28850 couldn't find in banded data. Delete record
dat <- dat %>% 
  filter(band != '9100-28850')

#80 Band 9100-59238 couldn't find in banded data. Delete record
dat <- dat %>% 
  filter(band != '9100-59238')

#81 Band 9100-59270 couldn't find in banded data. Delete record
dat <- dat %>% 
  filter(band != '9100-59270')
  
#82 Band 9100-90940 couldn't find in banded data. Delete record
dat <- dat %>% 
  filter(band != '9100-90940')

#83 Band 9111-12257 misread, need to change to 9000-12257
dat$band[dat$band == '9111-12257'] <- '9000-12257'

#84 Band 9199-22916 misread, need to change to 9100-22916
dat$band[dat$band == '9199-22916'] <- '9100-22916'

# Arrange the band numbers after fixing the errors
dat <- dat %>% 
  arrange(band, year, band_status)

# Run the checks again after fixing the errors 
# Extract unique band numbers
unique.bands <- unique(dat$UBI_band)

# Create new "best_band_status"
dat$best_band_status <- NA

# Create a capture number column and add sequence of captures
dat$cap_number <- sequence(from = 1, rle(dat$band)$lengths)

# Fill in best_band_status with 1 and R  
dat$best_band_status <- ifelse(dat$cap_number == 1, "1", "R")
count(dat, band_status, best_band_status)
# No more errors!

# Remove unnecessary columns created to check recaptures
dat <- dat %>% 
  select(-c(best_band_status, cap_number, check))

# Fill in NA values created in columns age_fc, sex, site_fc and site_recap when 
# changed band numbers to the 54 bands between code lines 256 and 616 
dat <- dat %>%
  arrange(band, year, band_status) %>%
  group_by(band) %>%
  mutate(age = age_fc[1],
         sex_2 = sex[1],
         site = site_fc[1],
         site_recap_2 = if_else(is.na(site_recap), site, site_recap)) %>% 
  ungroup() %>%
  select(-c(age_fc, sex, site_fc, site_recap)) %>% 
  rename(sex = sex_2,
         site_recap = site_recap_2) %>% 
  relocate(site_recap, .after = site) %>% 
  distinct() %>% 
  data.frame()

# 2)

# Check values in age
unique(dat$age) # No NA values

# Check that each band number is associated with a single age
age_check <- dat %>%
  group_by(band) %>%
  summarize(n_age = length(unique(age)))

# Extract band numbers associated # Extract band numbers associated # Extract band numbers associated with multiple sexes 
(error_age <- age_check$band[age_check$n_age > 1])

# Checks for juveniles recaptures 
bandcheck <- unique(dat$band[dat$age == 'HY' &
                               dat$band_status == "R"])

# 170 individuals captured multiple times, first as juveniles
dat[dat$band == bandcheck[5],]
dat[dat$band == bandcheck[100],]

# 3)

# Check values in sex
unique(dat$sex) # No NA values

# Check that each band number is associated with a single sex
sex.check <- dat %>%
  group_by(band) %>%
  summarize(n_sex = length(unique(sex)))

# Extract band numbers associated with multiple sexes 
(error.sex <- sex.check$band[sex.check$n_sex > 1])

# Add the location in the park (east or west) to all the sites
dat$location <- ifelse(dat$site %in% c('CC3', 'CC2', 'GC1', 'HPE', 'HPK1', 'HPK2',
                                       'MCGC', 'MP1', 'NFPC', 'WB1', 'WB2', 'WC1',
                                        'WPK1'), 'east', 'west')

#Export csv of final data frame ready for survival analysis 
write.csv(dat, 'output/cleaned-capture-data-RMNP.csv') 
