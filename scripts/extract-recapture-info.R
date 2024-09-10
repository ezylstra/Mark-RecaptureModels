# Extracting location and dates of captures

# IMPORTANT: Most all of this code created by G. Samaniego in 
# merge-cleaned-banded-recaptured-data-RMNP.R. Copied here to create a version
# of the dataset that can help answer 2 questions:
  # 1) Are individuals captured in August or September likely to be transients?
  # 2) Should we differentiate between capture locations in analyses? (assessed 
  #    by looking at movements between sites)

# Load packages
library(tidyverse)
library(terra)

# Load cleaned, full recap file:
recap <- read.csv("output/cleaned-recaptured-data-RMNP-full.csv",
                  na.strings = c(NA, ""))
colnames(recap)[colnames(recap) %in% paste0("r", 1:17)] <- 
  paste0("r", str_pad(1:17, width = 2, pad = "0"))
recap <- recap %>% 
  rename(recap_year = recap_yr)

# Load cleaned banded file that includes date:
banded <- read.csv("output/cleaned-banded-data-RMNP-date.csv") %>%
  rename(sex = fixed_sex,
         band = UBI_band)

# Load information about sites
sites_df <- read.csv("data/RMNP-sites-data.csv")
sites <- sort(unique(sites_df$site))

# Move information about rebanding events from banded to recap ----------------#

# When a bird was rebanded, the new band number was entered in the banded data set,
# but this is a recapture event, so here we are moving the recapture events to the 
# recap data set. Change the column names to differentiate them in the new data sets

# Select all bands with status 1 = first time applied
band_first <- banded %>%
  filter(band_status == "1") %>%
  select(-band_status) %>%
  rename(band_year = year,
         band_date = date,
         band_age = age, 
         band_sex = sex,
         band_site = site)

# Extract rebanded birds from banded data frame and merge with band_first. Now we 
# have information for the first time the band was applied and the reband event
rebanded <- banded %>% 
  filter(band_status == 'R') %>% 
  rename(reband_site = site,
         reband_year = year,
         reband_date = date,
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
  select(-c(reband_age, band_year, band_date, band_age, band_sex)) %>% 
  mutate(r01 = paste0(month(reband_date), "/", day(reband_date), ", ", 
                      reband_site)) %>%
  rename(recap_year = reband_year, # Rename columns to match recap data set
         sex = reband_sex,  # checked that sex is always same (orig, reband)
         site_orig = band_site) %>%
  select(-c(reband_date, reband_site)) %>%
  mutate(r02 = NA, r03 = NA, r04 = NA, r05 = NA, r06 = NA, r07 = NA, r08 = NA, 
         r09 = NA, r10 = NA, r11 = NA, r12 = NA, r13 = NA, r14 = NA, r15 = NA, 
         r16 = NA, r17 = NA, comments = NA)

# Merge selected bands to recap data set
recap <- rbind(recap, rebands_to_recap)

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
sites_df$location <- ifelse(str_detect(sites_df$site, west), "west", "east")

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
                           location = "east")
sites_df <- rbind(sites_df, sites_to_add)

# Remove recaps at other sites, create recap_site column and indicator if
# recap is in banding year (first_yr)
recapl <- recapl %>%
  filter(!str_detect(entry, "Peaceful Valley|ARIZONA")) %>%
  mutate(site = toupper(str_extract(entry, pattern = sites_search))) %>%
  mutate(recap_site = if_else(is.na(site), band_site, site)) %>%
  mutate(first_yr = if_else(recap_year == band_year, 1, 0)) %>%
  select(-c(at, site))

# Sexes -----------------------------------------------------------------------#

# Check that sex == band_sex
filter(recapl, sex != band_sex)

# There are 8 band numbers associated to two different sexes:
# 3100-41634 first capture AHY in 2009 M and 2010 F
# 3100-42083 first capture AHY in 2009 F and 2010 M
# 4000-47657 first capture HY in 2003 M and 2004 F
# 4000-47693 first capture HY in 2003 M and 2005 F
# 5000-29371 first capture HY in 2003 F and 2005 M
# 5000-96694 first capture HY in 2004 M and 2005 F
# 6000-81183 first capture AHY in 2006 M and 2009 F
# 9000-40183 first capture HY in 2007 M, then 2009 F, and 2010 M

# After checking these bands in the original Excel files provided by Fred, just 
# one record had enough information to fix the misidentified sex (9000-40183)

# Change sex from F to M for record 9000-40183
band <- recapl %>% 
  filter(band == '9000-40183') %>% 
  mutate(new_sex = 'M') %>% 
  select(-sex) %>% 
  rename(sex = new_sex) %>% 
  relocate(sex, .after = recap_year)

# Add fixed band number to recaplong
recapl <- bind_rows(filter(recapl, band != "9000-40183"), band)

# Remove bands with two sexes that can't be fixed
recapl <- recapl %>% 
  filter(!band %in% c('3100-41634', '3100-42083', '4000-47657', '4000-47693',
                      '5000-29371', '5000-96694', '6000-81183'))

# Check that now all sex == band_sex
# filter(recapl, sex != band_sex)

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

# Extract date (in any format) from entry column
datesd <- str_extract(recapl$entry, dates)
sum(is.na(datesd)) 
# Only 5 NAs
missing_dates <- which(is.na(datesd))
recapl[missing_dates,]
filter(recap, band == "5000-97088" & recap_year == 2005) # Dated recaps earlier in year
filter(recap, band == "5000-29276" & recap_year == 2008) # Only recap this year
filter(recap, band == "9000-90364" & recap_year == 2008) # Dated recaps earlier in year
filter(recap, band == "9000-90531" & recap_year == 2008) # Dated recaps earlier in year
filter(recap, band == "9000-90333" & recap_year == 2009) # Dated recaps earlier in year
# Add date to the r01 missing date
recapl$entry[missing_dates[2]] <- paste0("7/15, ", recapl$entry[missing_dates[2]])
# Delete other recaptures since they don't matter
recapl <- recapl[-missing_dates[c(1,3:5)],]

sort(unique(datesd)) # Most look ok (but need to deal with 6/64, 61/7, 7/63, 8/83)
recapl <- recapl %>%
  mutate(entry = str_replace(entry, "61/7", "6/17")) %>%
  mutate(entry = str_replace(entry, "6/64", "6/6")) %>%
  mutate(entry = str_replace(entry, "7/63", "7/6")) %>%
  mutate(entry = str_replace(entry, "8/83", "8/8"))

# Add dates to dataframe and reformat
recapl$recap_date_uf <- str_extract(recapl$entry, dates)  
recapl <- recapl %>%
  mutate(recap_date_uf2 = paste0(recap_year, "-", recap_date_uf)) %>%
  mutate(recap_date = parse_date_time(recap_date_uf2,
                                      orders = c("Y-d-b",
                                                 "Y-m.d",
                                                 "Y-m/d",
                                                 "Y-m-d"))) %>%
  mutate(recap_date = as.Date(recap_date),
         recap_month = month(recap_date),
         recap_day = day(recap_date))
  # Check
  # count(recapl, recap_month, recap_day, recap_date_uf)
  
# Remove unnecessary columns
recapl <- recapl %>%
  select(-c(recap_date_uf, recap_date_uf2, recap_month, recap_day))

# Are there multiple entries in recapl with the same band, date, location?
recap_ck <- recapl %>%
  group_by(band, recap_date) %>%
  summarize(n_recaps = n(),
            n_sites = length(unique(recap_site)),
            .groups = "keep") %>%
  data.frame()
# Look at duplicate band-dates
filter(recap_ck, n_recaps > 1)
# Can probably delete most, but... a few with multiple sites...
recap_ck2 <- filter(recap_ck, n_sites > 1)
for (i in 1:nrow(recap_ck2)) {
  print(i)
  print(filter(recapl,
               band == recap_ck2$band[i],
               recap_date == recap_ck2$recap_date[i]))
}
# Add an indicator to show if bird moved sites (that'll be the one we keep),
# then remove duplicate band-dates
recapl <- recapl %>%
  mutate(same_sites = ifelse(band_site == recap_site, 1, 0))
recapl <- recapl %>%
  arrange(band, recap_date, same_sites) %>%
  distinct(band, recap_date, .keep_all = TRUE)

# Fix band numbers in recap dataset where needed ------------------------------#

# Gaby searched each band (60) individually in the clean-RMNP-banded-data.R and 
# clean-RMNP-recaptured-data.R scripts, and in the banded and recaptured data
# sets to identify the problems with the bands. Most of them needed to be updated
# to the replaced band number. 

#1 Band 3100-42456 is former, need to change to the new one 9100-22920
recapl$band[recapl$band == '3100-42456'] <- '9100-22920'

#2 Band 3100-42457 not found in banded.recapl, but the recaptured information in 
# Excel file says it was originally banded on 6/18/2010 @ KV1, then recaptured
# in 2011. Delete record.
recapl <- recapl %>% 
  filter(band != '3100-42457')

#3 Band 3100-42458 not found in banded.recapl, but the recaptured information in 
# Excel file says it was originally banded on 6/19/2010 @ MP1, then recaptured
# in 2011. Delete record.
recapl <- recapl %>% 
  filter(band != '3100-42458')

#4 Band 3100-42514 is former, need to change to the new one 4100-59564
recapl$band[recapl$band == '3100-42514'] <- '4100-59564'

#5 Band 3100-42840 is former, need to change to the new one 4100-08515
recapl$band[recapl$band == '3100-42840'] <- '4100-08515'

#6 Band 4000-47686 is former, need to change to the new one 9000-12075
recapl$band[recapl$band == '4000-47686'] <- '9000-12075'

#7 Band 4000-47796 is former, need to change to the new one 9000-90377
recapl$band[recapl$band == '4000-47796'] <- '9000-90377'

#8 Band 4100-08231 is former, need to change to the new one 4100-58648
recapl$band[recapl$band == '4100-08231'] <- '4100-58648'

#9 Band 4100-08319 is former, need to change to the new one 4100-59347
recapl$band[recapl$band == '4100-08319'] <- '4100-59347'

#10 Band 4100-58654 has just one recapture record because first capture was
# identified as RUHU. Delete this record
recapl <- recapl %>% 
  filter(band != '4100-58654')

#11 Band 4100-58885 is former, need to change to the new one 9100-22970
recapl$band[recapl$band == '4100-58885'] <- '9100-22970'

#12 Band 4100-58937 is former, need to change to the new one 9100-23557
recapl$band[recapl$band == '4100-58937'] <- '9100-23557'

#13 Band 4100-59259 is former, need to change to the new one 9100-22805
recapl$band[recapl$band == '4100-59259'] <- '9100-22805'

#14 Band 4100-59326 is former, need to change to the new one 9100-23579
recapl$band[recapl$band == '4100-59326'] <- '9100-23579'

#15 Band 5000-11757 is former, need to change to the new one 4100-08484
recapl$band[recapl$band == '5000-11757'] <- '4100-08484'

#16 Band 5000-11768 is former, need to change to the new one 9000-12256
recapl$band[recapl$band == '5000-11768'] <- '9000-12256'

#17 Band 5000-29452 is former, need to change to the new one 3100-42459
recapl$band[recapl$band == '5000-29452'] <- '3100-42459'

#18 Band 5000-29478 is former, need to change to the new one 9000-90216
recapl$band[recapl$band == '5000-29478'] <- '9000-90216' 

#19 Band 5000-29486 is former, need to change to the new one 3100-42126
recapl$band[recapl$band == '5000-29486'] <- '3100-42126' 

#20 Band 5000-29552 is former, need to change to the new one 9000-39955
recapl$band[recapl$band == '5000-29552'] <- '9000-39955' 

#21 Band 5000-29595 is former, need to change to the new one 9000-90883
recapl$band[recapl$band == '5000-29595'] <- '9000-90883'

#22 Band 5000-96732 is former, need to change to the new one 3100-41814
recapl$band[recapl$band == '5000-96732'] <- '3100-41814'

#23 Band 5000-96838 is former, need to change to the new one 4100-58824
recapl$band[recapl$band == '5000-96838'] <- '4100-58824'

#24 Band 5000-96917 not found in banded.recapl, but the recaptured information in 
# Excel file says it was originally banded on 8/20/2004 @ MP1, then recaptured
# in 2005. Delete record
recapl <- recapl %>% 
  filter(band != '5000-96917')

#25 Band 6000-23412 is former, need to change to the new one 3100-41738
recapl$band[recapl$band == '6000-23412'] <- '3100-41738'

#26 Band 6000-23503 is former, need to change to the new one 3100-41595
recapl$band[recapl$band == '6000-23503'] <- '3100-41595'

#27 Band 6000-23694 is former, need to change to the new one 9000-90908
recapl$band[recapl$band == '6000-23694'] <- '9000-90908'

#28 Band 6000-23745 is former, need to change to the new one 9000-90414
recapl$band[recapl$band == '6000-23745'] <- '9000-90414'

#29 Band 6000-23805 is former, need to change to the new one 9000-91047
recapl$band[recapl$band == '6000-23805'] <- '9000-91047'

#30 Band 6000-53578 is former, need to change to the new one 9000-11915
recapl$band[recapl$band == '6000-53578'] <- '9000-11915'

#31 Band 6000-53587 is former, need to change to the new one 9000-39914
recapl$band[recapl$band == '6000-53587'] <- '9000-39914'

#32 Band 6000-53656 is former, need to change to the new one 4100-08685
recapl$band[recapl$band == '6000-53656'] <- '4100-08685'

#33 Band 6000-53668 is former, need to change to the new one 3100-41591
recapl$band[recapl$band == '6000-53668'] <- '3100-41591'

#34 Band 6000-80944 is former, need to change to the new one 4100-08367
recapl$band[recapl$band == '6000-80944'] <- '4100-08367'

#35 Band 6000-80978 is former, need to change to the new one 9000-90273
recapl$band[recapl$band == '6000-80978'] <- '9000-90273'

#36 Band 9000-11812 is former, need to change to the new one 3100-41563
recapl$band[recapl$band == '9000-11812'] <- '3100-41563'

#73 Band 9000-11917 is former, need to change to the new one 9000-91152
recapl$band[recapl$band == '9000-11917'] <- '9000-91152'

#38 Band 9000-11985 is former, need to change to the new one 3100-41493
recapl$band[recapl$band == '9000-11985'] <- '3100-41493'

#39 Band 9000-11994 is former, need to change to the new one 9000-90668
recapl$band[recapl$band == '9000-11994'] <- '9000-90668'

#40 Band 9000-12042 is former, need to change to the new one 3100-41757
recapl$band[recapl$band == '9000-12042'] <- '3100-41757'

#41 Band 9000-12081 is former, need to change to the new one 9000-39458
recapl$band[recapl$band == '9000-12081'] <- '9000-39458'

#42 Band 9000-12462 is former, need to change to the new one 3100-42019
recapl$band[recapl$band == '9000-12462'] <- '3100-42019'

#43 Band 9000-29930 not found in banded.recapl, but the recaptured information in 
# Excel file says it was originally banded on 7/31/2007 @ MP1, then recaptured
# in 2008. Delete record.
recapl <- recapl %>% 
  filter(band != '9000-29930')

#44 Band 9000-39004 not found in banded.recapl, but the recaptured information in 
# Excel file says it was originally banded on 9/13/2006 @ NCOS, then recaptured
# in 2007 @ MCGC. Delete record.
recapl <- recapl %>% 
  filter(band != '9000-39004')

#45 Band 9000-39250 is former, need to change to the new one 9000-90316
recapl$band[recapl$band == '9000-39250'] <- '9000-90316'

#46 Band 9000-39397 is former, need to change to the new one 4100-08644
recapl$band[recapl$band == '9000-39397'] <- '4100-08644'

#47 Band 9000-39491 is former, need to change to the new one 9000-40106
recapl$band[recapl$band == '9000-39491'] <- '9000-40106'

#48 Band 9000-39509 is former, need to change to the new one 3100-42251
recapl$band[recapl$band == '9000-39509'] <- '3100-42251'

#49 Band 9000-39562 is former, need to change to the new one 3100-41632
recapl$band[recapl$band == '9000-39562'] <- '3100-41632'

#50 Band 9000-39584 is former, need to change to the new one 3100-41887
recapl$band[recapl$band == '9000-39584'] <- '3100-41887'

#51 Band 9000-39900 is former, need to change to the new one 4100-08151
recapl$band[recapl$band == '9000-39900'] <- '4100-08151'

#52 Band 9000-39959 is former, need to change to the new one 3100-42079
recapl$band[recapl$band == '9000-39959'] <- '3100-42079'

#53 Band 9000-41405 not found in banded.recapl, but the recaptured information in 
# Excel file says it was originally banded on 8/26/2008 @ MP1, then recaptured
# in 2009. Delete record. 
recapl <- recapl %>% 
  filter(band != '9000-41405')

#54 Band 9000-41411 not found in banded.recapl, but the recaptured information in 
# Excel file says it was originally banded on 8/26/2008 @ MP1, then recaptured
# in 2010. Delet record.
recapl <- recapl %>% 
  filter(band != '9000-41411')

#55 Band 9000-41412 not found in banded.recapl, but the recaptured information in 
# Excel file says it was originally banded on 8/26/2008 @ MP1, then recaptured
# in 2011. Delete record.
recapl <- recapl %>% 
  filter(band != '9000-41412')

#56 Band 9000-90129 is former, need to change to the new one 9100-24000
recapl$band[recapl$band == '9000-90129'] <- '9100-24000'

#57 Band 9000-90282 is former, need to change to the new one 3100-42160
recapl$band[recapl$band == '9000-90282'] <- '3100-42160'

#58 Band 9000-90309 is former, need to change to the new one 3100-41731
recapl$band[recapl$band == '9000-90309'] <- '3100-41731'

#59 Band 9000-90323 is former, need to change to the new one 4100-59034
recapl$band[recapl$band == '9000-90323'] <- '4100-59034'

#60 Band 9000-90910 is former, need to change to the new one 4100-59344
recapl$band[recapl$band == '9000-90910'] <- '4100-59344'

# Remove a couple other bands
recapl <- recapl %>%
  filter(!band %in% c('9000-40083', '9000-89901'))

# Now that we've changed some band numbers, just grab recap columns and re-merge
# with band_first
recapl <- recapl %>%
  select(band, band_status, recap_year, sex, 
         recap_num, recap_site, recap_date) %>%
  left_join(band_first, by = "band") %>%
  mutate(band_date = as.Date(band_date))
# If recap_site is NA, insert band_site
recapl <- recapl %>%
  mutate(recap_site = ifelse(is.na(recap_site), band_site, recap_site))

  # Check dates
  filter(recapl, band_date > recap_date)
  # 5 instances where recap date was before band date
  # All in the same year.
  # Will assume that band_date is correct and make recap date 1 day later
  
recapl <- recapl %>%
  mutate(recap_date = if_else(band_date > recap_date, 
                              band_date + days(1),
                              recap_date))
  
# Combine with recapture data with banding data, so there's one row for every -#
# capture/recapture -----------------------------------------------------------#
recapl <- recapl %>%
  rename(year = recap_year, 
         obsdate = recap_date,
         site = recap_site) %>%
  select(band, band_status, year, sex, recap_num, obsdate, site, 
         band_year, band_age, band_site)
band_first <- band_first %>%
  mutate(band_status = "1",
         recap_num = "r00",
         year = band_year,
         site = band_site,
         obsdate = as.Date(band_date)) %>%
  rename(sex = band_sex) %>%
  select(band, band_status, year, sex, recap_num, obsdate, site, 
         band_year, band_age, band_site)  
allcaps <- rbind(band_first, recapl) %>%
  mutate(month = month(obsdate),
         day = day(obsdate))

# Removing one capture in March
allcaps <- filter(allcaps, month %in% 5:9)

# Movements between sites -----------------------------------------------------#
# For each bird, calculate the number of unique sites captured at (n_sites) as
# well as the number of movements (n_moves)

  bird_sites <- allcaps %>%
    arrange(band, obsdate) %>%
    group_by(band, band_site, band_year) %>%
    summarize(n_sites = length(unique(site)),
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
  movements <- allcaps %>%
    filter(band %in% bird_sites$band[bird_sites$n_moves > 0]) %>%
    arrange(band, band_status, year, obsdate) %>%
    mutate(site_previous = NA,
           time = NA)
  
  for (i in 2:nrow(movements)) {
    if (movements$band[i] == movements$band[i - 1]) {
      movements$site_previous[i] <- movements$site[i - 1]
      movements$time[i] <- movements$year[i] - movements$year[i - 1]
    } else {
      movements$site_previous[i] <- NA
      movements$time[i] <- NA
    }
  }
  
  movements <- movements %>%
    filter(site != site_previous) %>%
    mutate(same_yr = if_else(time > 0, 0, 1)) %>%
    select(-c(year, band_status, time))
  
  nrow(movements)                # 459 movements
  length(unique(movements$band)) # 355 birds
  count(movements, same_yr)      # 235 movements within year, 224 across years
  
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
  movements$site_ew <- sites_df$location[match(movements$site, sites_df$site)]
  movements$site_previous_ew <- sites_df$location[match(movements$site_previous, sites_df$site)]
  movements <- movements %>%
    mutate(over_ridge = if_else(site_ew != site_previous_ew, 1, 0)) %>%
    select(-c(site_ew, site_previous_ew))
  
  filter(movements, over_ridge == 1)
  filter(allcaps, band == unique(movements$band[movements$over_ridge == 1])[1])
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

# Start looking at transient questions (are individuals captured late in the --#
# season part of the breeding population or are they individuals moving -------#
# through? --------------------------------------------------------------------#

  # Group capture dates into May-Jul, Aug 1-15, Aug 16-31, Sep
  allcaps <- allcaps %>%
    mutate(MJJ = ifelse(month %in% 5:7, 1, 0),
           MJJA15 = ifelse(month %in% 5:7 | (month == 8 & day < 16), 1, 0),
           MJJA31 = ifelse(month %in% 5:8, 1, 0))
  
  # Identify when first capture occurred and when recaptures occurred
  bird_months <- allcaps %>%
    group_by(band) %>%
    summarize(n_captures = n(),
              n_years = length(unique(year)),
              first_date = obsdate[band_status == 1],
              n_MJJ = sum(MJJ),
              n_MJJA15 = sum(MJJA15),
              n_MJJA31 = sum(MJJA31)) %>%
    data.frame() %>%
    mutate(first_cap = case_when(
      month(first_date) %in% 5:7 ~ "MJJ",
      month(first_date) == 8 & day(first_date) < 16 ~ "Aug1",
      month(first_date) == 8 & day(first_date) > 15 ~ "Aug2",
      .default = "Sep")
    ) %>%
    mutate(first_cap = factor(first_cap, levels = c("MJJ", "Aug1", "Aug2", "Sep")))

