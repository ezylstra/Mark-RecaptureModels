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
recaptured <- read.csv('output/cleaned-recaptured-data-RMNP.csv')

# Merge both data sets
dat <- full_join(banded, recaptured)

# Remove X column and sort band numbers by band number, year, and band_status
dat <- dat %>% 
  select(-X) %>% 
  arrange(UBI_band, year, band_status) 

# Check data set

# Verify that first use of a band number corresponds to band status 1 (new),
# and following captures correspond to band status R (recapture) 

# Adapted from Erin's code

# Extract unique band numbers
unique.bands <- unique(dat$UBI_band)

# Create new "best_band_status"
dat$best_band_status <- NA

# Create a capture number column and add sequence of captures
dat$cap_number <- sequence(from = 1, rle(dat$UBI_band)$lengths)

# Fill in best_band_status with 1 and R  
dat$best_band_status <- ifelse(dat$cap_number == 1, "1", "R")
count(dat, band_status, best_band_status)
# Looks like there are 60 instances with wrong status

# Identify the errors
dat$check <- ifelse(dat$best_band_status != dat$band_status, '1', '0')

# Errors and fixes: 

# I searched each band (60) individually in the clean-RMNP-banded-data.R and 
# clean-RMNP-recaptured-data.R scripts, and in the banded and recaptured data
# sets to identify the problems with the bands. Most of them needed to be updated
# to the replaced band number. 

#1 Band 3100-42456 is former, need to change to the new one 9100-22920
dat$UBI_band[dat$UBI_band == '3100-42456'] <- '9100-22920'

#2 Band 3100-42457 not found in banded.dat, but the recaptured information in 
# Excel file says it was originally banded on 6/18/2010 @ KV1, then recaptured
# in 2011. Delete record.
dat <- dat %>% 
  filter(UBI_band != '3100-42457')

#3 Band 3100-42458 not found in banded.dat, but the recaptured information in 
# Excel file says it was originally banded on 6/19/2010 @ MP1, then recaptured
# in 2011. Delete record.
dat <- dat %>% 
  filter(UBI_band != '3100-42458')

#4 Band 3100-42514 is former, need to change to the new one 4100-59564
dat$UBI_band[dat$UBI_band == '3100-42514'] <- '4100-59564'

#5 Band 3100-42840 is former, need to change to the new one 4100-08515
dat$UBI_band[dat$UBI_band == '3100-42840'] <- '4100-08515'

#6 Band 4000-47686 is former, need to change to the new one 9000-12075
dat$UBI_band[dat$UBI_band == '4000-47686'] <- '9000-12075'

#7 Band 4000-47796 is former, need to change to the new one 9000-90377
dat$UBI_band[dat$UBI_band == '4000-47796'] <- '9000-90377'

#8 Band 4100-08231 is former, need to change to the new one 4100-58648
dat$UBI_band[dat$UBI_band == '4100-08231'] <- '4100-58648'

#9 Band 4100-08319 is former, need to change to the new one 4100-59347
dat$UBI_band[dat$UBI_band == '4100-08319'] <- '4100-59347'

#10 Band 4100-58654 has just one recapture record because first capture was
# identified as RUHU. Delete this record
dat <- dat %>% 
  filter(UBI_band != '4100-58654')

#11 Band 4100-58885 is former, need to change to the new one 9100-22970
dat$UBI_band[dat$UBI_band == '4100-58885'] <- '9100-22970'

#12 Band 4100-58937 is former, need to change to the new one 9100-23557
dat$UBI_band[dat$UBI_band == '4100-58937'] <- '9100-23557'

#13 Band 4100-59259 is former, need to change to the new one 9100-22805
dat$UBI_band[dat$UBI_band == '4100-59259'] <- '9100-22805'

#14 Band 4100-59326 is former, need to change to the new one 9100-23579
dat$UBI_band[dat$UBI_band == '4100-59326'] <- '9100-23579'

#15 Band 5000-11757 is former, need to change to the new one 4100-08484
dat$UBI_band[dat$UBI_band == '5000-11757'] <- '4100-08484'

#16 Band 5000-11768 is former, need to change to the new one 9000-12256
dat$UBI_band[dat$UBI_band == '5000-11768'] <- '9000-12256'

#17 Band 5000-29452 is former, need to change to the new one 3100-42459
dat$UBI_band[dat$UBI_band == '5000-29452'] <- '3100-42459'

#18 Band 5000-29478 is former, need to change to the new one 9000-90216
dat$UBI_band[dat$UBI_band == '5000-29478'] <- '9000-90216' 

#19 Band 5000-29486 is former, need to change to the new one 3100-42126
dat$UBI_band[dat$UBI_band == '5000-29486'] <- '3100-42126' 

#20 Band 5000-29552 is former, need to change to the new one 9000-39955
dat$UBI_band[dat$UBI_band == '5000-29552'] <- '9000-39955' 

#21 Band 5000-29595 is former, need to change to the new one 9000-90883
dat$UBI_band[dat$UBI_band == '5000-29595'] <- '9000-90883'

#22 Band 5000-96732 is former, need to change to the new one 3100-41814
dat$UBI_band[dat$UBI_band == '5000-96732'] <- '3100-41814'

#23 Band 5000-96838 is former, need to change to the new one 4100-58824
dat$UBI_band[dat$UBI_band == '5000-96838'] <- '4100-58824'

#24 Band 5000-96917 not found in banded.dat, but the recaptured information in 
# Excel file says it was originally banded on 8/20/2004 @ MP1, then recaptured
# in 2005. Delete record
dat <- dat %>% 
  filter(UBI_band != '5000-96917')

#25 Band 6000-23412 is former, need to change to the new one 3100-41738
dat$UBI_band[dat$UBI_band == '6000-23412'] <- '3100-41738'

#26 Band 6000-23503 is former, need to change to the new one 3100-41595
dat$UBI_band[dat$UBI_band == '6000-23503'] <- '3100-41595'

#27 Band 6000-23694 is former, need to change to the new one 9000-90908
dat$UBI_band[dat$UBI_band == '6000-23694'] <- '9000-90908'

#28 Band 6000-23745 is former, need to change to the new one 9000-90414
dat$UBI_band[dat$UBI_band == '6000-23745'] <- '9000-90414'

#29 Band 6000-23805 is former, need to change to the new one 9000-91047
dat$UBI_band[dat$UBI_band == '6000-23805'] <- '9000-91047'

#30 Band 6000-53578 is former, need to change to the new one 9000-11915
dat$UBI_band[dat$UBI_band == '6000-53578'] <- '9000-11915'

#31 Band 6000-53587 is former, need to change to the new one 9000-39914
dat$UBI_band[dat$UBI_band == '6000-53587'] <- '9000-39914'

#32 Band 6000-53656 is former, need to change to the new one 4100-08685
dat$UBI_band[dat$UBI_band == '6000-53656'] <- '4100-08685'

#33 Band 6000-53668 is former, need to change to the new one 3100-41591
dat$UBI_band[dat$UBI_band == '6000-53668'] <- '3100-41591'

#34 Band 6000-80944 is former, need to change to the new one 4100-08367
dat$UBI_band[dat$UBI_band == '6000-80944'] <- '4100-08367'

#35 Band 6000-80978 is former, need to change to the new one 9000-90273
dat$UBI_band[dat$UBI_band == '6000-80978'] <- '9000-90273'

#36 Band 9000-11812 is former, need to change to the new one 3100-41563
dat$UBI_band[dat$UBI_band == '9000-11812'] <- '3100-41563'

#73 Band 9000-11917 is former, need to change to the new one 9000-91152
dat$UBI_band[dat$UBI_band == '9000-11917'] <- '9000-91152'

#38 Band 9000-11985 is former, need to change to the new one 3100-41493
dat$UBI_band[dat$UBI_band == '9000-11985'] <- '3100-41493'

#39 Band 9000-11994 is former, need to change to the new one 9000-90668
dat$UBI_band[dat$UBI_band == '9000-11994'] <- '9000-90668'

#40 Band 9000-12042 is former, need to change to the new one 3100-41757
dat$UBI_band[dat$UBI_band == '9000-12042'] <- '3100-41757'

#41 Band 9000-12081 is former, need to change to the new one 9000-39458
dat$UBI_band[dat$UBI_band == '9000-12081'] <- '9000-39458'

#42 Band 9000-12462 is former, need to change to the new one 3100-42019
dat$UBI_band[dat$UBI_band == '9000-12462'] <- '3100-42019'

#43 Band 9000-29930 not found in banded.dat, but the recaptured information in 
# Excel file says it was originally banded on 7/31/2007 @ MP1, then recaptured
# in 2008. Delete record.
dat <- dat %>% 
  filter(UBI_band != '9000-29930')

#44 Band 9000-39004 not found in banded.dat, but the recaptured information in 
# Excel file says it was originally banded on 9/13/2006 @ NCOS, then recaptured
# in 2007 @ MCGC. Delete record.
dat <- dat %>% 
  filter(UBI_band != '9000-39004')

#45 Band 9000-39250 is former, need to change to the new one 9000-90316
dat$UBI_band[dat$UBI_band == '9000-39250'] <- '9000-90316'

#46 Band 9000-39397 is former, need to change to the new one 4100-08644
dat$UBI_band[dat$UBI_band == '9000-39397'] <- '4100-08644'

#47 Band 9000-39491 is former, need to change to the new one 9000-40106
dat$UBI_band[dat$UBI_band == '9000-39491'] <- '9000-40106'

#48 Band 9000-39509 is former, need to change to the new one 3100-42251
dat$UBI_band[dat$UBI_band == '9000-39509'] <- '3100-42251'

#49 Band 9000-39562 is former, need to change to the new one 3100-41632
dat$UBI_band[dat$UBI_band == '9000-39562'] <- '3100-41632'

#50 Band 9000-39584 is former, need to change to the new one 3100-41887
dat$UBI_band[dat$UBI_band == '9000-39584'] <- '3100-41887'

#51 Band 9000-39900 is former, need to change to the new one 4100-08151
dat$UBI_band[dat$UBI_band == '9000-39900'] <- '4100-08151'

#52 Band 9000-39959 is former, need to change to the new one 3100-42079
dat$UBI_band[dat$UBI_band == '9000-39959'] <- '3100-42079'

#53 Band 9000-41405 not found in banded.dat, but the recaptured information in 
# Excel file says it was originally banded on 8/26/2008 @ MP1, then recaptured
# in 2009. Delete record. 
dat <- dat %>% 
  filter(UBI_band != '9000-41405')

#54 Band 9000-41411 not found in banded.dat, but the recaptured information in 
# Excel file says it was originally banded on 8/26/2008 @ MP1, then recaptured
# in 2010. Delet record.
dat <- dat %>% 
  filter(UBI_band != '9000-41411')

#55 Band 9000-41412 not found in banded.dat, but the recaptured information in 
# Excel file says it was originally banded on 8/26/2008 @ MP1, then recaptured
# in 2011. Delete record.
dat <- dat %>% 
  filter(UBI_band != '9000-41412')

#56 Band 9000-90129 is former, need to change to the new one 9100-24000
dat$UBI_band[dat$UBI_band == '9000-90129'] <- '9100-24000'

#57 Band 9000-90282 is former, need to change to the new one 3100-42160
dat$UBI_band[dat$UBI_band == '9000-90282'] <- '3100-42160'

#58 Band 9000-90309 is former, need to change to the new one 3100-41731
dat$UBI_band[dat$UBI_band == '9000-90309'] <- '3100-41731'

#59 Band 9000-90323 is former, need to change to the new one 4100-59034
dat$UBI_band[dat$UBI_band == '9000-90323'] <- '4100-59034'

#60 Band 9000-90910 is former, need to change to the new one 4100-59344
dat$UBI_band[dat$UBI_band == '9000-90910'] <- '4100-59344'

# Arrange the band numbers after fixing the errors
dat <- dat %>% 
  arrange(UBI_band, year, band_status)
  
# Run the checks again after fixing the errors 

# Extract unique band numbers
unique.bands <- unique(dat$UBI_band)

# Create new "best_band_status"
dat$best_band_status <- NA

# Create a capture number column and add sequence of captures
dat$cap_number <- sequence(from = 1, rle(dat$UBI_band)$lengths)

# Fill in best_band_status with 1 and R  
dat$best_band_status <- ifelse(dat$cap_number == 1, "1", "R")
count(dat, band_status, best_band_status)
# No more errors!

# Remove unnecessary columns created to check recaptures
dat <- dat %>% 
  select(-c(best_band_status, cap_number, check))

# Identify if first capture of all individuals have age 
age.check <- dat %>% 
  filter(band_status == 1)
unique(age.check$age)
# There are no NA values for age, therefore all first captures have the age in 
# the data

# Edit column names
dat <- dat %>% 
  rename(sex = fixed_sex)

# Add the location in the park (east or west) to all the sites
dat$location <- ifelse(dat$site %in% c('CC3', 'CC2', 'GC1', 'HPE', 'HPK1', 'HPK2',
                                       'MCGC', 'MP1', 'NFPC', 'WB1', 'WB2', 'WC1',
                                       'WPK1'), 'east', 'west')

# Check that each band number is associated with a single sex
sex.check <- dat %>%
  group_by(UBI_band) %>%
  summarize(n_sex = length(unique(sex)))

# Extract band numbers associated with multiple sexes 
(error.sex <- sex.check$UBI_band[sex.check$n_sex > 1])

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
# one record had enough information to fix the misidentified sex (9000-40183).

# Change sex from F to M for record 9000-40183
band <- dat %>% 
  filter(UBI_band == '9000-40183') %>% 
  mutate(new_sex = 'M') %>% 
  select(-sex) %>% 
  rename(sex = new_sex) %>% 
  select(UBI_band, band_status, year, species, age, sex, site, location)

# Remove problematic bands
dat <- dat %>% 
  filter(!UBI_band %in% c(error.sex))

# Add fixed band number to main data set
dat.final <- full_join(dat, band)

#Export csv of final data frame ready for survival analysis 
write.csv(dat.final, 'output/cleaned-capture-data-RMNP.csv')
