# Clean RMNP banding data
# Gaby Samaniego
# gabysamaniego@arizona.edu
# 2023-11-20

# Load packages
library(tidyverse)
library(data.table)
library(janitor)

# Clear environment
rm(list = ls())

# Read all csv files to merge

# Create a list of all files to merge
banded.files <- list.files(path = "data/RMNP-banded-by-year/", 
                               pattern = "*.csv", 
                               full.names = TRUE)
banded.dat <- lapply(banded.files, fread, sep = ",")  

# Combine all csv files in a data frame
banded.dat <- rbindlist(banded.dat, fill = TRUE)

# Replace space in column names with '_'
banded.dat <- banded.dat %>% 
  clean_names()

# ---------------- EXPLORE AND FIX COLUMNS NEEDED FOR THESIS ----------------- # 

# COLUMN DATE
# Change column date from character to class date
class(banded.dat$date)
banded.dat <- banded.dat %>% 
  mutate(date = mdy(date))

# Split column date by year, month, and day
banded.dat <- mutate(banded.dat, year = year(date), 
                    month = month(date),
                    day = day(date))  

# COLUMN SITE
count(banded.dat, site)

# Capitalize characters in column site 
banded.dat$site <- toupper(banded.dat$site)
#  BGMD    1
#   CC2   66
#   CC3  246
#   GC1   34
# GNMTN  833
#  HOLZ  599
#   HPE   22
#  HPK1  510
#  HPK2  313
#   KV1  757
#  MCGC 4531
#   MP1 3942
#  NFPC   17
#  POLC   10
#  SHIP    3
#   WB1   23
#   WB2   33
#   WC1   21
#  WPK1    2

# COLUMN SPECIES
count(banded.dat, species)
#  BTLH 10222
#  CAHU   249
#  RTHU     1
#  RUHU  1491

# COLUMN SEX
count(banded.dat, sex)
#  A  1      Checked band number 5000-11637 in recapture data to fix this record. This is a Male 
#  F  8239
#  M  3724

# Fix record with sex A
banded.dat$sex[banded.dat$sex == 'A'] <- 'M'
unique(banded.dat$sex)

# COLUMN AGE
count(banded.dat, age) 

# Change SY (second year) in age to AHY (after hatch-year)
banded.dat$age[banded.dat$age == 'SY'] <- 'AHY'
# AHY  9736
#  HY  2227
unique(banded.dat$age)

# COLUMN BAND NUMBERS
# From Erin's code:

# Look at format of entries in band_number column:
bn <- sort(unique(banded.dat$band_number)) # Length = 11963
table(nchar(bn)) # Count the number of characters in band numbers
# There are 11954 band numbers with 10 characters 
# There are 8 band numbers with 11 characters 
# There is 1 band number with 16 characters (9000-39805+A1218)

sum(str_detect(bn, "[0-9]{4}-[0-9]{5}") * nchar(bn) == 10)
# All 11954 band numbers with 10 characters are correct format (4 numbers - 5 numbers)

sum(str_detect(bn, "[0-9]{4}-[0-9]{5}\\*") * nchar(bn) == 11)
# All 8 band numbers with 11 characters are correct format with * at end.

# Fix band number with 16 characters
banded.dat$band_number[banded.dat$band_number == '9000-39805+A1218'] <- '9000-39805'

# Create a variable to indicate when an asterisk appears at end of band number
banded.dat <- banded.dat %>%
  mutate(band_asterisk = 1 * str_detect(band_number, "[0-9]{4}-[0-9]{5}\\*"))

# Find duplicate band numbers
if(any(duplicated(banded.dat$band_number))){ # Are there duplicate band numbers?
  duplicates <- which(duplicated(banded.dat$band_number))  # Which one is duplicated? 
  message(paste0("The following rows are duplicate band numbers: ",
                 paste0(duplicates, collapse = ",")))
}
# There are no duplicated band numbers. Maybe this is not necessary, but I added it
# just in case. 

# We need to find new band numbers in comments column, even when band_number
# column doesn't have asterisk

# Looking for [A-Z]##### or ####-#####
banded.dat <- banded.dat %>%
  # First change comments = NA to ""
  mutate(comments = ifelse(is.na(comments), "", comments)) %>%
  mutate(band_in_comment = 1 * str_detect(comments, "[A-Z][0-9]{5}|[0-9]{4}-[0-9]{5}")) %>%
  data.frame()

# Count the rows of data that have a band number in comments
count(banded.dat, band_in_comment)
# 11841 records don't have a band number in comments
#   122 records have a band numbers in comments

head(filter(banded.dat, band_in_comment == 0)) # no comments
head(filter(banded.dat, band_in_comment == 1)) # some comments are notes about 
# other birds' bands (eg, "captured with C53670", "probably related to C53759")

count(filter(banded.dat, band_in_comment == 1), comments)
# and sometimes it looks like the former band number is listed here. Also foreign
# captures band numbers are listed here.

count(banded.dat, band_asterisk, band_in_comment)
# Some that don't have asterisk in band_number do have comments about bands
# But ALL (8) with asterisks have comments about bands (which is good)

# Separate those bands that have "former" from those that have "foreign" and 
# "removed"
banded.dat <- banded.dat %>%
  mutate(foreign = 1 * str_detect(toupper(comments), "FOREIGN"),
         former = 1 * str_detect(toupper(comments), "FORMER"),
         removed = 1 * str_detect(toupper(comments), 'REMOVED'))

# Create data frame with comment combos
comment_combos_banded <- banded.dat %>% 
  group_by(band_in_comment, former, foreign, removed) %>%
  summarize(n = length(former)) %>%
  data.frame()

# Looking at each of these indicator variable columns 
comment_combos_banded

filter(banded.dat, band_in_comment == 0, former == 0, foreign == 0, removed == 1) 
# n = 3:
# All comments are about removing spines or pine sap. Keep all records

filter(banded.dat, band_in_comment == 0, former == 0, foreign == 1, removed == 0) 
# n = 4: 
# 4000-88228 AZ band just captured once. Not relevant. Record in recaptured data as well
# 4000-01786 just captured once. Not relevant. Record in recaptured data as well
# 5000-02820 recaptured twice 2005 and 2007. Relevant? We don't have first capture. Record in recapture data as well
# 4100-08433 just captured once. Not relevant. Record not in the recaptured data
# Remove all records

filter(banded.dat, band_in_comment == 0, former == 1, foreign == 0, removed == 0) 
# n = 1
# Letter N code is 5000.
# 5000-29395 is correct, but I'm deleting it to avoid confusion with comment. 
# Remove record with 5000-29253 in band number. Band is incorrect, corresponds 
# to a RUHU. 

banded.dat %>% filter(band_in_comment == 1, former == 0, foreign == 0, removed == 0) %>% 
  select(band_number, comments)
# n = 53
# All bands in comments are irrelevant. They are 'probably related to', 
# 'captured with', 'similar to,' 'flew away with'. Keep all records

filter(banded.dat, band_in_comment == 1, former == 0, foreign == 1, removed == 0) 
# n = 3
# Both records 5000-22231 (band_number) and 9000-11887 (in comment) are irrelevant. 
# It is a foreign bird, captured just once. Record is also in recapture data.
# 9000-39100 new band applied to a foreign bird (band replaced) captured in 2007 and recaptured in 2008
# 9000-39209 new band applied to a foreign bird (band replaced) captured in 2007 and recaptured in 2008
# Remove all records

banded.dat %>% filter(band_in_comment == 1, former == 1, foreign == 0, removed == 0) %>%
  select(band_number, comments)
# n = 56
# All records with the word former in comments have the new band in the column
# 'band_number'. For most of them, the record with the word former is actually 
# a recapture, so we need to replace the original band number (first applied) 
# with the one in the comments. 
# Keep all records

filter(banded.dat, band_in_comment == 1, former == 1, foreign == 0, removed == 1)
# n = 4
# 5000-96919 band removed. Recaptured once. Didn't find former band in banded or 
# recaptured data, maybe it was a foreign capture. Remove this record
# 5000-96944 band removed. Not recaptured. Didn't find former band in banded or 
# recaptured data, maybe another foreign band? Remove this record.
# 9000-39804 band removed. Not recaptured. Found former band 6000-23685 in banded 
# data. Keep record, but make sure to replace former band number 
# 9000-90668 band replaced, not removed. Keep this record but make sure to replace
# former band number 

filter(banded.dat, band_in_comment == 1, former == 1, foreign == 1, removed == 0)
# n = 5
# I think we should remove all of foreign bands as they are not so many, most have 
# just one capture, and we don't have the information for the first capture. 
# Bands are: 9000-90490, 4100-59350, 4100-59755, 9100-22998, 9100-57147
# Remove all records

filter(banded.dat, band_in_comment == 1, former == 1, foreign == 1, removed == 1)
# n = 1
# 5000-96877 remove this record

# For now, will remove any questionable entries

# Start with all the bands with the word foreign in comments. Removes 13 rows
banded.dat <- banded.dat %>%
  filter(!grepl('foreign', comments)) %>%
  filter(!grepl('Foreign', comments)) %>%
  filter(!grepl('FOREIGN', comments))
# Band numbers removed: 4000-88228, 5000-96877, 4000-01786, 5000-02820, 5000-22231,
# 9000-39100, 9000-39209, 9000-90490, 4100-08433, 4100-59350, 4100-59755, 
# 9100-22998, 9100-57147

# Remove one foreign record that doesn't have foreign in comments but it is a 
# foreign band
banded.dat <- banded.dat %>% 
  filter(band_number != '9000-11887')

# Remove one band that has no band number in comments but has the word former in comments. 
# This band is not in the recaptured data. Also it is a record for RUHU, which doesn't affect our data.
# Second record deleted to avoid confusion with comment. Also deleted from recaptured data
banded.dat <- banded.dat %>% 
  filter(band_number != '5000-29253',
         band_number != '5000-29395') 

# Remove two bands that were applied to a foreign capture but don't have the word
# foreign in comments in this data set, but is foreign in recaptured data set.
# I deleted them form recaptured data. 
banded.dat <- banded.dat %>% 
  filter(band_number != '3100-41468',
         band_number != '4100-08428')

# Remove two records that have 'removed band' and 'former' in comments, but former
# band was not in data set.
banded.dat <- banded.dat %>% 
  filter(band_number != '5000-96919',
         band_number != '5000-96944')

# Lines 232 to 266 remove 20 records from data. 

# In this data set the band_number column has OLD records when a row has the 
# word 'former' in comments. The records with the OLD band number does not have
# a band in comments or any key word... example 53578

# Also notice that in tow years (2008 and 2010) the data has two extra columns
# replaced_band_number (2008) and old_band_number (2010). The records with a 
# band number in these columns not always have a band in  comments or a key 
# word. Not sure how to work with this information. 

# If the following condition is met, the band_number column has a NEW 
# band number and the band number in comments is irrelevant:

# band_in_comment = 1, former = 0, foreign = 0, removed = 0, correct
# related to, flew with, similar to, etc

# If the following condition is met, the band_number column has a NEW 
# band number and the comments column has an OLD band number:

# band_in_comment = 1, former = 1, foreign = 0, removed = 0, correct

banded.dat <- banded.dat %>%
  mutate(band_is_new_in_band_number = ifelse ((
    band_in_comment == 1 & former == 1 & foreign == 0 & removed == 0), 1, 0))

# Check:
count(banded.dat, band_in_comment, former, foreign, removed,
      band_is_new_in_band_number)

###### Stopped here..... 

# Create band_old and band_new column
dat <- dat %>%
  mutate(comment_band = str_extract(comments, "[A-Z][0-9]{5}|[0-9]{4}-[0-9]{5}"),
         band_no_asterisk = ifelse(str_sub(band_number, -1, -1) == "*",
                                   str_sub(band_number, 1, nchar(band_number) - 1),
                                   band_number),
         band_old = ifelse(band_number_is_new == 1, comment_band,
                           ifelse(band_number_is_old, band_no_asterisk, NA)),
         band_new = ifelse(band_number_is_old == 1, comment_band,
                           ifelse(band_number_is_new, band_no_asterisk, NA)))
# Check: 
# head(filter(dat, !is.na(band_old)))

# Now, create band_orig column and keep the band_new column, which will 
# have a new band number only if the bird was rebanded (NA otherwise).
# Will keep band_number column just in case we need the original
dat <- dat %>%
  mutate(band_orig = ifelse(!is.na(band_old), band_old, band_no_asterisk)) %>%
  select(-c(band_asterisk, band_in_comment, new, rebanded, former, 
            band_number_is_old, band_number_is_new, comment_band, 
            band_no_asterisk, band_old))






####################

# Summarize data for analysis 
dat <- banded.dat %>% 
  select(band_number, date, site, species, sex, age, year, day, month) %>%
  mutate(band_status = 1) %>% # 1 for all new bands applied
  filter(species == 'BTLH')

# Check for duplicated band numbers
if(any(duplicated(banded.dat$band_number))){ # Are there duplicate band numbers?
  duplicates <- which(duplicated(banded.dat$band_number))  # Which one is duplicated? 
  message(paste0("The following rows are duplicate band numbers: ",
                 paste0(duplicates, collapse = ",")))
}

# 


