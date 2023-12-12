# Clean RMNP recaptured data
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
recaptured.files <- list.files(path = "data/RMNP-recaptured-by-year/", 
                           pattern = "*.csv", 
                           full.names = TRUE)
recaptured.dat <- lapply(recaptured.files, fread, sep = ",")  

# Combine all csv files in a data frame
recaptured.dat <- rbindlist(recaptured.dat, fill = TRUE)

# Replace space in column names with '_'
recaptured.dat <- recaptured.dat %>% 
  clean_names()

# ----------------- EXPLORE AND FIX COLUMNS NEEDED FOR THESIS ---------------- # 

# COLUMN ORIGINAL DATE
# Change column date from character to class date, delete and rename new date column
class(recaptured.dat$original_date)
recaptured.dat <- recaptured.dat %>% 
  mutate(date = mdy(original_date)) %>% 
  select(-original_date) %>% 
  rename(original_date = date)

# COLUMN ORIGINAL SITE
count(recaptured.dat, original_site)

# Capitalize characters in column site 
recaptured.dat$original_site <- toupper(recaptured.dat$original_site)

# Replace COWCR3 with CC3
recaptured.dat$original_site[recaptured.dat$original_site == 'COWCR3'] <- 'CC3' 
#   CC2   33
#   CC3   70
#   GC1    8
# GNMTN  356
#  HOLZ  286
#   HPE    1
#  HPK1  159
#  HPK2   48
#   KV1  329
#  MCGC 2200
#   MP1 1075
#  NCOS    1
#  NFPC    1
#  POLC    3
#  SHIP    3
#   WC1   16

# COLUMN ORIGINAL SEX
count(recaptured.dat, original_sex)
#  A  1 Checked band number 9000-40058 in banded data to fix this record. This is a female 
#  f  1 

# Capitalize characters in column original_sex 
recaptured.dat$original_sex <- toupper(recaptured.dat$original_sex)

# Fix record with sex A
recaptured.dat$original_sex[recaptured.dat$original_sex == 'A'] <- 'F'
#  F  3737
#  M  849

# COLUMN SPECIES
count(recaptured.dat, species)
#  BTLH 4451
#  CAHU   19
#  RUHU  116

# COLUMN ORIGINAL AGE
count(recaptured.dat, original_age) 
# A  1 Checked band number 3100-41974 in banded data to fix this record. This is a AHY 
# SY 3

# Change SY (second year) in age to AHY (after hatch-year)
recaptured.dat$original_age[recaptured.dat$original_age == 'SY'] <- 'AHY'

# Fix record with age A
recaptured.dat$original_age[recaptured.dat$original_age == 'A'] <- 'AHY'
# AHY  4150
#  HY   436

# COLUMN RECAPTURE YEAR
count(recaptured.dat, recapture_year) 
# 2003 131
# 2004 199
# 2005 366
# 2006 479
# 2007 573
# 2008 623
# 2009 782
# 2010 764
# 2011 669

# RECAPTURE 1 TO 17 COLUMNS.... LATER

# COLUMN BAND NUMBER
# From Erin's code

# Look at format of entries in band_number column:
bn <- sort(unique(recaptured.dat$band_number)) # Length = 3279
table(nchar(bn)) # Count the number of characters in band numbers
# There are 3239 band numbers with 10 characters
# There are 40 band numbers with 11 characters

sum(str_detect(bn, "[0-9]{4}-[0-9]{5}") * nchar(bn) == 10)
# All 3239 band numbers with 10 characters are correct format (4 numbers - 5 numbers)

sum(str_detect(bn, "[0-9]{4}-[0-9]{5}\\*") * nchar(bn) == 11)
# All 40 band numbers with 11 characters are correct format with * at end.

# Create a variable to indicate when an asterisk appears at end of band number
recaptured.dat <- recaptured.dat %>%
  mutate(band_asterisk = 1 * str_detect(band_number, "[0-9]{4}-[0-9]{5}\\*"))

# Need to find new band numbers in comments column, even when band_number
# column doesn't have asterisk
# Looking for [A-Z]##### or ####-#####
recaptured.dat <- recaptured.dat %>%
  # First change comments = NA to ""
  mutate(comments = ifelse(is.na(comments), "", comments)) %>%
  mutate(band_in_comment = 1 * str_detect(comments, "[A-Z][0-9]{5}|[0-9]{4}-[0-9]{5}")) %>%
  data.frame()

# Count the rows of data that have a band number in comments
count(recaptured.dat, band_in_comment)
# 4471 records don't have a band number in comments
#  115 records have a band numbers in comments

head(filter(recaptured.dat, band_in_comment == 0)) # no comments
head(filter(recaptured.dat, band_in_comment == 1)) # note that sometimes there
# are notes about other birds' bands (eg, "captured with E12354")

count(filter(recaptured.dat, band_in_comment == 1), comments)
# and sometimes it looks like the former band number is listed here.

count(recaptured.dat, band_asterisk, band_in_comment)
# Some that don't have asterisk in band_number do have comments about bands
# But ALL with asterisks have comments about bands (which is good)

# Looks like we can separate those that have "new"/"rebanded" from those that 
# have "former" and "foreign"
recaptured.dat <- recaptured.dat %>%
  mutate(new = 1 * str_detect(toupper(comments), "NEW"),
         rebanded = 1 * str_detect(toupper(comments), "REBANDED"), 
         former = 1 * str_detect(toupper(comments), "FORMER"),
         foreign = 1 * str_detect(toupper(comments), "FOREIGN"))

# Create data frame with comment combos
comment_combos_recaptured <- recaptured.dat %>% 
  group_by(band_in_comment, new, rebanded, former, foreign) %>%
  summarize(n = length(new)) %>% # It could be any column (new,former,etc). It doesn't change the outcome
  data.frame()

# Looking at each of these indicator variable columns
comment_combos_recaptured

filter(recaptured.dat, band_in_comment == 0, foreign == 1)
# n = 5
# 5000-02840: not relevant foreign recapture, not in banded data
# 9000-11887: not relevant foreign recapture, I deleted it from banded data
# 5000-02820: not relevant foreign recapture, I deleted it from banded data
# 5000-02973: not relevant foreign recapture, not in banded data
# 3100-17085: not relevant foreign recapture, not in banded data
# Remove all

filter(recaptured.dat, band_in_comment == 0, rebanded == 1) 
# n = 7: all bands removed, but not rebanded
# Bands: 6000-53987, 9000-12270, 9000-90137, 9000-90149, 9000-12271, 9000-91173, 
# 3100-42324
# Decided to keep these records.

filter(recaptured.dat, band_in_comment == 0, new == 1) 
# n = 1: Not relevant comment, it is about p9. Keep this record

filter(recaptured.dat, band_in_comment == 1, new == 0, rebanded == 0, former == 0, foreign == 0) 
# n = 10 total
# n = 7: these are irrelevant ("captured with" or "captured w/"). Keep them
# n = 2: "possibly related to E12085". Keep them 
# n = 1: "prob. C23826" band removed. Remove this record 6000-23826

filter(recaptured.dat, band_in_comment == 1, new == 0, rebanded == 0, former == 1, foreign == 0) 
# n = 7: All former band numbers (so band_number column is new band). 
# Keep these records, but make sure to change the original band in banded data 
# set (former in comments) to the new band.

filter(recaptured.dat, band_in_comment == 1, new == 0, rebanded == 0, former == 1, foreign == 1)
# n = 4
# 9000-90490: not relevant foreign recapture, I deleted it from banded data. Remove
# 3100-41468: not relevant foreign recapture, I deleted it from banded data. Remove
# 4100-08428: not relevant foreign recapture. Not in banded data set. Remove
# 4100-59350: not relevant foreign recapture, I deleted it from banded data. Remove

filter(recaptured.dat, band_in_comment == 1, new == 0, rebanded == 1, former == 0, foreign == 0) 
# n = 1: rebanded as E39458 (so same info as those with "NEW")

recaptured.dat %>% filter(band_in_comment == 1, new == 1, rebanded == 0, former == 0, foreign == 0) %>%
  select(band_number, original_date, recapture_year, recapture_1, comments)
# n = 83: always has new band number in comments and band number not always has *

filter(recaptured.dat, band_in_comment == 1, new == 1, rebanded == 0, former == 0, foreign == 1) 
# n = 3
# 5000-22231: not relevant foreign recapture, I deleted it from banded data. Remove
# 6000-55560: not relevant foreign recapture, I deleted the new band from banded data. Remove
# 6000-82856: not relevant foreign recapture, I deleted the new band from banded data. Remove

filter(recaptured.dat, band_in_comment == 1, new == 1, rebanded == 0, former == 1, foreign == 0) 
# n = 4: New band in band_number column; band in comments is former.
# I think former band in comments N11763 is wrong and it should be N11768
# Keep these records

filter(recaptured.dat, band_in_comment == 1, new == 1, rebanded == 1, former == 0, foreign == 0) 
# n = 3: Need to check two of these that have a new band number but also say
# that band was removed. Letter E code is 9000
# 4000-47796 is a former band, new band is 9000-90377. Keep this record
# 9000-11917 is a former band, new band is 9000-91152. Keep this record
# 3100-42864 is a former band, new band is 4100-08489. Keep this record  

# For now, will remove any questionable entries

# Start with all the bands with the word foreign in comments. Removes 12 rows
recaptured.dat <- recaptured.dat %>%
  filter(!grepl('foreign', comments)) %>%
  filter(!grepl('Foreign', comments)) %>%
  filter(!grepl('FOREIGN', comments))
# Band numbers removed: 5000-02840, 5000-22231, 9000-11887, 5000-02820, 5000-02973,
# 6000-55560, 6000-82856, 9000-90490, 3100-17085, 4100-08428, 4100-59350, 3100-41468
  
# Remove one row for a band number that is questionable
recaptured.dat <- recaptured.dat %>% 
  filter(!grepl("prob. C23826", comments))

# Remove band numbers that have the word 'foreign' in column Recapture_1 not in
# comments
recaptured.dat <- recaptured.dat %>% 
  filter(band_number != '4000-88228', # This record also removed form banded data
         band_number != '9000-93692', # This record is not in banded data
         band_number != '9000-93695', # This record is not in banded data
         band_number != '9000-93749', # This record is not in banded data
         band_number != '9000-93765') # This record is not in banded data

# Remove this record because I removed it from banded data
recaptured.dat <- recaptured.dat %>% 
  filter(band_number != '5000-96919')

# Lines 234 to 257 remove 19 records from data.

# Erin's code
# dat <- dat %>% 
  # filter(!grepl("possibly related to E12085", comments)) %>% 
  # Removes 2 rows. I looked into these two records and they are fine. The comment
  # 'possible related to E12085' is irrelevant and doesn't affect the records.
  # filter(!grepl("prob. C23826", comments)) %>% 
  # Removes one row, it is right. I agree with the deletion. 
  # filter(!(grepl("band removed", comments) & 
             # band_in_comment == 1 & new == 1 & rebanded == 1 & former == 0)) 
  # Removes 2 rows. Their comments are confusing I agree, but I looked into them
  # and they have good information. They were rebanded at some point and then 
  # the band was removed. Both records have good recapture data (5 and 4 years).
  # So I'll treat them as bands removed but not replaced. Bands are 
  # 4000-47796 and 9000-11917
 
# Adding a column indicating when a band was removed but not replaced (will
# ultimately put NAs in capture histories for all years after band removed)

# Comments say 'band removed', 'band removed, not rebanded.' There are three
# bands in this group that also have the word 'new' in the comments they are:
# 4000-47796, 9000-11917, and 9000-39250. The comment 'New' is old from a previous
# year band replacement. 
recaptured.dat <- recaptured.dat %>%
  mutate(removed = ifelse(str_detect(toupper(comments), "BAND REMOVED"), 1, 0))

# Stopped here.................

# If the following conditions are met, the band_number column has an OLD
# band number and the comments column has a NEW band number:

# band_in_comment = 1, new = 1, rebanded = 0, former = 0, foreign = 0, 
# band_in_comment = 1, new = 1, rebanded = 1, former = 0
# band_in_comment = 1, new = 0, rebanded = 1, former = 0

# If the following conditions are met, the band_number column has a NEW 
# band number and the comments column has an OLD band number:

# band_in_comment = 1, new = 1, rebanded = 0, former = 1
# band_in_comment = 1, new = 0, rebanded = 0, former = 1

# Create new columns that indicate if a band is old or new
dat <- dat %>%
  mutate(band_number_is_old = ifelse (
    (band_in_comment == 1 & new == 1 & rebanded == 0 & former == 0) |
      (band_in_comment == 1 & new == 1 & rebanded == 1 & former == 0) |
      (band_in_comment == 1 & new == 0 & rebanded == 1 & former == 0), 1, 0)) %>%
  mutate(band_number_is_new = ifelse (
    (band_in_comment == 1 & new == 1 & rebanded == 0 & former == 1) |
      (band_in_comment == 1 & new == 0 & rebanded == 0 & former == 1), 1, 0))

# Check:
# count(dat, band_in_comment, new, rebanded, former, 
#       band_number_is_new, band_number_is_old)

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

# NEXT STEPS:
# CREATE FULL BAND NUMBERS (replace any letters in band_new and band_orig)
# MATCH UP BAND NUMBERS IN THESE COLUMNS WITH INFO IN THE BANDED DATA FOLDER
# EXTRACT RECAPTURE DATE FROM RECAPTURE COLUMNS (AND CHECK THERE'S NO BAND INFO IN THERE)



