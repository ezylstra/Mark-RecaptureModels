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
# Decided to keep these records

filter(recaptured.dat, band_in_comment == 0, new == 1) 
# n = 1: Not relevant comment, it is about p9. Keep this record

filter(recaptured.dat, band_in_comment == 1, new == 0, rebanded == 0, former == 0, foreign == 0) 
# n = 10 total
# n = 7: these are irrelevant ("captured with" or "captured w/"). Keep them
# n = 2: "possibly related to E12085". Keep them 
# n = 1: "prob. C23826" band removed. Remove this record 6000-23826

filter(recaptured.dat, band_in_comment == 1, new == 0, rebanded == 0, former == 1, foreign == 0) 
# n = 7: All former band numbers in comments (so band_number column is new band). 
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
# 4000-47796 is a former band, new band is 9000-90377 in comments. Keep this record
# 9000-11917 is a former band, new band is 9000-91152 in comments. Keep this record
# 3100-42864 is a former band, new band is 4100-08489 in comments. Keep this record  

# For now, will remove any questionable entries

# Start with all the bands with the word foreign in comments. Removes 12 rows
recaptured.dat <- recaptured.dat %>%
  filter(!grepl('foreign', comments)) %>%
  filter(!grepl('Foreign', comments)) %>%
  filter(!grepl('FOREIGN', comments))
# Band numbers removed: 5000-02840, 5000-22231, 9000-11887, 5000-02820, 5000-02973,
# 6000-55560, 6000-82856, 9000-90490, 3100-17085, 4100-08428, 4100-59350, 3100-41468
  
# Remove one row for a record that is questionable, but doesn't delete the band 
# number in the comment
recaptured.dat <- recaptured.dat %>% 
  filter(!grepl("prob. C23826", comments))

# Remove band numbers that have the word 'foreign' in column Recapture_1 not in
# comments
recaptured.dat <- recaptured.dat %>% 
  filter(band_number != '4000-88228', # This record also removed form banded data
         band_number != '9000-93692', # This record is not in banded data
         band_number != '9000-93695', # This record is not in banded data
         band_number != '9000-93749', # This record is not in banded data
         band_number != '9000-93765', # This record is not in banded data
         band_number != '9100-93534') # This record is not in banded data

# Remove these records because I removed them from banded data
recaptured.dat <- recaptured.dat %>% 
  filter(band_number != '5000-96919',
         band_number != '5000-29395',
         band_number != '4000-01786',
         band_number != '5000-02820',
         band_number != '9000-39100',
         band_number != '9000-39209',
         band_number != '5000-96877')

# Lines 232 to 265 remove 25 band numbers and 28 rows from data.

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

# If the following conditions are met, the band_number column has an OLD
# band number and the comments column has a NEW band number:

# band_in_comment = 1, new = 1, rebanded = 0, former = 0, foreign = 0, correct
# band_in_comment = 1, new = 1, rebanded = 1, former = 0, foreign = 0, correct
# band_in_comment = 1, new = 0, rebanded = 1, former = 0, foreign = 0, correct 

# If the following conditions are met, the band_number column has a NEW 
# band number and the comments column has an OLD band number:

# band_in_comment = 1, new = 1, rebanded = 0, former = 1, foreign = 0, correct
# band_in_comment = 1, new = 0, rebanded = 0, former = 1, foreign = 0, correct

# Create new columns that indicate if a band in column 'band_number' is old or new
recaptured.dat <- recaptured.dat %>%
  mutate(band_is_old_in_band_number = ifelse (
    (band_in_comment == 1 & new == 1 & rebanded == 0 & former == 0) |
      (band_in_comment == 1 & new == 1 & rebanded == 1 & former == 0) |
      (band_in_comment == 1 & new == 0 & rebanded == 1 & former == 0), 1, 0)) %>%
  mutate(band_is_new_in_band_number = ifelse (
    (band_in_comment == 1 & new == 1 & rebanded == 0 & former == 1) |
      (band_in_comment == 1 & new == 0 & rebanded == 0 & former == 1), 1, 0))

# Check:
count(recaptured.dat, band_in_comment, new, rebanded, former, foreign, removed,
       band_is_old_in_band_number, band_is_new_in_band_number)

# Create new columns to:
# 1) Extract band number from comments
# 2) Remove * from band_number  
# 3) Extract old band numbers 
# 4) Extract new band numbers 
recaptured.dat <- recaptured.dat %>%
  mutate(band_in_comment = str_extract(comments, "[A-Z][0-9]{5}|[0-9]{4}-[0-9]{5}"),
         band_number_withouth_asterisk = ifelse(str_sub(band_number, -1, -1) == "*",
                                   str_sub(band_number, 1, nchar(band_number) - 1),
                                   band_number),
         old_band_number = ifelse(band_is_new_in_band_number == 1, band_in_comment,
                           ifelse(band_is_old_in_band_number, band_number_withouth_asterisk, NA)),
         new_band_number = ifelse(band_is_old_in_band_number == 1, band_in_comment,
                           ifelse(band_is_new_in_band_number, band_number_withouth_asterisk, NA)))

# Check: 
head(filter(recaptured.dat, !is.na(old_band_number)))

# Now, create original_band column and keep the new_band_number column, which will 
# have a new band number only if the bird was rebanded (NA otherwise).
# Will keep band_number column just in case we need the original
# Remove indicator variable columns 
recaptured.dat <- recaptured.dat %>%
  mutate(original_band = ifelse(!is.na(old_band_number), old_band_number, band_number_withouth_asterisk)) %>%
  select(-c(band_asterisk, new, rebanded, former, foreign, band_is_old_in_band_number, 
            band_is_new_in_band_number, band_in_comment, band_number_withouth_asterisk,
            old_band_number, removed)) 

# Replace any letters in new_band_number and original_band

# Bring in BBL letter codes 
letter.codes <- read.csv("data/BBL_letter_codes.csv")

# Extract bands with a letter in columns new_band_number and original_band
recaptured.dat <- recaptured.dat %>% 
  mutate(new_band_with_letter = str_extract(new_band_number, "[A-Z][0-9]{5}"),
         original_band_with_letter = str_extract(original_band, "[A-Z][0-9]{5}"))

# First work with new_band_with_letter column

# Separate letter from numbers 
recaptured.dat$band_letter <-substr(recaptured.dat$new_band_with_letter,
                               start = 1, 
                               stop = 1)

recaptured.dat$band_number_2 <- substr(recaptured.dat$new_band_with_letter,
                                start = 2,
                                stop = 6)

# Replace the letter in the band numbers with the codes from BBL, then combine
# the BBL code with the band number without the letter, and delete unnecessary
# columns
recaptured.dat <- recaptured.dat %>%
  left_join(letter.codes, by = c("band_letter" = "letter")) %>% 
  unite('new_new_band_number', c('letter_number','band_number_2'), sep = "-", remove = F, na.rm = T) %>% 
  select(-c(band_letter, band_number_2, letter_number, new_band_with_letter))

# Second work with original_band_number_with_letter column 

# Separate letter from numbers 
recaptured.dat$band_letter <-substr(recaptured.dat$original_band_with_letter,
                                    start = 1, 
                                    stop = 1)

recaptured.dat$band_number_2 <- substr(recaptured.dat$original_band_with_letter,
                                       start = 2,
                                       stop = 6)

# Replace the letter in the band numbers with the codes from BBL, then combine
# the BBL code with the band number without the letter, and delete unnecessary
# columns 
recaptured.dat <- recaptured.dat %>%
  left_join(letter.codes, by = c("band_letter" = "letter")) %>% 
  unite('new_original_band', c('letter_number','band_number_2'), sep = "-", remove = F, na.rm = T) %>% 
  select(-c(band_letter, band_number_2, letter_number, original_band_with_letter))

# Move those band numbers without a letter from new_band_number
# and original_band to the new columns new_new_band_number and new_original_band,
# remove unnecessary columns and rename the new columns created
recaptured.dat <- recaptured.dat %>% 
  mutate(new = ifelse(new_new_band_number == '', new_band_number, new_new_band_number),
         original = ifelse(new_original_band == '', original_band, new_original_band)) %>% 
  select(-c(new_band_number, new_new_band_number, original_band, new_original_band)) %>% 
  rename(original_band = original,
         new_band_number = new)

# Create a new column I'll use as the 'unique bird identifier'
recaptured.dat <- recaptured.dat %>% 
  mutate(UBI_band = ifelse(is.na(new_band_number), original_band, new_band_number))

                      
# Data set now has four columns with band numbers:

# 1) band_number: is the original entry from Fred, has * and a mix of new and old bands
# 2) new_band_number: is a new band number only if the bird was rebanded, NA otherwise
# 3) original_band: is the original or first band applied to an individual
# 4) UBI_band: is the fixed band number when a band was replaced. Use this column for 
# analysis

# Add a new column to indicate the 'band status' of all records
recaptured.dat <- recaptured.dat %>% 
  mutate(band_status = 'R') # R indicates all the band numbers in this data set
                            # are recaptures

# Fix misidentified sex 

# When the bird's sex was misidentified at first capture, the correct sex was
# added as part of the comments in the columns recapture_n. We need to identify 
# the words 'FEMALE' and 'MALE' and letters 'F' or 'M' from these columns, so we 
# can fix the sex of those misidentified birds in both data sets 'banded data' 
# and 'recaptured data'

# I checked columns recapture_3:17 using unique(recaptured.dat$column) and there 
# were no FEMALE or MALE words or F and M letters that indicated a different sex
# that the one marked in column original_sex. Most of this information should be 
# in column recapture_1 or 2

# Check column recapture_2 

# Create a variable to indicate when the updated sex was identified in recapture, 
# column 'recapture_2' 
recaptured.dat <- recaptured.dat %>%
  mutate(female_2 = 1 * str_detect(toupper(recapture_2), "FEMALE"),
         male_2 = 1 * str_detect(toupper(recapture_2), "MALE"), 
         f_2 = 1 * str_detect(toupper(recapture_2), "F"),
         m_2 = 1 * str_detect(toupper(recapture_2), "M"))

# Create data frame with recapture_2 sex combos
sex_combos_recaptured_2 <- recaptured.dat %>% 
  group_by(female_2, male_2, f_2, m_2) %>%
  summarize(n = length(female_2)) %>% 
  data.frame()

# Looking at each of these indicator variable columns
sex_combos_recaptured_2

filter(recaptured.dat, m_2 == 1) %>% 
  select(band_number, original_sex, recapture_2)
# n = 38
# One record needs to be changed: 6000-53867 from F to M
# The other 37 records are in this combo because the letter M in in a site code 
# or M represents moderate in MTW (moderate tail wear) 

# Create new column to fix the sex in this record and remove columns with indicator 
# variables for sex combos for column recapture_2
recaptured.dat <- recaptured.dat %>% 
  mutate(new_sex = ifelse (
    (str_detect(recapture_2, 'AHY M')), 'M', NA)) %>% 
  select(-c(female_2, male_2, f_2, m_2))
  
# Check column recapture_1 

# Create a variable to indicate when the updated sex was identified in recapture, 
# column 'recapture_1' 
recaptured.dat <- recaptured.dat %>%
  mutate(female_1 = 1 * str_detect(toupper(recapture_1), "FEMALE"),
         male_1 = 1 * str_detect(toupper(recapture_1), "MALE"), 
         f_1 = 1 * str_detect(toupper(recapture_1), "F"),
         m_1 = 1 * str_detect(toupper(recapture_1), "M"))

# Create data frame with recapture_1 sex combos
sex_combos_recaptured_1 <- recaptured.dat %>% 
  group_by(female_1, male_1, f_1, m_1) %>%
  summarize(n = length(female_1)) %>% 
  data.frame()

# Looking at each of these indicator variable columns
sex_combos_recaptured_1

filter(recaptured.dat, female_1 == 0, male_1 == 0, f_1 == 0, m_1 == 1) %>% 
  select(band_number, original_sex, recapture_1)
# n = 206
# All records are in this combo because the letter M is in a site code,
# M represents moderate in MTW (moderate tail wear), m represent millimeter
# One record could be confusing, 5000-96986. Comment says 
# '8/14, n.w. mtw w/E91096 HY M'.
# I think it means: female 5000-96986 was captured with hatch year male E91096 
# No need to change anything 

filter(recaptured.dat, female_1 == 0, male_1 == 0, f_1 == 1, m_1 == 0) %>% 
  select(band_number, original_sex, recapture_1)
# n = 14
# Three records need to be changed: 6000-53878, 6000-53918, 6000-53949 from M to F
# These three records are for AHY F
# Other 11 records refer to feathers, flight, fat, and other codes

filter(recaptured.dat, female_1 == 0, male_1 == 1, f_1 == 0, m_1 == 1) %>% 
  select(band_number, original_sex, recapture_1)
# n = 7
# All seven records need to be changed from F to M
# Bands are: 9000-12279, 9000-12283, 9000-12490, 9000-12279, 9000-91371, 9000-40183

filter(recaptured.dat, female_1 == 1, male_1 == 1, f_1 == 1, m_1 == 1) %>% 
  select(band_number, original_sex, recapture_1)
# n = 9
# All nine records need to be changed from M to F
# Bands are: 6000-53751, 6000-53749, 6000-53918, 9000-90030, 9000-90683, 9000-91173

# If the following conditions are met, the sex in column 'original_sex' needs to 
# change from M to F
# female_1 == 1, male_1 == 1, f_1 == 1, m_1 == 1
# female_1 == 0, male_1 == 0, f_1 == 1, m_1 == 0 and 'AHY F' in column recapture_1

# If the following condition is met, the sex in column 'original_sex' needs to 
# change from F to M
# female_1 == 0, male_1 == 1, f_1 == 0, m_1 == 1

# Create new columns that hold the fixed sex code
recaptured.dat <- recaptured.dat %>%
  mutate(new_sex_F = ifelse (
    (female_1 == 1 & male_1 == 1 & f_1 == 1 & m_1 == 1) |
      (str_detect(recapture_1, 'AHY F')) |
      str_detect(recapture_1, 'AHY  F'), 'F', NA)) %>% 
  mutate(new_sex_M = ifelse(
    (female_1 == 0 & male_1 == 1 & f_1 == 0 & m_1 == 1), 'M', NA))
    
# Check:
filter(recaptured.dat, female_1 == 1, male_1 == 1, f_1 == 1, m_1 == 1) %>% 
         select(band_number, original_sex, recapture_1, new_sex_F)

filter(recaptured.dat, female_1 == 0, male_1 == 0, f_1 == 1, m_1 == 0) %>% 
  select(band_number, original_sex, recapture_1, new_sex_F)

filter(recaptured.dat, female_1 == 0, male_1 == 1, f_1 == 0, m_1 == 1) %>% 
  select(band_number, original_sex, recapture_1, new_sex_M)

# Remove columns with indicator variables for sex combos for column recapture_1
# and merge three new columns with fixed sex code 
recaptured.dat <- recaptured.dat %>% 
  select(-c(female_1, male_1, f_1, m_1)) %>% 
  unite(fixed_sex, new_sex:new_sex_M, sep = "", remove = T, na.rm = T)

# Fill in the empty rows in column 'fixed_sex' with original sex
recaptured.dat <- recaptured.dat %>% 
  mutate(fixed_sex = ifelse(str_detect(fixed_sex, '[A-Z]{1}'), fixed_sex, original_sex))

# Prepare new data frame to merge with banded data

# Create a new data frame with selected columns we'll need for analysis
recaptured <- recaptured.dat %>% 
  select(UBI_band, band_status, recapture_year, species, fixed_sex, original_site) %>% 
  filter(species == 'BTLH') %>% 
  rename(year = recapture_year,
         site = original_site) %>% 
  select(-species)

# Export csv of new data frame
write.csv(recaptured, 'output/cleaned-recaptured-data-RMNP.csv') 

# Prepare new data frame to explore recaptures at multiple sites

# Create new data frame 
recap <- recaptured.dat %>% 
     filter(species == 'BTLH') %>%   
     select(UBI_band, band_status, recapture_year, fixed_sex, original_site, 
            paste0('recapture_', 1:17), comments) %>%
     rename(band = UBI_band, 
            recap_yr = recapture_year,
            site_orig = original_site,
            sex = fixed_sex)
colnames(recap)[6:22] <- paste0('r', 1:17)
   
# Export csv of new data frame
write.csv(recap, 'output/cleaned-recaptured-data-RMNP-full.csv',
          row.names = FALSE) 
