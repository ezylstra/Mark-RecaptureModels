# Clean RMNP 2012 recaptured data
# Gaby Samaniego
# gabysamaniego@arizona.edu
# 2024-09-02

# Load packages
library(tidyverse)
library(data.table)
library(janitor)

# Clear environment
rm(list = ls())

# Read data
dat.12 <- read.csv('data/RMNP-recaptured-by-year/recaptured-2012.csv')

# Replace space in column names with '_'
dat.12 <- dat.12 %>% 
  clean_names()

# --------------- EXPLORE AND FIX COLUMNS IN 2012 RECAPTURE DATA  ------------ # 

# COLUMN RECAPTURE DATE
# Change column date from character to class date, delete and rename new date column
class(dat.12$recapture_date)
dat.12 <- dat.12 %>% 
  mutate(date = mdy(recapture_date)) %>% 
  select(-recapture_date) %>% 
  rename(recapture_date = date) %>% 
  mutate(month = month(recapture_date),
                 day = day(recapture_date)) %>% 
  select(-recapture_date)

# COLUMN ORIGINAL SITE
count(dat.12, recapture_site)

# COLUMN ORIGINAL SEX
count(dat.12, recaptured_sex)

# COLUMN SPECIES
count(dat.12, species)
#  BTLH 1625
#  CAHU    8
#  RUHU   28

# COLUMN ORIGINAL AGE
count(dat.12, recapture_age) 
# AHY  1599
#  HY    62

# COLUMN RECAPTURE YEAR
count(dat.12, recapture_year) 
# 2012 1661

# COLUMN BAND NUMBER

# Look at format of entries in band_number column:
bn <- sort(unique(dat.12$band_number)) # Length = 3279
table(nchar(bn)) # Count the number of characters in band numbers
# There are 847 band numbers with 10 characters

sum(str_detect(bn, "[0-9]{4}-[0-9]{5}") * nchar(bn) == 10)
# All 847 band numbers with 10 characters are correct format (4 numbers - 5 numbers)

# Need to find new band numbers in comments column
# Looking for [A-Z]##### or ####-#####
dat.12 <- dat.12 %>%
  # First change comments = NA to ""
  mutate(comments = ifelse(is.na(comments), "", comments)) %>%
  mutate(band_in_comment = 1 * str_detect(comments, "[A-Z][0-9]{5}|[0-9]{4}-[0-9]{5}")) %>%
  data.frame()

# Count the rows of data that have a band number in comments
count(dat.12, band_in_comment)
# 1654 records don't have a band number in comments
#  7 records have a band numbers in comments

head(filter(dat.12, band_in_comment == 0)) # no comments
head(filter(dat.12, band_in_comment == 1)) 

count(filter(dat.12, band_in_comment == 1), comments)
# and sometimes it looks like the former band number is listed here.

# Looks like we can separate those that have "new"/"rebanded" from those that 
# have "former" and "foreign"
dat.12 <- dat.12 %>%
  mutate(new = 1 * str_detect(toupper(comments), "NEW"),
         rebanded = 1 * str_detect(toupper(comments), "REBANDED"), 
         former = 1 * str_detect(toupper(comments), "FORMER"),
         foreign = 1 * str_detect(toupper(comments), "FOREIGN"),
         removed = 1 * str_detect(toupper(comments), "REMOVED"))

# Create data frame with comment combos
comment_combos_recaptured <- dat.12 %>% 
  group_by(band_in_comment, new, rebanded, former, foreign, removed) %>%
  summarize(n = length(new)) %>% # It could be any column (new,former,etc). It doesn't change the outcome
  data.frame()

# Looking at each of these indicator variable columns
comment_combos_recaptured

filter(dat.12, band_in_comment == 0, removed == 1)
# n = 2
# Keep these bands

filter(dat.12, band_in_comment == 0, foreign == 1)
# n = 9 
# All foreign
# Remove all

filter(dat.12, band_in_comment == 1, new == 0, rebanded == 0, former == 0, foreign == 0)
# n = 4
# All in main database already
# Remove all

filter(dat.12, band_in_comment == 1, new == 0, rebanded == 0, former == 0, foreign == 1) 
# n = 1
# Foreign band
# Remove it

filter(dat.12, band_in_comment == 1, new == 0, rebanded == 1, former == 0, foreign == 0) 
# n = 1
# Record already in main database
# Remove it

filter(dat.12, band_in_comment == 1, new == 1, rebanded == 0, former == 0, foreign == 0) 
# n = 1
# Record already in main database
# Remove it

# Will remove these entries

# Start with all the bands with the word foreign in comments. Removes 12 rows
dat.12 <- dat.12 %>%
  filter(!grepl('foreign', comments)) %>%
  filter(!grepl('Foreign', comments)) %>%
  filter(!grepl('FOREIGN', comments))
# Removes 10 bands = to 10 records

# Remove bands found with code lines 104, 114 and 119
dat.12 <- dat.12 %>% 
  filter(band_number != '3100-41970',
         band_number != '3100-42456',
         band_number != '4100-59259',
         band_number != '4100-59326',
         band_number != '4100-58885',
         band_number != '4100-58937')
# Removes 6 bands = to 23 records

# Lines 132 to 147 remove 16 band numbers and 33 rows from data

# Remove all identification variable columns, change column names and add band_status
# column
dat.12 <- dat.12 %>% 
  select(-c(new, removed, former, foreign, rebanded, band_in_comment)) %>% 
  rename(band = band_number,
         recap_yr = recapture_year) %>% 
  mutate(band_status = 'R') # R indicates all the band numbers in this data set
                            # are recaptures

# Merge day and month xx/xx to 'recapture_1' column to match other
# recapture data sets (2002 to 2011)
dat.12 <- dat.12 %>% 
  unite(mon_day, month:day, sep = '/') %>% 
  unite(recapture_info, c(mon_day, recapture_1), sep = ', ') %>% 
  rename(r1 = recapture_info)

# Prepare new data frame to merge with other recapture data sets
recap_12 <- dat.12 %>%   
  mutate(r02 = NA, r03 = NA,r04 = NA, r05 = NA, r06 = NA, r07 = NA, r08 = NA,
         r09 = NA, r10 = NA, r11 = NA, r12 = NA, r13 = NA, r14 = NA, r15 = NA,
         r16 = NA, r17 = NA,) %>% 
  rename(r01 = r1) %>% 
  select(species,band, band_status, recap_yr, recaptured_sex, recapture_site, 
         r01, r02, r03, r04, r05, r06, r07, r08, r09, r10, r11, r12, r13, r14, r15,
         r16, r17, comments) %>%
  rename(site_orig = recapture_site,
         sex = recaptured_sex) %>% 
  filter(species == 'BTLH') %>% 
  select(-species)

# Export csv of new data frame
write.csv(recap_12, 'output/capture-data/cleaned-recaptured-data-RMNP-2012-full.csv',
          row.names = FALSE) 
