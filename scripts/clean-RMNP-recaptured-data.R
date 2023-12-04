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

# Read all cvs files to merge

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

# Split column date by year, month, and day
recaptured.dat <- mutate(recaptured.dat, year = year(original_date), 
                     month = month(original_date),
                     day = day(original_date))  

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
# have "former"
recaptured.dat <- recaptured.dat %>%
  mutate(new = 1 * str_detect(toupper(comments), "NEW"),
         rebanded = 1 * str_detect(toupper(comments), "REBANDED"), 
         former = 1 * str_detect(toupper(comments), "FORMER"))

# Create data frame with comment combos
comment_combos_recaptured <- recaptured.dat %>% 
  group_by(band_in_comment, new, rebanded, former) %>%
  summarize(n = length(new)) %>% # Why new here? 
  data.frame()

# Looking at each of these indicator variable columns
comment_combos_recaptured

filter(recaptured.dat, band_in_comment == 0, rebanded == 1) 
# n = 7: band removed, but not rebanded

filter(recaptured.dat, band_in_comment == 0, new == 1) 
# n = 1: Not relevant (comment about p9)

filter(recaptured.dat, band_in_comment == 1, new == 0, rebanded == 0, former == 0) 
# n = 7: these are irrelevant ("captured with" or "captured w/")
# n = 2: "possibly related to E12085" 
# n = 1: "prob. C23826" band removed

filter(recaptured.dat, band_in_comment == 1, new == 0, rebanded == 0, former == 1) 
# n = 11: All former band numbers (so band_number column is new band)

filter(recaptured.dat, band_in_comment == 1, new == 0, rebanded == 1, former == 0) 
# n = 1: rebanded as E39458 (so same info as those with "NEW")

recaptured.dat %>% filter(band_in_comment == 1, new == 1, rebanded == 0, former == 0) %>%
  select(band_number, original_date, recapture_year, recapture_1, comments)
# n = 86: always has new band number in comments and band number not always has *

filter(recaptured.dat, band_in_comment == 1, new == 1, rebanded == 0, former == 1) 
# n = 4: New band in band_number column; band in comments is former.

filter(recaptured.dat, band_in_comment == 1, new == 1, rebanded == 1, former == 0) 
# n = 3: Need to check two of these that have a new band number but also say
# that band was removed.
# Letter E code is 9000

# New band E90377 applied on 6/29/2008, then I think removed when recaptured in 2009, former R47796 originally applied on 6/12/2004
# New band E91152 applied on 7/30/2008, then I think removed when recaptured in 2010, former E11917 originally applied on 6/25/2006
# New band in comments for these two records

###### I stooped here........................................................... 


# For now, will remove any questionable entries
dat <- dat %>%
  filter(!grepl("possibly related to E12085", comments)) %>%
  filter(!grepl("prob. C23826", comments)) %>%
  filter(!(grepl("band removed", comments) & 
             band_in_comment == 1 & new == 1 & rebanded == 1 & former == 0))

# Adding a column indicating when a band was removed but not replaced (will
# ultimately put NAs in capture histories for all years after band removed)
dat <- dat %>%
  mutate(removed = ifelse(band_in_comment == 0 & rebanded == 1, 1, 0))

# If the following conditions are met, the band_number column has an OLD
# band number and the comments column has a NEW band number:
# band_in_comment = 1, new = 1, rebanded = 0, former = 0,
# band_in_comment = 1, new = 1, rebanded = 1, former = 0
# band_in_comment = 1, new = 0, rebanded = 1, former = 0
# If the following conditions are met, the band_number column has a NEW 
# band number and the comments column has an OLD band number:
# band_in_comment = 1, new = 1, rebanded = 0, former = 1
# band_in_comment = 1, new = 0, rebanded = 0, former = 1
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



