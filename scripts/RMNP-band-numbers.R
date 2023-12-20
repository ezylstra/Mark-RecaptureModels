
# Load packages
library(tidyverse)
library(data.table)
library(janitor)

rm(list = ls())

# Create a list of all recapture files to merge
recaptured.files <- list.files(path = "data/RMNP-recaptured-by-year/", 
                               pattern = "*.csv", 
                               full.names = TRUE)
recaptured.dat <- lapply(recaptured.files, fread, sep = ",")  

# Combine all csv files in a data frame
recaptured.dat <- rbindlist(recaptured.dat, fill = TRUE)

# Replace space in column names with '_'
dat <- recaptured.dat %>% clean_names()

# Look at columns
count(dat, original_site) # One "mcgc" that should probably be "MCGC"
count(dat, species) # There are some RUHU (n = 116) and CAHU (19)
count(dat, original_sex) # F, M, plus 1 "f" and 1 "A"
count(dat, recapture_year) # 2003-2011
# Then recapture_1 through recapture_17 

# comments
count(dat, comments) 
# 3945 of 4586 are blank, 330 are NA, rest are things like: *NEW: E11915 or 
# within 12 min...

# Look at format of entries in band_number column:
bn <- sort(unique(dat$band_number)) # Length = 3279
table(nchar(bn))
# 3239 of them have 10 characters, 40 of them have 11

sum(str_detect(bn, "[0-9]{4}-[0-9]{5}") * nchar(bn) == 10)
# All 3239 with 10 characters are correct format (4 numbers - 5 numbers)

sum(str_detect(bn, "[0-9]{4}-[0-9]{5}\\*") * nchar(bn) == 11)
# all 40 with 11 characters are correct format with * at end.

# Create a variable to indicate when an asterisk appears at end of band number
dat <- dat %>%
  mutate(band_asterisk = 1 * str_detect(band_number, "[0-9]{4}-[0-9]{5}\\*"))

# Need to find new band numbers in comments column, even when band_number
# column doesn't have asterisk
# Looking for [A-Z]##### or ####-#####
dat <- dat %>%
  # First change comments = NA to ""
  mutate(comments = ifelse(is.na(comments), "", comments)) %>%
  mutate(band_in_comment = 1 * str_detect(comments, "[A-Z][0-9]{5}|[0-9]{4}-[0-9]{5}")) %>%
  data.frame()
count(dat, band_in_comment)
head(filter(dat, band_in_comment == 0)) # 4471 don't have a band number in them
head(filter(dat, band_in_comment == 1)) # 115 do, BUT note that sometimes there
# are notes about other birds' bands (eg, "captured with E12354")
count(filter(dat, band_in_comment == 1), comments)
# and sometimes it looks like the former band number is listed here.

count(dat, band_asterisk, band_in_comment)
# Some that don't have asterisk in band_number do have comments about bands
# But ALL with asterisks have comments about bands (which is good)

# Looks like we can separate those that have "new"/"rebanded" from those that 
# have "former"
dat <- dat %>%
  mutate(new = 1 * str_detect(toupper(comments), "NEW"),
         rebanded = 1 * str_detect(toupper(comments), "REBANDED"), 
         former = 1 * str_detect(toupper(comments), "FORMER"))
comment_combos <- dat %>% 
  group_by(band_in_comment, new, rebanded, former) %>%
  summarize(n = length(new)) %>%
  data.frame()

# Looking at each of these indicator variable columns
comment_combos
filter(dat, band_in_comment == 0, new == 1) 
# n = 1: Not relevant (comment about p9)
filter(dat, band_in_comment == 0, rebanded == 1) 
# n = 7: band removed, but not rebanded
filter(dat, band_in_comment == 1, new == 0, rebanded == 0, former == 0) 
# n = 7: these are irrelevant ("captured with" or "captured w/")
# n = 2: "possibly related to E12085" 
# n = 1: "prob. C23826"
dat %>% filter(band_in_comment == 1, new == 1, rebanded == 0, former == 0) %>%
  select(band_number, original_date, recapture_year, recapture_1, comments)
# n = 86: always has new band number in comments
filter(dat, band_in_comment == 1, new == 0, rebanded == 1, former == 0) 
# n = 1: rebanded as E39458 (so same info as those with "NEW")
filter(dat, band_in_comment == 1, new == 0, rebanded == 0, former == 1) 
# n = 11: All former band numbers (so band_number column is new band)
filter(dat, band_in_comment == 1, new == 1, rebanded == 1, former == 0) 
# n = 3: Need to check two of these that have a new band number but also say
# that band was removed.
filter(dat, band_in_comment == 1, new == 1, rebanded == 0, former == 1) 
# n = 4: New band in band_number column; band in comments is former.

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
