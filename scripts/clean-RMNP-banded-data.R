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

# Read all cvs files to merge

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

# Separate those bands that have "former" from those that have "foreign"
banded.dat <- banded.dat %>%
  mutate(foreign = 1 * str_detect(toupper(comments), "FOREIGN"),
         former = 1 * str_detect(toupper(comments), "FORMER"))

# Create data frame with comment combos
comment_combos_banded <- banded.dat %>% 
  group_by(band_in_comment, former, foreign) %>%
  summarize(n = length(former)) %>%
  data.frame()

# Looking at each of these indicator variable columns 
comment_combos_banded

filter(banded.dat, band_in_comment == 0, foreign == 1) 
# n = 4: 
# 4000-88228 AZ band just captured once. Not relevant. Record in recaptured data as well
# 4000-01786 just captured once. Not relevant. Record in recaptured data as well
# 5000-02820 recaptured twice 2005 and 2007. Relevant? We don't have first capture. Record in recapture data as well
# 4100-08433 just captured once. Not relevant. Record not in the recaptured data

filter(banded.dat, band_in_comment == 0, former == 1) 
# Letter N code is 5000

# I stopped here..............................................................

# n = 1: band removed, but not rebanded

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




# Summarize data for analysis 
dat <- banded.dat %>% 
  select(band_number, date, site, species, sex, age, year, day, month) %>%
  mutate(band_status = 1) %>% # 1 for all new bands applied
  filter(species == 'BTLH')

# Check for duplicated band numbers
if(any(duplicated(dat$band_number))){ # Are there duplicate band numbers?
  duplicates <- which(duplicated(dat$band_number))  # Which one is duplicated? 
  message(paste0("The following rows are duplicate band numbers: ",
                 paste0(duplicates, collapse = ",")))
}

# 


