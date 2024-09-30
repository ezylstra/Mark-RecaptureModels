# Exploring at potential transient birds
# Are individuals captured late in the season part of the breeding population or 
# are they individuals moving through?

# Original Code by Erin Zylstra
# Edited by Gaby Samaniego
# gabysamaniego@arizona.edu
# 2024-09-27

# Load packages
library(tidyverse)

# Clear environment
rm(list = ls())

# Load data
raw.dat <- read.csv('output/capture-data/cleanded-capture-data-RMNP-full.csv')

# Select columns and eliminate duplicates
dat <- raw.dat %>% 
  select(band, band_status, year, month, day, obsdate) %>% 
  distinct()

length(unique(dat$band))
# 10126 unique bands 

# Are individuals captured in August or September likely to be transients?

# Group capture dates into May-Jul, Aug 1-15, Aug 16-31
dat <- dat %>%
  mutate(MJJ = ifelse(month %in% 5:7, 1, 0),
         MJJA15 = ifelse(month %in% 5:7 | (month == 8 & day < 16), 1, 0),
         MJJA31 = ifelse(month %in% 5:8, 1, 0))

# Identify when first capture occurred and when recaptures occurred
bird_months <- dat %>%
  group_by(band) %>%
  summarize(n_captures = n(),
            n_years = length(unique(year)),
            first_date = obsdate[band_status == 1],
            n_MJJ = sum(MJJ),
            n_MJJA15 = sum(MJJA15),
            n_MJJA31 = sum(MJJA31),
            years = unique(year)) %>%
  data.frame() %>%
  mutate(first_cap = case_when(
    month(first_date) %in% 5:7 ~ "MJJ",
    month(first_date) == 8 & day(first_date) < 16 ~ "Aug1",
    month(first_date) == 8 & day(first_date) > 15 ~ "Aug2",
    .default = "Sep")) %>%
  mutate(first_cap = factor(first_cap, levels = c("MJJ", "Aug1", "Aug2", "Sep"))) %>% 
  pivot_wider(names_from = years,
              values_from = years) %>% 
  select(band, n_captures, n_years, first_date, n_MJJ, n_MJJA15, n_MJJA31, first_cap,
         '2003', '2004', '2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012')
            
# How many birds have been banded in each month group?
count(bird_months, first_cap)
# month    n       %
# MJJ     6694    66.1 
# Aug1    1595    15.8
# Aug2    1303    12.9
# Sep     534      5.2
# Total  10126    100

# Exploring recaptures per month period

# 1) Birds banded in September
sep <- filter(bird_months, first_cap == 'Sep' & n_captures >= 2)
count(filter(bird_months, first_cap == 'Sep' & n_captures >= 2 & n_years >= 2))
filter(bird_months, first_cap == 'Sep' & n_captures >= 2 & n_years >= 2) %>% 
  print(n = Inf, width = Inf)
filter(bird_months, first_cap == 'Sep' & n_captures >= 2 & n_years > 2) %>% 
  print(width = Inf)

# From the total 534 birds banded in September, 80 were recaptured. From these,
# 29 were recaptured in different years besides the year when the bird was banded
# (true recaptures). This represents the 5.4% of birds banded in September. From 
# these, just 4 birds have been recaptured more than two times. 

# 2) Birds banded in August between the 16th and 31st
aug2 <- filter(bird_months, first_cap == 'Aug2'& n_captures >= 2)
count(filter(bird_months, first_cap == 'Aug2' & n_captures >= 2 & n_years >= 2))
filter(bird_months, first_cap == 'Aug2' & n_captures >= 2 & n_years > 2) %>% 
  print(n = Inf, width = Inf)

# From the 1303 birds banded between August 16th and 31st, 267 were recaptured. 
# From these, 186 were recaptured in different years besides the year when the 
# bird was banded (true recaptures). This represents 14.3% of birds banded in 
# the last half of August. From these, 55 have been recaptured more than two times.

# 3) Birds banded in August between the 1st and 15th
aug1 <- filter(bird_months, first_cap == 'Aug1'& n_captures >= 2)
count(filter(bird_months, first_cap == 'Aug1' & n_captures >= 2 & n_years >= 2))
filter(bird_months, first_cap == 'Aug1' & n_captures >= 2 & n_years > 2) %>% 
  print(n = Inf, width = Inf)

# From the 1595 birds banded between August 1st and 15th, 469 were recaptured. 
# From these, 308 were recaptured in different years besides the year when the 
# bird was banded (true recaptures). This represents 19.3% of birds banded in 
# the first half of August. From these, 82 have been recaptured more than two times.

# 4) Birds banded between May and July
mjj <- filter(bird_months, first_cap == 'MJJ'& n_captures >= 2)
count(filter(bird_months, first_cap == 'MJJ' & n_captures >= 2 & n_years >= 2))
filter(bird_months, first_cap == 'MJJ' & n_captures >= 2 & n_years > 2) %>% 
  print(n = Inf)

# From the 6694 birds banded between May and July, 2783 were recaptured. 
# From these, 1432 were recaptured in different years besides the year when the 
# bird was banded (true recaptures). This represents 21.4% of birds banded in 
# May, June and July. From these, 447 have been recaptured more than two times.

# What is the age of birds captured in September and August? 
(ages.sep <- raw.dat %>% 
  filter(band_status == 1,
         month == 9) %>%
    count(band_age))
# AHY    51
# HY    483
# Most are HY birds. Maybe birds born in the study sites or juveniles moving through? 

(ages.aug2 <- raw.dat %>% 
  filter(band_status == 1,
         month(obsdate) == 8 & day(obsdate) > 15) %>%  
  count(band_age))
# AHY   514
# HY    789
# More than half HY birds. 

(ages.aug1 <- raw.dat %>% 
    filter(band_status == 1,
           month(obsdate) == 8 & day(obsdate) < 16) %>%  
    count(band_age))
# AHY   1163
# HY     432
# Most are AHY birds. 
