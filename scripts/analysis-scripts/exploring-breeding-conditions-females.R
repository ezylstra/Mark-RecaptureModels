# Explore BTLH female breeding conditions at RMNP
# Gaby Samaniego
# gabysamaniego@arizona.edu
# 2024-09-22

# Load packages
library(tidyverse)

# Clear environment
rm(list = ls())

# Load data
dat <- read.csv('output/capture-data/cleanded-capture-data-RMNP-full.csv')

# Change column obsdate from character to date class
dat <- dat %>%  
  mutate(obsdate = ymd(obsdate))

# From Fred's reports: gravid females are noted by E (egg) designation: 
# E6, slightly swollen abdomen; 
# E7, swollen; 
# E8, very enlarged abdomen; 
# E9, egg visible and mass equal to or greater than 4.0 grams 

# Subset data set
breeding <- dat %>% 
  filter(!is.na(breeding)) %>%  
  select(band, breeding, year, month, day, obsdate)

# How many female BTLH are in the data set?
length(unique(breeding$band))
# There are 1662 unique females 

# How many times each category of breeding condition was registered?
count(breeding, breeding)
#   breeding   n
#       E6    956
#       E7    578
#       E8    457
#       E9    224
#     TOTAL  2215
# Same females with multiple breeding conditions. Checked data set and some
# females have different conditions in the same year. For now I'll consider 
# breeding females if they have any of the conditions (E6 to E9) at any time of
# year

# When are female BTLH breeding at RMNP?
(breeding.by.month <- breeding %>% 
    group_by(month) %>% 
    summarize(unique_females = length(unique(band))))
#  May      409
#  June     1160
#  July     293
#  August   1  

# When in August was that female breeding and what was her condition?
filter(breeding, month == 8)
# Early August

# Take away:
# Breeding season seems to be May, June and July. Maybe we could include the 
# first half of August as breeding season, but just one female might not
# be enough for justifying the extent of breeding season in August. 
 