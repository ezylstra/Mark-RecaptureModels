# Exploring banding effort between east and west sites
# Gaby Samaniego
# gabysamaniego@arizona.edu
# 2024-05-23

# Load packages
library(tidyverse)

# Clear environment
rm(list = ls()) 

# Load effort data
effort.raw <- read.csv('output/banding-effort-data/banding-effort-all-sites-RMNP.csv')
# Total banding days per year

# Edit effort data set
effort <- effort.raw %>% 
  # Sites not included in capture data for analysis:
  filter(!site %in% c('CLP', 'BGMD', 'WB2','WB1', 'WPK1', 'NFPC', 'POLC', 'SHIP')) %>% 
  mutate(location = ifelse(site %in% c('CLP', 'GNMTN', 'HOLZ', 'KV1'), 'west', 'east')) %>% 
  group_by(location, year) %>% 
  summarize(total_days = sum(total_banding_days)) %>% 
  pivot_wider(names_from = location,
              values_from = total_days) %>% 
  print()

# There is a significant difference in the total number of banding days between
# east and west locations. West locations were not operated in 2003.
