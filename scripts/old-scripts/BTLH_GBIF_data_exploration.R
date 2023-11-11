# Explore BTLH-GBIF data
# 2023-03-21

# Data downloaded from GBIF website
# Filters used in website:
# Species: Broad-tailed hummingbird
# Type of register: human observations
# Years: 1939 - 2022
# Months: November - March
# Country: Mexico

library(tidyverse)

# Read data
BTLH.GBIF <- read.csv("data/BTLH_GBIF_mex.csv")

# Data exploration
names(BTLH.GBIF)

# Explore records by elevation 
unique(BTLH.GBIF$elevation)
count(BTLH.GBIF, elevation)
sum(is.na(BTLH.GBIF$elevation))
# 86 records with elevation data
# 2694 NA records

# Explore records by year
unique(BTLH.GBIF$year)
count(BTLH.GBIF, year)





BTLH.GBIF %>% 
  arrange(year)
