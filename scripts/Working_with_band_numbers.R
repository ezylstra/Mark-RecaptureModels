# Working with band numbers
# To compare 2022 bands with 2002-2021 bands
# Gaby Samaniego gaby@savehummingbirds.org
# April 2022

library(tidyverse)

# Set working directory 
setwd("C:/Users/gabym/Documents/R/HummingBird/data")

# Bring in data, all bands (2002-2021) and new bands (2022)
old_bands <- read.csv("all_bands.csv")
ML_2022_bands <- read.csv("ML_vetted.csv")

# Replace letter in Band.Number with BBL codes
ML_new$Band.Number <- gsub("U","8100",ML_new$Band.Number)
head(ML_new)

# Get all band numbers and sort them 
bands %>% 
  arrange(Band.Number,Species, Age, Sex)