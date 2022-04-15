# Working with band numbers
# To compare 2022 bands with 2002-2021 bands
# Gaby Samaniego gaby@savehummingbirds.org
# April 2022

library(tidyverse)

# Set working directory 
setwd("C:/Users/gabym/Documents/R/HummingBird/data")

# Bring in data, all band numbers (2002-2021), new bands (2022) and 
# Bird Banding Laboratory (BBL) letter codes
old_bands <- read.csv("all_bands.csv")
ML_2022_bands <- read.csv("ML_vetted.csv") # Data vetted with R code 
letter_codes <- read.csv("BBL_letter_codes.csv")

# Replace letter in Band.Number with BBL codes
unique(ML_2022_bands$Band.Number)

# Separate letter from numbers in Band.Number column in ML 2022 data 
a <- ML_2022_bands$band_letter <-substr(ML_2022_bands$Band.Number,
                                   start = 1, 
                                   stop = 1)
a

b <- ML_2022_bands$band_number <- substr(ML_2022_bands$Band.Number,
                                    start = 2,
                                    stop = 6)
b

# Create new column in ML 2022 with band number containing BBL codes
ML_BBL <- ML_2022_bands %>% 
  inner_join(letter_codes, 
             by = c("band_letter" = "letter"))
head(ML_BBL)

# Merge BBL code with numbers of Band.Number 
ML_new_bands <- ML_BBL %>% 
  mutate(Band.Number.New = paste0(letter_number, band_number))

head(ML_ready)

# Delete unnecessary columns created to assigned the BBL codes to band numbers 
# and reorganize column's order
ML_ready <- ML_new_bands %>% 
  select(-band_letter, 
         -band_number, 
         -band_number, 
         -letter_number, 
         -Band.Number) %>%
  relocate(Protocol, CMR, Bander, Location, Date, Year, Month, Day, Time, 
           Old.Band.Status, Band.Status, Band.Number.New, Leg.Condition, 
           Tarsus.Measurement, Band.Size, Species, Sex, Age, Replaced.Band.Number) %>% 
  rename(Band.Number = Band.Number.New)

head(ML_ready)

##### Check if recapture band numbers (band status R) matches first band number 
##### assigned to that recaptured bird (band status 1)  

# This is my next step, not ready yet, consult with Erin 



