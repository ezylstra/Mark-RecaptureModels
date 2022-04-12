# Working with band numbers
# To compare 2022 bands with 2002-2021 bands
# Gaby Samaniego gaby@savehummingbirds.org
# April 2022

library(tidyverse)

# Set working directory 
setwd("C:/Users/gabym/Documents/R/HummingBird/data")

# Bring in data, all bands (2002-2021), new bands (2022) and BBL letter codes
old_bands <- read.csv("all_bands.csv")
ML_2022_bands <- read.csv("ML_vetted.csv")
letter_codes <- read.csv("BBL_letter_codes.csv")


# Replace letter in Band.Number with BBL codes

# First Separate letter form numbers in Band.Number column 
a <- ML_2022_bands$band_letter <-substr(ML_2022_bands$Band.Number,
                                   start = 1, 
                                   stop = 1)
a

b <- ML_2022_bands$band_number <- substr(ML_2022_bands$Band.Number,
                                    start = 2,
                                    stop = 6)
b

# Second create new column with band number containing BBL codes

ML_BBL <- ML_2022_bands %>% 
  inner_join(letter_codes, 
             by = c("band_letter" = "letter"))

ML_BBL

done <- ML_BBL %>% 
  mutate(Band.Number = paste0(letter_number, band_number))

done

# Now delete columns you dont need anymore 


# Get all band numbers and sort them 
old_bands

bands %>% 
  arrange(Band.Number,Species, Age, Sex)


# Get information for Report 