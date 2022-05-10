# Working with band numbers
# To compare new bands applied during current season with previous season's bands
# Gaby Samaniego gaby@savehummingbirds.org
# April 2022

library(tidyverse)

#### Bring in data ####

# All band numbers used by HMN's monitoring program
old_bands <- read.csv("data/all_bands.csv")

# Vetted data for current season we want to compare 
new_bands <- read.csv("output/vetted_HC_data.csv") # Data vetted with R script

# Bird Banding Laboratory (BBL) letter codes
letter_codes <- read.csv("data/BBL_letter_codes.csv")

### Data wrangling ###

# Replace letter in Band.Number with BBL codes
# Separate letter from numbers in Band.Number column. Band numbers have six 
# digits, they are always 1 letter (A-Z) followed by five numbers (0-9)
unique(new_bands$Band.Number)
new_bands$band_letter <-substr(new_bands$Band.Number,
                                   start = 1, 
                                   stop = 1)

new_bands$band_number <- substr(new_bands$Band.Number,
                                    start = 2,
                                    stop = 6)

# Create new column in vetted data with band number containing BBL codes
BBL <- new_bands %>% 
  inner_join(letter_codes, 
             by = c("band_letter" = "letter"))

# Merge BBL code with numbers from Band.Number 
new_bands <- BBL %>% 
  mutate(Band.Number.New = paste0(letter_number, band_number))

# Delete unnecessary columns created to assigned the BBL codes to band numbers 
# Reorganize column's order, and
# Replace name in column Band.Number
ready <- new_bands %>% 
  select(-band_letter, 
         -band_number, 
         -band_number, 
         -letter_number, 
         -Band.Number) %>%
  relocate(Protocol, CMR, Bander, Location, Date, Year, Month, Day, Time, 
           Old.Band.Status, Band.Status, Band.Number.New, Leg.Condition, 
           Tarsus.Measurement, Band.Size, Species, Sex, Age, Replaced.Band.Number) %>% 
  rename(Band.Number = Band.Number.New)

write.csv(ready,"output/vetted_CFCK_data_GS.csv", row.names = FALSE)

##### Check if recapture band numbers (band status R) matches first band number 
##### assigned to that recaptured bird (band status 1)  

# Sort data by band number, species, age and sex
old_bands <- old_bands %>% 
  arrange(Band.Number, Species, Age, Sex)


