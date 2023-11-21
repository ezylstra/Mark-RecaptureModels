# Wrangle RMNP banding data
# Gaby Samaniego
# gabysamaniego@arizona.edu
# 2023-11-20

# Load packages
library(tidyverse)
library(data.table)
library(janitor)

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

# Remove '-' in column band_number and change it to class numeric
banded.dat$band_number <- gsub('-', '', banded.dat$band_number)
banded.dat$band_number <- as.numeric(as.character((banded.dat$band_number)))

# Change column date from character to class date
banded.dat <- banded.dat %>% 
  mutate(date = mdy(date))

# Split column date by year, month, and day
banded.dat <- mutate(banded.dat, year = year(date), 
                    month = month(date),
                    day = day(date))  

# Fix time... later. Add colon between hours and minutes

# Fix bander... later. Use the same code for Fred and Tena

# Change any SY (second year) in age to AHY (after hatch-year)
unique(banded.dat$age)
banded.dat$age[banded.dat$age == 'SY'] <- 'AHY'

# Fix other columns later... maybe

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


