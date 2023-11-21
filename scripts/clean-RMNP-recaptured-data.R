# Wrangle RMNP recaptured data
# Gaby Samaniego
# gabysamaniego@arizona.edu
# 2023-11-20

# Load packages
library(tidyverse)
library(data.table)
library(janitor)

# Read all cvs files to merge

# Create a list of all files to merge
recaptured.files <- list.files(path = "data/RMNP-recaptured-by-year/", 
                           pattern = "*.csv", 
                           full.names = TRUE)
recaptured.dat <- lapply(recaptured.files, fread, sep = ",")  

# Combine all csv files in a data frame
recaptured.dat <- rbindlist(recaptured.dat, fill = TRUE)

# Fix columns....

# Replace space in column names with '_'
recaptured.dat <- recaptured.dat %>% 
  clean_names()

#### SKIP THIS FOR NOW ####
# Remove '-' in column band_number and change it to class numeric
recaptured.dat$band_number <- gsub('-', '', recaptured.dat$band_number)
recaptured.dat$band_number <- as.numeric(as.character((recaptured.dat$band_number)))

# 
