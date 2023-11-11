# Exploring Broad-tailed hummingbird data from Rocky Mountain National Park
# Gaby Samaniego
# gaby@savehummingbirds.org
# 2022-08-23

library(tidyverse)
library(lubridate)
library(stringr)

##### Data wrangling #####

# Bring in raw data
raw_data <- read.csv("data/RMNP_data_new.csv",
                     na.strings = c("",NA),
                     strip.white = TRUE)

# Capitalize all characters and factors across data frame 
band_data <- raw_data %>%
  mutate_if(is.character, str_to_upper) 

# Remove unnecessary columns
band_data <- band_data %>% 
  select(-latitude, -longitude, -elevation, -region, -session, -week, -X0, 
         -X0.1, -TArsus.LEG, -Size.Material)    

# Change column's names 
band_data <- band_data %>% 
  rename(Bander = Initials.Bdr,
         Date = DATE,
         Year = year,
         Month = mo,
         Day = day,
         Time = time,
         Band.Number = BAND,
         Sex = SEX,
         Age = AGE)

# Remove - from Band.Number
band_data$Band.Number <- gsub("-","",as.character(band_data$Band.Number))
# It worked but it generates an error...

# Change date column from character to date
band_data <- band_data %>% 
  mutate(Date = mdy(Date))

# Change Band.Number from character to numeric
band_data$Band.Number <- as.numeric(as.character((band_data$Band.Number)))

# Explore the data
count(band_data, Band.Number)

# Sort data by Band.Number, Date, Species, and Sex
band_data <- band_data %>% 
  arrange(Band.Number, Date, Species, Sex)

# Data exploration
unique(band_data$Bander)       # Banders Fred and Tina Engelmann
unique(band_data$Location)     # All records are from RMBL
unique(band_data$Species)      # All records are BTLH
unique(band_data$Band.Status)  # Records are either 1 or R
unique(band_data$Sex)          # Records are for M and F
unique(band_data$Age)          # All records are for age 1

# Summarize data
RMBL_data <- band_data %>% 
  summarize(N.Captures = length(Band.Number),
            Individuals.Banded = length(unique(Band.Number)),
            N.Recaptures = length(unique(Band.Number[Band.Status == "R"])),
            N.Males = length(unique(Band.Number[Sex == "M"])),
            N.Females = length(unique(Band.Number[Sex == "F"]))) %>% 
  as.data.frame



