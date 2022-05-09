# Verify Old.Band. and New.Band numbers
# HMN Data
# Gaby Samaniego
# 2022-04-28

library(tidyverse)
library(lubridate)

# Bring data in 
raw_data <- read.csv("data/raw_data.csv",
                     na.strings = c("","NA"))

# Replace Band.Number 'XXXXXX' with NA
raw_data <- raw_data %>% 
  mutate(Band.Number = na_if(Band.Number, "XXXXXX"))

# Change date from character to date
raw_data <- raw_data %>% 
  mutate(Date = mdy(date)) %>% 
  mutate(date = Date)

raw_data <- raw_data %>% 
  select(-Date) 

class(raw_data$date)  

# Change band number from character to numeric
raw_data$Band.Number <- as.numeric(as.character((raw_data$Band.Number)))
class(raw_data$Band.Number)

# Sort data by band number, date and time
dat <- raw_data %>% 
  arrange(Band.Number, date, time)

# Verify that first use (date) of a band number corresponds to band status 1
# and following captures correspond to recaptures or band status R 

# extract the rows that equal a band number 

new_bands <- unique(dat$Band.Number)
dat$best_band_status <- NA

# for loop


for (i in 1:length(new_bands)){
  band_id <- new_bands[i]
  df <- filter(dat, Band.Number == band_id)
  
  if(nrow(df) = 1){
    dat$best_band_status[dat$Band.Number == band_id] <- 1
  } else {
    dat$best_band_status[dat$Band.Number == band_id & dat$date == min(df$date)] <- 1
  }
}

# complete this code after Ellen's help! 




