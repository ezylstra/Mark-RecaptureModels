# Verify Old.Band. and New.Band numbers
# HMN Data
# Gaby Samaniego
# 2022-04-28

install.packages("xlsx")

library(tidyverse)
library(lubridate)
library("xlsx")

# Bring data in 
raw_data <- read.csv("data/raw_data.csv")

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
sorted_data <- raw_data %>% 
  arrange(Band.Number, date, time)

# Verify that first use (date) of a band number corresponds to band status 1
# and following captures correspond to recaptures or band status R 

# Extract the rows that equal first capture and recaptures 
new__recap_bands <- subset(sorted_data, Band.Status %in% c("1","R"))

# Before for loop
# Extract the rows that equal a band number 
new_bands <- unique(new__recap_bands$Band.Number)
new__recap_bands$best_band_status <- NA
unique(new__recap_bands$Band.Status)

# For loop worked with Ellen, works but takes too long.  

for (i in 1:length(new_bands)){
  band_id <- new_bands[i]
  df <- filter(new__recap_bands, Band.Number == band_id)
  
  if(nrow(df) == 1){
    new__recap_bands$best_band_status[new__recap_bands$Band.Number == band_id] <- 1
  } else {
    new__recap_bands$best_band_status[new__recap_bands$Band.Number == band_id & new__recap_bands$date == min(df$date)] <- 1
    new__recap_bands$best_band_status[new__recap_bands$Band.Number == band_id & new__recap_bands$date > min(df$date)] <- "R"
  } 
}    

# Another way to do the same thing without the for loop from line 39 after sorting 

# Create a capture number column 
new__recap_bands$capture_number <- sequence(from = 1, rle(new__recap_bands$Band.Number)$lengths)

new__recap_bands$best_band_status <- ifelse(new__recap_bands$capture_number == 1, "1","R")

# Subset other band status (4, 6, 8, F)
other_band_status <- sorted_data %>% 
  filter(Band.Status %in% c("4", "6", "8", "F"))

# merge all band status
all_bands <- other_band_status %>% 
  bind_rows(new__recap_bands) %>% 
  arrange(Band.Number, date)

write.csv(all_bands,"output/full_data_2.csv", row.names = FALSE)
