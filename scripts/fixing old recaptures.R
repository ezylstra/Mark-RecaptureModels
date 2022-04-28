# Verify Old.Band. and New.Band numbers
# HMN Data
# Gaby Samaniego
# 2022-04-28

library(tidyverse)
library(lubridate)

# Bring data
raw_data <- read.csv("data/raw_data.csv",
                     na.strings = c("","NA"))

# Replace Band.Number 'XXXXXX' with NA
raw_data <- raw_data %>% 
  mutate(Band.Number = na_if(Band.Number, "XXXXXX"))

class(raw_data$Band.Number)
unique(raw_data$Band.Number)
str(raw_data)

# Change date from character to date
class(raw_data$date)

raw_data <- raw_data %>% 
  mutate(Date = mdy(date)) %>% 
  mutate(date = Date)

raw_data <- raw_data %>% 
  select(-Date) 

class(raw_data$date)  

# Change band number form character to numeric

raw_data <- apply(raw_data, c("Band.Number"), as.numeric())




raw_data <- mutate(raw_data, Band_Number = as.numeric(raw_data$Band.Number)

                   


raw_data <- as.numeric(raw_data$Band.Number)

df1$x1<-as.numeric(df1$x1)

# Sort data by date and band number
dat <- raw_data %>% 
  arrange(Band.Number, date)



