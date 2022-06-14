# Clean HMN's main database before using it for anything 
# Gaby Samaniego
# gaby@savehummingbirds.org
# 2022-06-08

library(tidyverse)
library(lubridate)
library(stringr)

# Bring in raw data
data <- read.csv("data/updated_raw_data.csv")

# Capitalize all characters and factors across data frame 
data <- mutate_all(data, .funs=toupper)

# Remove all leading and trailing white spaces
data <- mutate_all(data,str_trim, side=c("both"))

# Change date column from character to date
class(data$date)
data <- data %>% 
  mutate(date = mdy(date))
class(data$date)

# Remove unnecesary columns 
colnames(data)

data <- data %>% 
  select(-Tarsus,      # Checklist for paper
         -BS_Paper,    # Checklist for paper
         -TBS_test,    # Checklist for paper
         -TBS_Paper)   # Checklist for paper
            

