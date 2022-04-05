# Working with KoBo datasheet 
# For HMN's 2022 data vetting 
# Gaby Samaniego

library(tidyverse)
library(lubridate)
library(pointblank)

# Set working directory
setwd("C:/Users/gabym/Documents/R/HummingBird/data")

# Bring in data 
ML_data <- read.csv("ML_data.csv", na.strings = c("","NA"))
head(ML_data)

# Capitalize all characters and factors
ML_data <- mutate_all(ML_data, .funs=toupper)
head(ML_data)

# Create columns for CMR and Protocol
ML_data <- mutate(ML_data, CMR = "Y", Protocol = "HMN")
head(ML_data)

# Change Date column from character to date
class(ML_data$Date)

ML_data <- ML_data %>% 
           mutate(Date = mdy(Date))

head(ML_data)
class(ML_data$Date)

# Change Time column from character to posix 
class(ML_data$Time)

ML_data <- ML_data %>% 
           mutate(Time = hm(Time))

head(ML_data)
class(ML_data$Time)

# Split date by year, month, and day
ML_data <- mutate(ML_data, Year = year(Date), Month = month(Date),Day = day(Date))  
head(ML_data)
class(ML_data$Month)

# Data quality reporting for Mount Lemmon site (ML)

# Set thresholds for report 
al <- action_levels(warn_at = 1, stop_at = 1) 

# Create pattern (regex) to check band numbers 
pattern <- "[a-z]{1}[0-9]{5}"
ML_data %>% test_col_vals_regex(vars(Band.Number), regex = pattern)

colnames(ML_data)

ML <- 
  create_agent(
    tbl = ML_data,
    tbl_name = "ML_data",
    label = "ML Data Validation",
    actions = al) %>% 
  col_is_date(vars(Date)) %>% 
  col_is_posix(vars(Time)) %>% 
  col_vals_equal(vars(Bander), value = "GS") %>%    
  col_vals_equal(vars(Location), value = "ML") %>%  
  col_vals_in_set(vars(Species), set = c("ANHU","ALHU","BBLH","BCHU","BADE",
                                         "BEHU","BTMG","BTLH","BUFH","BALO",
                                         "CAHU","COHU","LUHU","RIHU","HYHU",
                                         "RTHU","RUHU","VCHU","WEHU","UNHU")) %>% 
  col_vals_in_set(vars(Sex), set = c("M","F","U")) %>% 
  col_vals_in_set(vars(Age), set = c("0","1","2","5")) %>% 
  col_vals_in_set(vars(Band.Status), set = c("1","R","F","4","5","6","8")) %>%  
  col_vals_in_set(vars(Tarsus.Measurement), set = c("B","C","D","E","F","G","H",
                                                    "I","J","K","L","M","N","O",NA)) %>% 
  col_vals_in_set(vars(Band.Size), set = c("B","C","D","E","F","G","H","I","J",
                                           "K","L","M","N","O")) %>% 
  col_vals_regex(vars(Band.Number), regex = pattern) %>% 
  col_vals_in_set(vars(Leg.Condition), set = c("1","2","3","4","5","6","7")) %>% 
  col_vals_regex(vars(Replaced.Band.Number), regex = pattern) %>%
  col_vals_in_set(vars(Gorget.Color), set = c("O","R","V","P","B","G","GP","NS",
                                              "LS","MS","HS")) %>% 
  col_vals_between(vars(Gorget.Count),0, 99, na_pass = TRUE) %>% 
  col_vals_between(vars(Head.Count),0, 99, na_pass = TRUE) %>% 
  col_vals_in_set(vars(Grooves), set = c("0","1","2","3")) %>% 
  col_vals_in_set(vars(Buffy), set = c("Y","N","S")) %>% 
  col_vals_between(vars(Wing.Chord), 35, 80, na_pass = TRUE) %>% 
  col_vals_between(vars(Culmen), 12, 30, na_pass = TRUE) %>% 
  col_vals_in_set(vars(Fat), set = c("0","1","2","3","P","T")) %>%
  col_vals_in_set(vars(CP.Breed), set = c("9","8","7","5","2")) %>%
  col_vals_in_set(vars(Head.Gorget), set = c("1","2","3","F","L","M","R")) %>%
  col_vals_in_set(vars(Body), set = c("1","2","3","F","L","M","R")) %>%
  col_vals_in_set(vars(Primaries), set = c("1","2","3","4","5","6","7","8","9",
                                           "0","F","L","M","R")) %>%
  col_vals_in_set(vars(Secondaries), set = c("1","2","3","4","5","6","F","L","M","R")) %>%
  col_vals_in_set(vars(Tail.1), set = c("1","2","3","4","5","F","L","M","R")) %>% 
  col_vals_between(vars(Weight), 2, 12, na_pass = TRUE)
  
interrogate(ML)


# to get errors in the rows: Michael suggestions 

interrogate(ML) %>% 
  get_agent_report(display_table = FALSE)

interrogate(ML) %>% 
  get_sundered_data(type = "fail")

# Another way to check the information in each column
unique(ML_data$Bander)
unique(ML_data$Location)
unique(ML_data$Species)
unique(ML_data$Sex)
unique(ML_data$Age)
unique(ML_data$Band.Status)
unique(ML_data$Tarsus.Measurement)
unique(ML_data$Band.Size)
unique(ML_data$Leg.Condition)
unique(ML_data$Gorget.Color)
unique(ML_data$Gorget.Count)
unique(ML_data$Head.Count)
unique(ML_data$Grooves)
unique(ML_data$Body)

# Find duplicated band numbers
any(duplicated(ML_data$Band.Number)) # Are there duplicates? true or false
which(duplicated(ML_data$Band.Number)) # If true, which row contains the duplicate

# Organize columns order so it matches main database 



