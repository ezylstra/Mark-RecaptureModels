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

# Capitalize all characters and factors
ML_data <-mutate_all(ML_data, .funs=toupper)

# Create columns for CMR and Protocol
ML_data <- mutate(ML_data, CMR = "Y", Protocol = "HMN")

# Split date by year, month, and day, now is not working!
ML_data <- mutate(ML_data, Year = year(Date), Month = month(Date),Day = day(Date))  # old, now is not working!!! 

# Bring in KoBo datasheet structure to verify column names
KoBo_datasheet_structure <- read.csv("KoBo-datasheet.csv")
colnames(KoBo_datasheet_structure)

# Data quality reporting for Mount Lemmon site (ML)

# Set thresholds for report 
al <- action_levels(warn_at = 0.1, stop_at = 0.1) 

head(ML_data)

ML <- 
  create_agent(
    tbl = new,
    tbl_name = "ML_data",
    label = "ML Data Validation",
    actions = al) %>% 
  col_vals_equal(vars(Bander), value = "GS") %>%   # 1
  col_vals_equal(vars(Location), value = "ML") %>% # 2
  col_vals_in_set(vars(Species), set = c("ANHU","ALHU","BBLH","BCHU","BADE",
                                         "BEHU","BTMG","BTLH","BUFH","BALO",
                                         "CAHU","COHU","LUHU","RIHU","HYHU",
                                         "RTHU","RUHU","VCHU","WEHU","UNHU")) %>% # 3
  col_vals_in_set(vars(Sex), set = c("M","F","U")) %>% # 4
  col_vals_in_set(vars(Age), set = c("0","1","2","5")) %>% # 5
  col_vals_in_set(vars(Band.Status), set = c("1","R","F","4","5","6","8")) %>% # 6 
  col_vals_in_set(vars(Tarsus.Measurement), set = c("B","C","D","E","F","G","H",
                                                    "I","J","K","L","M","N","O")) %>% # 7
  col_vals_in_set(vars(Band.Size), set = c("B","C","D","E","F","G","H","I","J",
                                           "K","L","M","N","O")) %>% # 8
  col_vals_in_set(vars(Leg.Condition), set = c("1","2","3","4","5","6","7")) %>% # 9
  col_vals_in_set(vars(Gorget.Color), set = c("O","R","V","P","B","G","GP","NS",
                                              "LS","MS","HS")) %>% # 10
  col_vals_between(vars(Gorget.Count),0, 99, na_pass = TRUE) %>% # 11
  col_vals_between(vars(Head.Count),0, 99, na_pass = TRUE) %>% # 12
  col_vals_in_set(vars(Grooves), set = c("0","1","2","3")) %>% # 13
  col_vals_in_set(vars(Buffy), set = c("Y","N","S")) # 14 

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

# Find duplicated band numbers
any(duplicated(ML_data$Band.Number)) # Are there duplicates? true or false
which(duplicated(ML_data$Band.Number)) # If true, which row contains the duplicate

# Organize columns order so it matches main database 



