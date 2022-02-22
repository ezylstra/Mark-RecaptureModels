# Working with KoBo datasheet 
# For HMN's 2022 data vetting 
# Gaby Samaniego

library(tidyverse)
library(pointblank)

# Set working directory
setwd("C:/Users/gabym/Documents/R/HummingBird/data")

# Bring in KoBo datasheet structure
KoBo_datasheet_structure <- read.csv("KoBo-datasheet.csv")
colnames(KoBo_datasheet_structure)

# Data quality reporting for Mount Lemmon site (ML)

ML_data <- read.csv("ML_data.csv")

ML <- 
  create_agent(
    tbl = ML_data,
    tbl_name = "ML_data",
    label = "ML Data Validation") %>% 
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
                                                    "I","J","K","L","M","N","O")) %>%
  col_vals_in_set(vars(Band.Size), set = c("B","C","D","E","F","G","H","I","J",
                                           "K","L","M","N","O")) %>%
  col_vals_in_set(vars(Leg.Condition), set = c("1","2","3","4","5","6","7")) %>%
  col_vals_in_set(vars(Gorget.Color), set = c("O","R","V","P","B","G","GP","NS",
                                              "LS","MS","HS")) %>% 
  col_vals_between(vars(Gorget.Count),1, 99, na_pass = TRUE) %>% 
  col_vals_between(vars(Head.Count),1, 99, na_pass = TRUE) %>% 
  col_vals_in_set(vars(Grooves), set = c("0","1","2","3")) %>% 
  col_vals_in_set(vars(Buffy), set = c("Y","N","S")) %>% 
  

interrogate(ML)

# Create columns for CMR and Protocol

new_columns <- mutate(ML, CMR = "Y", Protocol = "HMN")

# Create columns and separate date by year, month and day 


