# Data quality control for biweekly monitoring data 
# Hummingbird Monitoring Network 
# Gaby Samaniego gaby@savehummingbirds.org
# April 2022

library(tidyverse)
library(lubridate)
library(pointblank)

##### Data wrangling ##### 

# Bring in data 
data <- read.csv("data/ML_data.csv", 
                    na.strings = c("","NA"))

# Capitalize all characters and factors across data frame 
data <- mutate_all(data, .funs=toupper)

# Create columns to indicate if the data will be used for capture mark recapture
# (CMR) analysis and to add the protocol used to collect it
data <- mutate(data, CMR = "Y", Protocol = "HMN")

# Change Date column from character to date
class(data$Date)
data <- data %>% 
           mutate(Date = mdy(Date))
class(data$Date)

# Split column Date by year, month, and day
data <- mutate(data, Year = year(Date), 
                  Month = month(Date),
                  Day = day(Date))  

# Duplicate Band Status column to match columns' names in main database when 
# merging data frames  
data <- data %>% 
  mutate(Old.Band.Status = Band.Status)

###### Data quality reporting for Mount Lemmon site (ML) ######

# Set thresholds for report 
al <- action_levels(warn_at = 1, stop_at = 1) 

# Create pattern (regex) to check that band number has a letter and five numbers
#### Need to do it again, it is not working #### 
pattern <- "[a-z]{1}[0-9]{5}"
ML_data %>% test_col_vals_regex(vars(Band.Number), regex = pattern)

unique(ML_data$Band.Number)

# Data validation with pointblank package   

validation <- 
  create_agent(
    tbl = data,
    tbl_name = "Vetted_data",
    label = "Data Validation",
    actions = al) %>% 
  col_is_date(vars(Date)) %>% 
  col_vals_in_set(vars(Bander), set = c("GS","ML","SMW","BJ","KO","TT","LY",
                                        "ER","AS")) %>%    
  col_vals_in_set(vars(Location), set = c("ML","HC","SWRS","PA","FH","DGS","MV",
                                          "MPGF","HSR","CFCK","ESC","WCAT")) %>%  
  col_vals_in_set(vars(Species), set = c("ANHU","ALHU","BBLH","BCHU","BADE",
                                         "BEHU","BTMG","BTLH","BUFH","BALO",
                                         "CAHU","COHU","LUHU","RIHU","HYHU",
                                         "RTHU","RUHU","VCHU","WEHU","UNHU")) %>% 
  col_vals_in_set(vars(Sex), set = c("M","F","U",NA)) %>% 
  col_vals_in_set(vars(Age), set = c("0","1","2","5",NA)) %>% 
  col_vals_in_set(vars(Band.Status), set = c("1","R","F","4","5","6","8",NA)) %>%  
  col_vals_in_set(vars(Tarsus.Measurement), set = c("B","C","D","E","F","G","H",
                                                    "I","J","K","L","M","N","O",NA)) %>% 
  col_vals_in_set(vars(Band.Size), set = c("B","C","D","E","F","G","H","I","J",
                                           "K","L","M","N","O",NA)) %>% 
  col_vals_in_set(vars(Leg.Condition), set = c("1","2","3","4","5","6","7",NA)) %>% 
  col_vals_in_set(vars(Gorget.Color), set = c("O","R","V","P","B","G","GP","NS",
                                              "LS","MS","HS",NA)) %>% 
  col_vals_between(vars(Gorget.Count),0, 99, na_pass = TRUE) %>% 
  col_vals_between(vars(Head.Count),0, 99, na_pass = TRUE) %>% 
  col_vals_in_set(vars(Grooves), set = c("0","1","2","3",NA)) %>% 
  col_vals_in_set(vars(Buffy), set = c("Y","N","S",NA)) %>% 
  col_vals_between(vars(Green.on.back),0, 99, na_pass = TRUE) %>%
  col_vals_between(vars(Wing.Chord), 35, 80, na_pass = TRUE) %>% 
  col_vals_between(vars(Culmen), 12, 30, na_pass = TRUE) %>% 
  col_vals_in_set(vars(Fat), set = c("0","1","2","3","P","T",NA)) %>%
  col_vals_in_set(vars(CP.Breed), set = c("9","8","7","5","2",NA)) %>%
  col_vals_in_set(vars(Head.Gorget), set = c("1","2","3","F","L","M","R",NA)) %>%
  col_vals_in_set(vars(Body), set = c("1","2","3","F","L","M","R",NA)) %>%
  col_vals_in_set(vars(Primaries), set = c("1","2","3","4","5","6","7","8","9",
                                           "0","F","L","M","R",NA)) %>%
  col_vals_in_set(vars(Secondaries), set = c("1","2","3","4","5","6","F","L","M","R",NA)) %>%
  col_vals_in_set(vars(Tail.1), set = c("1","2","3","4","5","F","L","M","R",NA)) %>% 
  col_vals_between(vars(Weight), 2, 9, na_pass = TRUE)
  
# Create report after validation 
interrogate(validation)

# View the errors (fails) by rows
interrogate(validation) %>% 
  get_sundered_data(type = "fail")

##### Find duplicate band numbers #####
any(duplicated(data$Band.Number)) # Are there duplicates? true or false
which(duplicated(data$Band.Number)) # If true, which row contains the duplicate

##### Create new csv with the validated data for ML site #####

# Organize columns' order so it matches main database 
vetted_data <- data %>% 
  relocate(Protocol, CMR, Bander, Location, Date, Year, Month, Day, Time, 
           Old.Band.Status, Band.Status, Band.Number, Leg.Condition, 
           Tarsus.Measurement, Band.Size, Species, Sex, Age, Replaced.Band.Number)

# Create csv with vetted data 
write.csv(vetted_data,"output/vetted_ML_Data.csv")




