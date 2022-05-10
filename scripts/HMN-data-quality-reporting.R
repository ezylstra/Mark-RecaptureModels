# Data validation for monitoring data received by-weekly  
# Hummingbird Monitoring Network 
# Gaby Samaniego gaby@savehummingbirds.org
# May 2022

library(tidyverse)
library(pointblank)
library(lubridate)
library(stringr)

##### Data wrangling ##### 

# Bring in data 
data <- read.csv("data/HC_0421_HMNBandingData_2022.csv", 
                    na.strings = c("",NA))

# Replace <NA> created when reading csv file with NA values
data <- data %>% 
  mutate(across(.fns = ~replace(., .x == "<NA>", NA)))

# If Molt information is F (fresh) for all the rows in the data, R will treat it 
# as a logical vector and will replace all Fs with FALSE. We need to change that 
# by creating a new character column.  

# First, check for logical columns 
class(data$Head.Gorget.Molt)
class(data$Body.Molt)
class(data$Primaries.Molt)
class(data$Secondaries.Molt)
class(data$Tail.Molt)

# Second, apply the following code for each logical column 

# For Head Gorget Molt 
if(is.logical(data$Head.Gorget.Molt)){
  data <- data %>% 
    mutate(head2 = ifelse(Head.Gorget.Molt == FALSE, "F", NA)) %>% 
    mutate(Head.Gorget.Molt = head2) %>% 
    select(-head2)
}

# For Body Molt 
if(is.logical(data$Body.Molt)){
  data <- data %>% 
    mutate(body2 = ifelse(Body.Molt == FALSE, "F", NA)) %>% 
    mutate(Body.Molt = body2) %>% 
    select(-body2)
}

# For Primaries Molt 
if(is.logical(data$Primaries.Molt)){
  data <- data %>% 
    mutate(prim2 = ifelse(Primaries.Molt == FALSE, "F", NA)) %>% 
    mutate(Primaries.Molt = prim2) %>% 
    select(-prim2)
}

# For Secondaries Molt 
if(is.logical(data$Secondaries.Molt)){
  data <- data %>% 
    mutate(sec2 = ifelse(Secondaries.Molt == FALSE, "F", NA)) %>% 
    mutate(Secondaries.Molt = sec2) %>% 
    select(-sec2)
} 

# For Tail Molt 
if(is.logical(data$Tail.Molt)){
  data <- data %>% 
    mutate(tail2 = ifelse(Tail.Molt == FALSE, "F", NA)) %>% 
    mutate(Tail.Molt = tail2) %>% 
    select(-tail2)
}

# Capitalize all characters and factors across data frame 
data <- mutate_all(data, .funs=toupper)

# Remove all leading and trailing white spaces
data <- mutate_all(data,str_trim, side=c("both"))

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

# If any 'XXXXXX' in Band.Number, replace it with NA 
unique(data$Band.Number)
data <- data %>% 
  mutate(Band.Number = na_if(Band.Number, "XXXXXX"))


###### Data validation with pointblank package ######

# Set thresholds for report 
al <- action_levels(warn_at = 1, stop_at = 1) 

# Create pattern (regex) to validate if Band.Number is a letter and five numbers
pattern <- "[A-Z]{1}[0-9]{5}"

# Data validation. Most columns in the data frame will be validated in the agent    

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
  col_vals_regex(vars(Band.Number), regex = pattern, na_pass = TRUE) %>% 
  col_vals_in_set(vars(Leg.Condition), set = c("1","2","3","4","5","6","7",NA)) %>%
  col_vals_in_set(vars(Gorget.Color), set = c("O","R","V","P","B","G","GP","NS",
                                              "LS","MS","HS",NA)) %>% 
  col_vals_between(vars(Gorget.Count),0, 99, na_pass = TRUE) %>% 
  col_vals_between(vars(Head.Count),0, 99, na_pass = TRUE) %>% 
  col_vals_in_set(vars(Grooves), set = c("0","1","2","3",NA)) %>% 
  col_vals_in_set(vars(Buffy), set = c("Y","N","S","% GREEN",NA)) %>%
  col_vals_in_set(vars(Bill.Trait), set = c("R", "D",NA)) %>% 
  col_vals_between(vars(Green.on.back),0, 99, na_pass = TRUE) %>%
  col_vals_in_set(vars(Fat), set = c("0","1","2","3","P","T",NA)) %>%
  col_vals_in_set(vars(CP.Breed), set = c("9","8","7","5","2",NA)) %>%
  col_vals_in_set(vars(Head.Gorget.Molt), set = c("1","2","3","F","L","M","R",NA)) %>%
  col_vals_in_set(vars(Body.Molt), set = c("1","2","3","F","L","M","R",NA)) %>%
  col_vals_in_set(vars(Primaries.Molt), set = c("1","2","3","4","5","6","7","8","9",
                                           "0","F","L","M","R",NA)) %>%
  col_vals_in_set(vars(Secondaries.Molt), set = c("1","2","3","4","5","6","F","L","M","R",NA)) %>%
  col_vals_in_set(vars(Tail.Molt), set = c("1","2","3","4","5","F","L","M","R",NA)) %>% 
  col_vals_between(vars(Weight), 2, 9, na_pass = TRUE)
  
# Create report after validation  
interrogate(validation)

# If report shows columns that didn't pass the validation (fail), view errors or 
# failed values by rows
interrogate(validation) %>% 
  get_sundered_data(type = "fail")


##### Find duplicate band numbers #####
any(duplicated(data$Band.Number)) 
which(duplicated(data$Band.Number)) 

##### Create new csv with the validated data #####

# Organize columns' order so it matches main database when merging this csv to 
# central database
vetted_data <- data %>% 
  relocate(Protocol, CMR, Bander, Location, Date, Year, Month, Day, Time, 
           Old.Band.Status, Band.Status, Band.Number, Leg.Condition, 
           Tarsus.Measurement, Band.Size, Species, Sex, Age, Replaced.Band.Number)

# Create csv with vetted data. Make sure to update the site in the output name 
write.csv(vetted_data,"output/vetted_HC_data.csv", row.names = FALSE)

### Get summarized data for report ### 

# Number of individuals by Band Status, new birds=1, recaptures=R
data %>% 
  count(Band.Status)

# Number of individuals by species
data %>% 
  count(Species)  

# Number of recaptures multiple times a day
data %>% 
  count(Day.Recaptures)
 


