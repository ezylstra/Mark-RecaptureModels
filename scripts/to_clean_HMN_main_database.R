# Clean HMN's main database before using it for anything 
# Gaby Samaniego
# gaby@savehummingbirds.org
# 2022-06-08

library(tidyverse)
library(lubridate)
library(stringr)

# Bring in raw data
raw_data <- read.csv("data/updated_raw_data.csv")

# Capitalize all characters and factors across data frame 
data <- mutate_all(raw_data, .funs=toupper)

# Remove all leading and trailing white spaces
data <- mutate_all(raw_data,str_trim, side=c("both"))

# Change date column from character to date
class(data$date)
data <- data %>% 
  mutate(date = mdy(date))
class(data$date)

# Remove columns 
data <- data %>% 
  select(-Tarsus,                   # Checklist for paper
         -BS_Paper,                 # Checklist for paper
         -TBS_test,                 # Checklist for paper
         -TBS_Paper,                # Checklist for paper
         -T1,                       # Unknown use  
         -t3,                       # Unknown use
         -Diff_factor,              # Unknown use
         -Diff_factorA,             # Unknown use
        -TARSUS_UNIT,               
        -BAND_UNIT,
        -Leg.Condition,
        -Orig_Tarsus.Condition,
        -TARSUS.CONDITION,
        -Field_BAND.SIZE,
        -BAND.SIZE)    

# If any 'XXXXXX' in Band.Number, replace it with NA 
data <- data %>% 
  mutate(Band.Number = na_if(Band.Number, "XXXXXX"))

# Change band number from character to numeric
data$Band.Number <- as.numeric(as.character((data$Band.Number)))
class(data$Band.Number)

# Sort data by band number, date, species and sex
data <- data %>% 
  arrange(Band.Number, date, Species, Sex)

# Verify that first use (date) of a band number corresponds to band status 1
# and following captures correspond to recaptures or band status R 

# Extract the rows that equal first capture and recaptures 
new__recap_bands <- subset(data, Band.Status %in% c("1","R"))

# Extract the rows that equal a band number 
new_bands <- unique(new__recap_bands$Band.Number)
new__recap_bands$best_band_status <- NA
unique(new__recap_bands$Band.Status)

# Create a capture number column 
new__recap_bands$capture_number <- sequence(from = 1, rle(new__recap_bands$Band.Number)$lengths)

new__recap_bands$best_band_status <- ifelse(new__recap_bands$capture_number == 1, "1","R")

# Subset other band status (4, 6, 8, F)
other_band_status <- data %>% 
  filter(Band.Status %in% c("4", "6", "8", "F"))

# merge all band status
all_bands <- other_band_status %>% 
  bind_rows(new__recap_bands) %>% 
  arrange(Band.Number, date)

# Reorganize data frame 
new_data <- all_bands %>% 
  select(-capture_number) %>% 
  relocate(CMR, Protocol, Initials.Bdr, latitude, longitude, elevation, State,
           region, Location, session, week, dayofyear, DayBlock, date, year, mo,
           day, time, OldBand.Status, Band.Status, best_band_status)

count(new_data, Band.Status)
count(new_data, best_band_status)

##### BTLH data ##### 

# Select BTLH data
BTLH <- new_data %>% 
  filter(Species == "BTLH") 

count(BTLH, Protocol) # Engelman 8622, HMN 16465, Train 797, NA 16 (Utah site 2021)

BTLH_HMN <- new_data %>% 
  filter(Species == "BTLH", Protocol == "HMN")

count(BTLH_HMN, CMR) # N 1005? Why N?

#### Sites where BTLH occurs ####

BTLH_sites <- BTLH_HMN %>% 
  group_by(latitude, longitude, elevation, State, region, Location) %>%
  summarize(first_yr = min(year),
            last_yr = max(year),
            n_yrs = length(unique(year)),
            n_mo = length(unique(mo)),
            n_dates = length(unique(date)),
            n_indiv = length(unique(Band.Number)),
            n_captures = length(Band.Number)) %>%
  arrange(n_captures) %>% 
  as.data.frame

#### Sites where BTLH breeds ####

# Change F for FEMALE and M for MALE
BTLH_HMN$Sex[BTLH_HMN$Sex == "F"] <- "FEMALE"
BTLH_HMN$Sex[BTLH_HMN$Sex == "M"] <- "MALE"

# Change CPBreed from character to numeric
BTLH_HMN$CPBreed <- as.numeric(as.character((BTLH_HMN$CPBreed)))
class(BTLH_HMN$CPBreed)

BTLH_breeding <- BTLH_HMN %>% 
  group_by(latitude, longitude, elevation, State, region, Location) %>% 
  filter(Sex == "FEMALE") %>%
  as.data.frame
  
  
  
  summarize(n_yrs = length(unique(year)),
            n_females = length(unique(Band.Number)),
            n_females_CPB2 = length(unique(Band.Number[CPBreed == 2])),
            n_females_CPB5 = length(unique(Band.Number[CPBreed == 5])),
            n_females_CPB7 = length(unique(Band.Number[CPBreed == 7])),
            n_females_CPB8 = length(unique(Band.Number[CPBreed == 8])),
            n_females_CPB9 = length(unique(Band.Number[CPBreed == 9])),) %>%  
  as.data.frame

count(BTLH_HMN, CPBreed)      
              
filter(BTLH_HMN, Sex == "FEMALE")

class(BTLH_HMN$Sex)
class(BTLH_HMN$CPBreed)
  
count(BTLH_HMN, Sex)
total(BTLH_breeding, n_females)

# Summarize breeding conditions by year also 
# Have BTLH been breeding earlier? If so, is this correlated to climate? 
# Can I answer this question with our data? 

# Sites with data up to 3 years 

#####################

# Select BTLH data
VCHU <- new_data %>% 
  filter(Species == "VCHU") 

count(VCHU, Protocol) # Engelman 8622, HMN 16465, Train 797, NA 16 (Utah site 2021)

VCHU_HMN <- new_data %>% 
  filter(Species == "VCHU", Protocol == "HMN")

count(VCHU_HMN, CMR) # N 1005? Why N?

#### Sites where BTLH occurs ####

VCHU_sites <- VCHU_HMN %>% 
  group_by(latitude, longitude, elevation, State, region, Location) %>%
  summarize(first_yr = min(year),
            last_yr = max(year),
            n_yrs = length(unique(year)),
            n_mo = length(unique(mo)),
            n_dates = length(unique(date)),
            n_indiv = length(unique(Band.Number)),
            n_captures = length(Band.Number)) %>%
  arrange(n_captures) %>% 
  as.data.frame

#### Sites where BTLH breeds ####

# Change F for FEMALE and M for MALE
VCHU_HMN$Sex[VCHU_HMN$Sex == "F"] <- "FEMALE"
VCHU_HMN$Sex[VCHU_HMN$Sex == "M"] <- "MALE"

# Change CPBreed from character to numeric
VCHU_HMN$CPBreed <- as.numeric(as.character((VCHU_HMN$CPBreed)))
class(VCHU_HMN$CPBreed)

VCHU_breeding <- VCHU_HMN %>% 
  group_by(latitude, longitude, elevation, State, region, Location) %>% 
  filter(Sex == "FEMALE") %>%
  as.data.frame



summarize(n_yrs = length(unique(year)),
          n_females = length(unique(Band.Number)),
          n_females_CPB2 = length(unique(Band.Number[CPBreed == 2])),
          n_females_CPB5 = length(unique(Band.Number[CPBreed == 5])),
          n_females_CPB7 = length(unique(Band.Number[CPBreed == 7])),
          n_females_CPB8 = length(unique(Band.Number[CPBreed == 8])),
          n_females_CPB9 = length(unique(Band.Number[CPBreed == 9])),) %>%  
  as.data.frame

count(VCHU_HMN, CPBreed)      

filter(BTLH_HMN, Sex == "FEMALE")

class(BTLH_HMN$Sex)
class(BTLH_HMN$CPBreed)

count(BTLH_HMN, Sex)
total(VCHU_breeding, n_females)

# Summarize breeding conditions by year also 
# Have BTLH been breeding earlier? If so, is this correlated to climate? 
# Can I answer this question with our data? 

# Sites with data up to 3 years 

