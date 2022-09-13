# Create HMN's sites summarized table
# Gaby Samaniego gaby@savehummingbirds.org
# 2022-09-08

library(tidyverse)
library(lubridate)
library(stringr)

# Bring in raw data
raw_data <- read.csv("data/updated_raw_data.csv",
                     na.strings = c("",NA))

##### DATA WRANGLING #####

# Capitalize all characters and factors across data frame 
band_data <- raw_data %>%
  mutate(across(!TailMid, .funs = toupper)) # To avoid error with TailMid column

# Remove all leading and trailing white spaces
band_data <- band_data %>%
  mutate(across(!Comment, str_trim, side = "both")) # To avoid error with comments column

# Remove unnecessary columns
band_data <- band_data %>% 
  select(-Tarsus, -BS_Paper, -TBS_test, -TBS_Paper, -CMR, -latitude, -longitude,
         -elevation, -region, -session, -week, -dayofyear, -DayBlock, -T1, -t3, 
         -Diff_factor, -Diff_factorA, -TARSUS_UNIT, -BAND_UNIT, -Leg.Condition, 
         -Orig_Tarsus.Condition, -Field_BAND.SIZE)    

# Change column's names 
band_data <- band_data %>% 
  rename(Bander = Initials.Bdr,
         Date = date,
         Year = year,
         Month = mo,
         Day = day,
         Time = time,
         Old.Band.Status = OldBand.Status,
         Tarsus.Condition = TARSUS.CONDITION,
         Band.Size = BAND.SIZE,
         Gorget.Color = GPORCOlOR,
         Gorget.Count = GorCount....,
         Head.Count = Head.GCnt....,
         Primary.Width.P.10.Shape = PriWidth...P10.Shape,
         Wing.Tail.Trait = Wng.TailTrait,
         Tail.Center = TailCent,
         Tail.Middle = TailMid,
         Tail.Outer = TailOut,
         Tail.Measurement = TailMeas....,
         Wing.Chord = WingCd,
         Pollen.Color.Location = Pollen..Color,
         Fat = Fat.fie1d.data,
         CP.Breed = CPBreed,
         Head.Gorget.Molt = Gorget.head.molt,
         Primaries.Molt = PriMary.Molt,
         Secondaries.Molt = Secondary.Molt,
         Day.Recaptures = Day.Recap..,
         Recapture.Time.1 = recap.time.1,
         Recapture.Time.2 = recap.time.2,
         Recapture.Time.3 = recap.time.3,
         Recapture.Time.4 = recap.time.4)

# If any 'XXXXXX' in Band.Number, replace it with NA 
if (any(unique(band_data$Band.Number) == "XXXXXX")) {
  message("Replacing band number 'XXXXXX' with NA")
  band_data <- band_data %>% 
    mutate(Band.Number = na_if(Band.Number, "XXXXXX"))
}

# Format columns

# Change date column from character to date
band_data <- band_data %>% 
  mutate(Date = mdy(Date))

# Change Band.Number from character to numeric
band_data$Band.Number <- as.numeric(as.character((band_data$Band.Number)))

# Change CP.Breed from character to numeric
band_data$CP.Breed <- as.numeric(as.character((band_data$CP.Breed)))

# Change Year from character to numeric
band_data$Year <- as.numeric(as.character((band_data$Year)))

# Change Month from character to numeric
band_data$Month <- as.numeric(as.character((band_data$Month)))

# Merge data for banding locations that are close to each other or are moved just 
# a little ways away but it's basically the same site
band_data$Location[band_data$Location == "MA1"] <- "MA"
band_data$Location[band_data$Location == "SWRS1"] <- "SWRS"

##### Update Band.Status ##### 

# Verify that first use of a band number (date) corresponds to band status 1 (new)
# and following captures correspond to band status R (recapture) 

# Extract the rows that equal first capture (1) and recaptures (R) 
new__recap_bands <- subset(band_data, Band.Status %in% c("1","R"))

# Extract the rows that equal a band number and create a new column for Best.Band.Status
new_bands <- unique(new__recap_bands$Band.Number)
new__recap_bands$best_band_status <- NA
unique(new__recap_bands$Band.Status)

# Create a capture number column and add sequence of captures
new__recap_bands$capture_number <- sequence(from = 1, rle(new__recap_bands$Band.Number)$lengths)

# Fill in Best.Band.Status with 1 and R  
new__recap_bands$best_band_status <- ifelse(new__recap_bands$capture_number == 1, "1","R")

# Subset other band status from data 
# 4 = band destroyed, 6 = band removed, 8 = band lost, F = foreign band
other_band_status <- band_data %>% 
  filter(Band.Status %in% c("4", "6", "8", "F"))

# Merge all band status (1, R, 4, 6, 8, F)
all_bands <- other_band_status %>% 
  bind_rows(new__recap_bands) %>% 
  arrange(Band.Number, Date)

# Reorganize data frame, new_data Contains ALL HMN's banding data 
new_data <- all_bands %>% 
  select(-capture_number) %>% 
  relocate(Protocol, Bander, State, Location, Date, Year, Month, Day, Time,
           Old.Band.Status, Band.Status, best_band_status) %>% 
  rename(Best.Band.Status = best_band_status)

# Replace NA values for band status 4, 6, 8, and F in Best.Band.Status
new_data$Best.Band.Status[new_data$Band.Status == "4"] <- 4
new_data$Best.Band.Status[new_data$Band.Status == "6"] <- 6
new_data$Best.Band.Status[new_data$Band.Status == "8"] <- 8
new_data$Best.Band.Status[new_data$Band.Status == "F"] <- "F"

##### SORT DATA #####

# Sort data by Band.Number, Date, Species, and Sex
new_data <- new_data %>% 
  arrange(Band.Number, Date, Species, Sex)

##### SUMMARIZE DATA FOR HMN's SITES #####

# Summarize data for all HMN's banidng sites  
HMN_sites <- new_data %>% 
  filter(Protocol == "HMN",
         !(Location %in% c("AL1"))) %>% #AL1 has 5 records on a different monitoring date
  group_by(Location, State) %>% 
  summarize(First.Year = min(Year),
            Last.Year = max(Year),
            N.Active.Years = length(unique(Year)),
            Start.Month = min(Month),
            End.Month = max(Month),
            Total.Captures = length(Band.Number),
            Individuals.Banded = length(unique(Band.Number)),
            N.Recaptures = length(unique(Band.Number[Best.Band.Status == "R"])),
            N.Males = length(unique(Band.Number[Sex == "M"])),
            N.Females = length(unique(Band.Number[Sex == "F"])),
            Individual.BTLH = length(unique(Band.Number[Species == "BTLH"]))) %>%
  arrange(State) %>% 
  as.data.frame 

# Replace numeric month to it's abbreviation name 
HMN_sites <- HMN_sites %>% 
  mutate(Start.Month = month.abb[as.numeric(Start.Month)]) %>% 
  mutate(End.Month = month.abb[as.numeric(End.Month)])

# Breeding information

# Change CP.Breed 8 to 9 
new_data$CP.Breed[new_data$CP.Breed == 8] <- 9

breeding_data <- new_data %>% 
  group_by(Location, State) %>%
  filter(Protocol == "HMN",
         Sex == "F" &
           !is.na(CP.Breed)) %>% 
  count(CP.Breed) %>% 
  pivot_wider(names_from = CP.Breed, values_from = n) %>% 
  arrange(State)

# Why are there missing CA sites in the breeding data???

# Add coordinates and elevation for all HMN's sites 

# Bring in site information 
sites_coordinates <-read.csv("data/HMN_sites_coordinates.csv",
                             na.strings = c("",NA))

# Join tables
all_sites_data <- left_join(HMN_sites, 
                             sites_coordinates, 
                             by = "Location") 

all_sites_data <- all_sites_data %>% 
  relocate(Location, State, Elevation, Latitude, Longitude) %>% 
  as.data.frame 

write.csv(all_sites_data, "output/HMN_all_sites.csv", row.names = FALSE)

