# Exploring Broad-tailed hummingbird data 
# Gaby Samaniego
# gaby@savehummingbirds.org
# 2022-08-08

library(tidyverse)
library(lubridate)
library(stringr)

##### Data wrangling #####

# Bring in raw data
raw_data <- read.csv("data/updated_raw_data.csv")

# Capitalize all characters and factors across data frame 
band_data <- mutate_all(raw_data, .funs=toupper)

# Remove all leading and trailing white spaces
band_data <- mutate_all(band_data,str_trim, side=c("both"))

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

##### Sort data #####

# Sort data by Band.Number, Date, Species, and Sex
band_data <- band_data %>% 
  arrange(Band.Number, Date, Species, Sex)

##### Update Band.Status ##### 

# Verify that first use of a band number (date) corresponds to band status 1 (new)
# and following captures correspond to band status R (recapture) 

# Extract the rows that equal first capture (1) and recaptures (R) 
new__recap_bands <- subset(band_data, Band.Status %in% c("1","R"))

# Extract the rows that equal a band number and create a new column for Best.Band.Status
new_bands <- unique(new__recap_bands$Band.Number)
new__recap_bands$best_band_status <- NA
unique(new__recap_bands$Band.Status)

# Create a capture number column 
new__recap_bands$capture_number <- sequence(from = 1, rle(new__recap_bands$Band.Number)$lengths)

# Fill in Best.Band.Status with 1 and R  
new__recap_bands$best_band_status <- ifelse(new__recap_bands$capture_number == 1, "1","R")

# Subset other band status from data 
# 4 = band destroyed, 6 = band removed, 8 = band lost, F = foreign band
other_band_status <- band_data %>% 
  filter(Band.Status %in% c("4", "6", "8", "F"))

# Merge all band status
all_bands <- other_band_status %>% 
  bind_rows(new__recap_bands) %>% 
  arrange(Band.Number, Date)

##### Reorganize data frame ##### 
new_data <- all_bands %>% 
  select(-capture_number) %>% 
  relocate(Protocol, Bander, State, Location, Date, Year, Month, Day, Time,
           Old.Band.Status, Band.Status, best_band_status) %>% 
  rename(Best.Band.Status = best_band_status)

count(new_data, Band.Status)
count(new_data, Best.Band.Status)

# Replace NA values for band status 4, 6, 8, and F in Best.Band.Status
new_data$Best.Band.Status[new_data$Band.Status == "4"] <- 4
new_data$Best.Band.Status[new_data$Band.Status == "6"] <- 6
new_data$Best.Band.Status[new_data$Band.Status == "8"] <- 8
new_data$Best.Band.Status[new_data$Band.Status == "F"] <- "F"

##### BTLH DATA ##### 

# Select BTLH data
BTLH <- new_data %>% 
  filter(Species == "BTLH") 

count(BTLH, Protocol) # Engelman 8622, HMN 16465, Train 797, NA 16 (Utah site 2021 for education)

# Select BTLH data that follows HMN's protocol
BTLH_HMN <- new_data %>% 
  filter(Species == "BTLH", Protocol == "HMN")

# Organize BTLH data by sites and summarize it 
BTLH_sites <- BTLH_HMN %>% 
  group_by(Location, State) %>%
  summarize(First.Year = min(Year),
            Last.Year = max(Year),
            N.Years = length(unique(Year)),
            N.Months = length(unique(Month)),
            N.Dates = length(unique(Date)),
            N.Individuals = length(unique(Band.Number)),
            N.Captures = length(Band.Number)) %>%
  arrange(N.Captures) %>% 
  as.data.frame

# Remove sites with less than 10 bids captured and 1 year of monitoring
# 21 sites < 10 birds 
# 4 sites < 1 year 
BTLH_sites_filtered <- BTLH_sites %>% 
  filter(N.Years != 1, N.Captures > 10)

# Replace coordinates and elevation data 

# Bring in site information 
BTLH_sites_coordinates <- read.csv("data/BTLH_sites.csv")

# Add coordinates and elevation data 
BTLH_sites_final <- left_join(BTLH_sites_filtered, 
                            BTLH_sites_coordinates, 
                            by = "Location") %>% 
  relocate(Location, State, Latitude, Longitude, Elevation, First.Year, 
           Last.Year, N.Years, N.Months, N.Dates, N.Individuals, 
           N.Captures)

# Write csv with the data for BTLH
write.csv(BTLH_sites_final, "output/BTLH_sites_raw_data.csv", row.names = FALSE)


#### BTLH breeding #### 

## Need to think about this... 

# Change F for FEMALE and M for MALE
BTLH_HMN$Sex[BTLH_HMN$Sex == "F"] <- "FEMALE"
BTLH_HMN$Sex[BTLH_HMN$Sex == "M"] <- "MALE"

# Organize BTLH data by sites and summarize it 
BTLH_sites <- BTLH_HMN %>% 
  group_by(Location, State) %>%
  summarize(First.Year = min(Year),
            Last.Year = max(Year),
            N.Years = length(unique(Year)),
            N.Months = length(unique(Month)),
            N.Dates = length(unique(Date)),
            N.Individuals = length(unique(Band.Number)),
            N.Captures = length(Band.Number)) %>%
  arrange(N.Captures) %>% 
  as.data.frame


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

##### BTLH distribution map and monitoring points #####

library(rgdal)
library(broom)
library(maps)

# Read shape file
BTLH_distribution <- readOGR(dsn = "data/Broad-tailed range map",
                             layer = "data_0")

# Create a tidy format of the map
BTLH_distribution_tidy <- tidy(BTLH_distribution)

#Plot BTLH distribution and HMN sites with BTLH data  
ggplot(BTLH_distribution_tidy, aes(x = long, y = lat, group = group),) +
  geom_polygon(color = "black", 
               size = 0.1, 
               fill = "lightgrey") +
  borders("world", xlim = c(-120, -100), ylim = c(15, 40),) +
  borders("state") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  geom_point(data = BTLH_sites_final, 
           mapping = aes(x = Longitude, y = Latitude, group = State), # Added State as group to avoid error
           color = "red",
           size = 1)
            

