# Exploring Broad-tailed hummingbird data 
# Gaby Samaniego
# gaby@savehummingbirds.org
# 2022-08-08

library(tidyverse)
library(lubridate)
library(stringr)

##### DATA WRANGLING #####

# Bring in raw data
raw_data <- read.csv("data/updated_raw_data.csv")

# Capitalize all characters and factors across data frame 
# old code: band_data <- mutate_all(raw_data, .funs=toupper)  
band_data <- raw_data %>%
 mutate(across(!TailMid, .funs = toupper)) # To avoid error with TailMid column

# Remove all leading and trailing white spaces
# old code: band_data <- mutate_all(band_data,str_trim, side=c("both"))
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

# Merge data for banding locations that are close to each other or are moved just 
# a little ways away but it's basically the same site

# Locations to merge: 
# MA - MA1 <- YES, Both sites close together MA1 has data  for 2002, MA has data from 2003 forward 
# AL - AL1 <- NO, AL1 has 5 records but on different monitoring dates than AL, maybe it was a training site? Protocol is HMN for both
# RA - RA1 <- NO, Both sites in Arizona, RA1 is a "training" site 
# PA - PA2 <- NO, PA is a site in Arizona, PA2 is a site in British Columbia 
# SWRS - SWRS1 <- YES, SWRS1 was the site we used while the station was closed due to covid, they are very close together
# W1 - NO, training site 
# T2 - NO, training site    

band_data$Location[band_data$Location == "MA1"] <- "MA"
band_data$Location[band_data$Location == "SWRS1"] <- "SWRS"

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


##### BTLH DATA EXPLORATION ##### 

# Select BTLH data for sites that follow HMN's protocol, sex are male and female,
# and exclude sites that have < 1 year of data and < 10 birds captured. 
BTLH_HMN <- new_data %>% 
  filter(Species == "BTLH", 
         Protocol == "HMN",
         Sex != "U", # Removes 10 individuals with unknown sex. These haven't been recaptured
         !(Location %in% c("BL","BM","IC","IP","MXL","RR","TU","WR","AV","CNM","CO", 
         "MOCA","TV", "AL1","PO","AR","KS","CLAY","FL","SG","RC","SC","HSR")))
          # We know these sites have < 1 year of data and < 10 birds captured from 
          # previous data exploration 

# Organize BTLH banding data by monitoring sites and summarize it 
BTLH_data <- BTLH_HMN %>% 
  group_by(Location, State) %>%
  summarize(N.Captures = length(Band.Number),
            Individuals.Banded = length(unique(Band.Number)),
            N.Recaptures = length(unique(Band.Number[Band.Status == "R"])),
            N.Males = length(unique(Band.Number[Sex == "M"])),
            N.Females = length(unique(Band.Number[Sex == "F"]))) %>% 
  arrange(N.Captures) %>% 
  as.data.frame
  
# Add coordinates and elevation for each monitoring site 

# Bring in site information 
BTLH_sites_coordinates <- read.csv("data/BTLH_sites.csv")

# Join tables
BTLH_sites_data <- left_join(BTLH_data, 
                            BTLH_sites_coordinates, 
                            by = "Location") %>% 
  relocate(Location, State, Latitude, Longitude, Elevation, N.Captures)

##### SITES' INFORMATION AND SAMPLING EFFORT #####

# Select banding data that follows HMN's protocol and includes sites with BTLH data.
# By using ALL the banding data set (new_data) instead of BTLH_HMN we are including 
# all the monitoring days at each site regardless of BTLH been trapped   
sites <- new_data %>% 
  filter(Protocol == "HMN",
         Location %in% c("SCSNA","KP","CH","HC","CAVO","BRCA","GC","CFCK","ESC",  
                         "FG","RA","FH","MI","MG","MA","AL","SWRS","SH","MV","PP",
                         "EC","PA","WCAT","PCBNM","DGS","ML"))
                        # Location with BTLH data from previous data exploration

# Summarize site's activity  
site_activity <- sites %>% 
  group_by(Location) %>% 
  summarize(N.Active.Years = length(unique(Year)),
            First.Year = min(Year),
            Last.Year = max(Year),
            N.Banding.Days = length(unique(Date))) %>% 
  as.data.frame 

# Estimate site effort
estimates_site_effort <- sites %>% 
  group_by(Location, State, Year) %>% 
  summarize(N.Banding.Days = length(unique(Date))) %>% 
  group_by(Location) %>% 
  summarize(Range = range(N.Banding.Days),
            Mean = mean(N.Banding.Days))

##### CREATE FINAL DATA FRAME WITH BTLH DATA AND SITES #####

# Join BTLH data with site's activity
BTLH <- left_join(BTLH_sites_data,
                  site_activity, 
                  by = "Location") %>% 
  relocate(Location, State, Latitude, Longitude, Elevation, First.Year, Last.Year,
           N.Active.Years, N.Banding.Days, N.Captures)

# Write csv with BTLH data 
write.csv(BTLH, "output/BTLH_sites_data.csv", row.names = FALSE)

##### BTLH  DISTRIBUTION MAPS WITH MONITORING POINTS #####

library(rgdal)
library(broom)
library(maps)
library(mapproj) # Needed for coord_map function in Erin's code

# Read shape file downloaded from UICN 
BTLH_distribution <- readOGR(dsn = "data/Broad-tailed range map", #dsn = data source name
                             layer = "data_0") # layer = name of the shape file 

# Convert spatial object to a data frame to use with ggplot2
BTLH_distribution_tidy <- tidy(BTLH_distribution)

# Add data from downloaded distribution map to tidy object created in line 203 
BTLH_distribution$id <- row.names(BTLH_distribution) 
BTLH_distribution_tidy <- left_join(BTLH_distribution_tidy, BTLH_distribution@data)

#Plot BTLH basic distribution map  
map1 <- ggplot(BTLH_distribution_tidy, 
               aes(x = long, 
                   y = lat, 
                   group = group, # Group keeps the polygon's shape
                   fill = LEGEND)) + # Colors the map by range
  geom_polygon(color = "black", size = 0.1) + 
  borders("world", xlim = c(-130, -100), ylim = c(15, 40)) +
  borders("state") +
  coord_equal() + # Adjusts coordinates to polygon 
  theme_bw() +
  theme_bw() +
  theme(axis.title = element_blank(),
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom") + 
  labs(title = "BTLH Distribution Map") 
  
map1

# Add HMN's banding locations to BTLH distribution map 
map2 <- map1 +
    geom_point(data = BTLH_sites_final, 
             mapping = aes(x = Longitude, 
                           y = Latitude,
                           group = NULL, # NULL = plotting features from the polygon layer
                           fill = NULL), # NULL = plotting features from the polygon layer
             color = "black",
             size = 1,
             show.legend = FALSE) +
  labs(title = "BTLH Distribution Map with HMN sites")
 
map2

# Zooming into the banding locations using map1 
map3 <- map1 + geom_point(data = BTLH_sites_final,
                  mapping = aes(x = Longitude, 
                                y = Latitude,
                                group = NULL,
                                fill = NULL),
                  color = "black",
                  size = 2,
                  show.legend = FALSE) +
  coord_map(xlim = c(-120, -100), ylim = c(30, 42)) + # Needed to install mapproj package
  labs(title = "BTLH Distribution Map with HMN sites") 

map3 


# Adding monitoring sites labels to the map 

# Code to fix location of the label on map 
BTLH_sites_final <- BTLH_sites_final %>%
  mutate(nudge_lat = Latitude + 0.3)  

# Create map with labels  
map4 <-map1 + geom_point(data = BTLH_sites_final,
                  mapping = aes(x = Longitude, 
                                y = Latitude,
                                group = NULL,
                                fill = NULL),
                  color = "black",
                  size = 2,
                  show.legend = FALSE) +
  coord_map(xlim = c(-120, -100), ylim = c(30, 42)) +
  labs(title = "BTLH Distribution Map with HMN sites") +
  geom_text(data = BTLH_sites_final[4:6,], # Just added a few labels to the map
            aes(x = Longitude, 
                y = nudge_lat, # Moves the label, so it is not on top of the point 
                group = NULL,
                fill = NULL,
                label = Location),
            color = "black",
            size = 3)

map4                            

##### Create map containing HMN's sites within BTLH breeding range #####  

# obtain sites within BTLH distribution range

library(sf)

# Read shape file with sf package 
BTLH_distribution_sf <- st_read(dsn = "data/Broad-tailed range map/data_0.shp") 

# Convert BTLH sites data frame into sf object
wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" # From Jeff's code to add to sf object

sites_sf <- BTLH_sites_coordinates %>%
  mutate_at(vars(Longitude, Latitude), as.numeric) %>%   # coordinates must be numeric,from internet
  st_as_sf( # converts object to an sf object
    coords = c("Longitude", "Latitude"),
    crs = wgs84) # this worked! 

# st_within returns logical vector indicating whether a point is within the polygon
points_within <- st_within(x = sites_sf, y = BTLH_distribution_sf) %>% 
  lengths > 0 # I got it! False and true! 7 points are within the breeding range! 

# use this vector to select rows from the data frame containing BTLH sites info 
# that are within the distribution polygon
BTLH_sites_in_range <- BTLH_sites_coordinates[points_within, ]
write.csv(x = BTLH_sites_in_range,
          file = "BTLH_sites_coordinates",
          row.names = FALSE)
BTLH_sites_in_range

# Create the map 

# Code to fix location of the label on map 
BTLH_sites_in_range <- BTLH_sites_in_range %>%
  mutate(nudge_lat = Latitude + 0.3)  

map5 <- map1 + geom_point(data = BTLH_sites_in_range,
                          mapping = aes(x = Longitude, 
                                        y = Latitude,
                                        group = NULL,
                                        fill = NULL),
                          color = "black",
                          size = 2,
                          show.legend = FALSE) +
  coord_map(xlim = c(-120, -100), ylim = c(30, 42)) +
  labs(title = "BTLH Distribution Map with HMN sites within the species' range") +
  geom_text(data = BTLH_sites_in_range,
            aes(x = Longitude, 
                y = nudge_lat, # Moves the label, so it is not on top of the point 
                group = NULL,
                fill = NULL,
                label = Location),
            color = "black",
            size = 3)
  
map5

##### BTLH BREEDING DATA #####

# Summarize breeding conditions by year also 
# Have BTLH been breeding earlier? If so, is this correlated to climate? 
# Can I answer this question with our data? 

# Are BTLH breeding in/near our monitoring sites? 

# CP.Breed measures egg development. Values 9 and 8 in CP.Breed = breeding, 
# Replace value 8 in CP.Breed by 9 

BTLH_breeding <- BTLH_HMN %>% 
  group_by(Location, State) %>% 
  summarize(N.Captures = length(Band.Number),
            Individuals.Banded = length(unique(Band.Number)),
            N.Females = length(unique(Band.Number[Sex == "F"])),
            N.Females.Breeding.9 = length(unique(Band.Number[CP.Breed == 9])),
            N.Females.Breeding.8 = length(unique(Band.Number[CP.Breed == 8])),
            N.Females.Breeding.7 = length(unique(Band.Number[CP.Breed == 7])),
            N.Females.Breeding.5 = length(unique(Band.Number[CP.Breed == 5])),
            N.Females.Breeding.2 = length(unique(Band.Number[CP.Breed == 2]))) %>% 
  arrange(N.Captures) %>% 
  as.data.frame

  

