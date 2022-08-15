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
# EZ: Getting an error when I run this related to TailMid column
# If you're also getting error, suggest replacing above with the following:
# band_data <- raw_data %>%
#   mutate(across(!TailMid, .funs = toupper))

# Remove all leading and trailing white spaces
band_data <- mutate_all(band_data,str_trim, side=c("both"))
# EZ: Getting an error related to non UTF-8 strings in Comment column
# If you're also getting error, suggest replacing above with the following:
# band_data <- band_data %>%
#   mutate(across(!Comment, str_trim, side = "both"))

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

##### Reorganize data frame ##### 
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

# Select BTLH data for HMN's sites
BTLH_HMN <- new_data %>% 
  filter(Species == "BTLH", 
         Protocol == "HMN",
         Sex != "U") # Removed individuals with unknown sex 

# Organize BTLH data by sites and summarize it 
BTLH_sites <- BTLH_HMN %>% 
  group_by(Location, State) %>%
  summarize(First.Year = min(Year),
            Last.Year = max(Year),
            N.Years = length(unique(Year)),
            N.Months = length(unique(Month)),
            N.Days = length(unique(Date)),
            N.Captures = length(Band.Number),
            Individuals.Banded = length(unique(Band.Number)),
            N.Males = length(unique(Band.Number[Sex == "M"])),
            N.Females = length(unique(Band.Number[Sex == "F"]))) %>% 
  arrange(N.Captures) %>% 
  as.data.frame
# EZ: a few sites have the same location name except there's a 1 after it.
# Can the data from these sites be combined?  (if, for example, the banding
# station was moved just a little ways away but it's basically the same site)

# Remove sites with less than 10 bids captured and 1 year of monitoring
# 21 sites < 10 birds   
# 4 sites < 1 year 
# HSR (155 BTLH) started in 2021, we'll have data for 2022
# SC (Sabino Canyon, 48 BTLH) has data just for 2011, but monitored 2006-2015. Interesting, what happened in 2011? 


BTLH_sites_filtered <- BTLH_sites %>% 
  filter(N.Years != 1, N.Captures > 10)

# Add coordinates, elevation, and years of activity for each site 

# Bring in site information 
BTLH_sites_coordinates <- read.csv("data/BTLH_sites.csv")

# Join tables to add coordinates and elevation data 
BTLH_sites_final <- left_join(BTLH_sites_filtered, 
                            BTLH_sites_coordinates, 
                            by = "Location") %>% 
  relocate(Location, State, Latitude, Longitude, Elevation, First.Year, 
           Last.Year, N.Years, N.Active.Years, N.Months, N.Days, Individuals.Banded, 
           N.Captures)

# Write csv with the data for BTLH
write.csv(BTLH_sites_final, "output/BTLH_sites_raw_data.csv", row.names = FALSE)


##### BREEDING CONDITIONS #####

# Summarize breeding conditions by year also 
# Have BTLH been breeding earlier? If so, is this correlated to climate? 
# Can I answer this question with our data? 


##### BTLH distribution map and monitoring points #####

library(rgdal)
library(broom)
library(maps)

# Read shape file downloaded from UICN 
BTLH_distribution <- readOGR(dsn = "data/Broad-tailed range map", #dsn = data source name
                             layer = "data_0") # layer = name of the shape file 

# Convert spatial object to a data frame to use with ggplot2
BTLH_distribution_tidy <- tidy(BTLH_distribution)

# Add data from downloaded distribution map to tidy object created in line 246 
BTLH_distribution$id <- row.names(BTLH_distribution) 
BTLH_distribution_tidy <- left_join(BTLH_distribution_tidy, BTLH_distribution@data)

#Plot BTLH basic distribution map  
map1 <- ggplot(BTLH_distribution_tidy, aes(x = long, y = lat, 
                                   group = group, # Group keeps the polygon's shape
                                   fill = LEGEND)) + # Colors the map by range
  geom_polygon(color = "black", size = 0.1) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + # Remove grid from map
  borders("world", xlim = c(-130, -100), ylim = c(15, 40)) +
  borders("state") +
  labs(title = "BTLH Distribution Map") 
  

map1

# Add HMN sites to BTLH distribution map 
map1 +
  geom_point(data = BTLH_sites_final, 
             mapping = aes(x = Longitude, y = Latitude,
                           group = State, # Added State as group to avoid error
                           fill = Location),
             color = "red",
             size = 1) +
  labs(title = "BTLH Distribution Map with HMN sites") 
 

# I want to use this code to adjust the map to the distribution polygon 
coord_equal()  # adjusts map coordinates to polygon
  
# EZ: code below should work to plot banding locations on top of the range map
map2 <- ggplot(BTLH_distribution_tidy,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = LEGEND)) +
  geom_polygon(color = "black", size = 0.1) +
  borders("world", xlim = c(-130, -100), ylim = c(15, 40)) +
  borders("state") +  
  coord_equal() + 
  theme_bw() +
  theme(axis.title = element_blank(),
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom") + 
  labs(title = "BTLH Distribution Map") 
map2

# Adding locations of banding stations:
# Note: need to specify that group and fill (plotting features
# from the polygon layer) are NULL for these point features.
map2 +
  geom_point(data = BTLH_sites_final, 
             mapping = aes(x = Longitude, 
                           y = Latitude,
                           group = NULL,
                           fill = NULL),
             color = "black",
             size = 1,
             show.legend = FALSE) +
  labs(title = "BTLH Distribution Map with HMN sites") 

# Zooming into the banding locations
# I also added an example of how to label them, but only did for a few sites
# since many are in such close proximity to each other:
map3 <- ggplot(BTLH_distribution_tidy,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = LEGEND)) +
  geom_polygon(color = "black", size = 0.1) +
  borders("world", regions = c("us", "mexico")) +
  borders("state") +  
  theme_bw() +
  theme(axis.title = element_blank(),
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom") + 
  labs(title = "BTLH Distribution Map") 

map3 + geom_point(data = BTLH_sites_final,
                  mapping = aes(x = Longitude, 
                                y = Latitude,
                                group = NULL,
                                fill = NULL),
                  color = "black",
                  size = 2,
                  show.legend = FALSE) +
  coord_map(xlim = c(-120, -100), ylim = c(30, 42)) +
  labs(title = "BTLH Distribution Map with HMN sites") 

BTLH_sites_final <- BTLH_sites_final %>%
  mutate(nudge_lat = Latitude + 0.3)

map3 + geom_point(data = BTLH_sites_final,
                  mapping = aes(x = Longitude, 
                                y = Latitude,
                                group = NULL,
                                fill = NULL),
                  color = "black",
                  size = 2,
                  show.legend = FALSE) +
  coord_map(xlim = c(-120, -100), ylim = c(30, 42)) +
  labs(title = "BTLH Distribution Map with HMN sites") +
  geom_text(data = BTLH_sites_final[4:6,],
            aes(x = Longitude, 
                y = nudge_lat,
                group = NULL,
                fill = NULL,
                label = Location),
            color = "black",
            size = 3)

                            

            

