# Preparing capture histories and survival analysis 
# Sites: Gaby's thesis ML, DGS, WCAT, PCBNM 
# Gaby Samaniego
# gaby@savehummingbirds.org
# 2022-02-28

library(tidyverse)
library(lubridate)
library(stringr)
library(data.table) # Used to combine lists in a data frame
library(RMark)

#------------------------- DATA WRANGLING ---------------------------------#

# Bring in raw data
raw.data <- read.csv("data/2002-2022_raw_data.csv",
                     na.strings = c("",NA))

# Remove unnecessary columns
band.data <- raw.data %>% 
  select(-Tarsus, -BS_Paper, -TBS_test, -TBS_Paper, -CMR, -latitude, -longitude,
         -elevation, -region, -session, -week, -dayofyear, -DayBlock, -T1, -t3, 
         -Diff_factor, -Diff_factorA, -TARSUS_UNIT, -BAND_UNIT, -Leg.Condition, 
         -Orig_Tarsus.Condition, -Field_BAND.SIZE)  

# Change column's names 
band.data <- band.data %>% 
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

# Remove all leading and trailing white spaces
band.data <- band.data %>%
  mutate(across(!Comment, str_trim, side = "both")) # To avoid error with comment column

# Capitalize all characters and factors across data frame 
band.data <- band.data %>% 
  mutate_if(is.character, str_to_upper) 

# If any 'XXXXXX' in Band.Number, replace it with NA 
if (any(unique(band.data$Band.Number) == "XXXXXX")) {
  message("Replacing band number 'XXXXXX' with NA")
  band.data <- band.data %>% 
    mutate(Band.Number = na_if(Band.Number, "XXXXXX"))
}

sum(is.na(band.data$Band.Number))
# Total 46 NAs

# Format columns

# Change date column from character to date
band.data <- band.data %>% 
  mutate(Date = mdy(Date))

# Change Band.Number from character to numeric
band.data$Band.Number <- as.numeric(as.character((band.data$Band.Number)))

# Change CP.Breed from character to numeric
band.data$CP.Breed <- as.numeric(as.character((band.data$CP.Breed)))

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
# SH - ML <- YES, sites are very close together and three birds have good recapture data

band.data$Location[band.data$Location == "MA1"] <- "MA"
band.data$Location[band.data$Location == "SWRS1"] <- "SWRS"
band.data$Location[band.data$Location == "SH"] <- "ML"

# Sort data 

# Sort data by Band.Number, Date, Species, and Sex
band.data <- band.data %>% 
  arrange(Band.Number, Date, Species, Sex)

# Update Band.Status 

# Verify that first use of a band number (date) corresponds to band status 1 (new)
# and following captures correspond to band status R (recapture) 

# Extract the rows that equal first capture (1) and recaptures (R) 
new.recap.bands <- subset(band.data, Band.Status %in% c("1","R"))

# Extract the rows that equal a band number and create a new column for Best.Band.Status
new.bands <- unique(new.recap.bands$Band.Number)
new.recap.bands$best.band.status <- NA 
unique(new.recap.bands$Band.Status)

# Create a capture number column and add sequence of captures
new.recap.bands$capture.number <- sequence(from = 1, 
                                           rle(new.recap.bands$Band.Number)$lengths)

# Fill in Best.Band.Status with 1 and R  
new.recap.bands$best.band.status <- ifelse(new.recap.bands$capture.number == 1, "1","R")

# Subset other band status from data 
# 4 = band destroyed, 6 = band removed, 8 = band lost, F = foreign band
other.band.status <- band.data %>% 
  filter(Band.Status %in% c("4", "6", "8", "F"))

# Merge all band status (1, R, 4, 6, 8, F)
all.bands <- other.band.status %>% 
  bind_rows(new.recap.bands) %>% 
  arrange(Band.Number, Date)

# Reorganize data frame, new_data Contains ALL HMN's banding data 
new.data <- all.bands %>% 
  select(-capture.number) %>% 
  relocate(Protocol, Bander, State, Location, Date, Year, Month, Day, Time,
           Old.Band.Status, Band.Status, best.band.status) %>% 
  rename(Best.Band.Status = best.band.status)

# Replace NA values for band status 4, 6, 8, and F in Best.Band.Status
new.data$Best.Band.Status[new.data$Band.Status == "4"] <- 4
new.data$Best.Band.Status[new.data$Band.Status == "6"] <- 6
new.data$Best.Band.Status[new.data$Band.Status == "8"] <- 8
new.data$Best.Band.Status[new.data$Band.Status == "F"] <- "F"

# Check for inconsistencies in the data for Species, Age and Sex 

# Code from Erin's script in GitHub

# Is each band number associated with a single species?
check.species <- new.data %>% 
  filter(Band.Number != !is.na(new.data$Band.Number)) %>%  # Removes 46 NAs in Band.Number
  group_by(Band.Number) %>%
  summarize(Number.Species = length(unique(Species)))

# Get the band numbers with errors
(errors.species <- check.species$Band.Number[check.species$Number.Species > 1])

# Band Numbers with errors: database info matches field datasheets
# 710018209: Asked BC bander to check this issue, waiting on response. 
# 710027442: Banded 2018 as ANHU F age 2, recaptured as BCHU F age 1 in 2022 @ FH
# 710027763: Recap 2020 as ANHU M @ HC, banded 2021 as RUHU M @ MPGF. Mistake in Rec
# 710069503: Banded May/2021 as BCHU F @ CFCK, recap same year and in 2022 band marked as destroyed
# 710069805: Banded Aug/2020 as RUHU M @ FH, recap Mar/2020 as BBLH M @ HC. Mistake in Recapture
# 810005126: Banded Jun/2020 as BCHU F @ ESC, recap Aug/2020 as HYHU @ ESC. Don't know mistake 
# 810008171: Banded 2021 as BCHU M age 1 @ CFCK, then band destroyed in 2022
# 810008256: Band applied twice Jul/14/21 as RUHU F @ WCAT and Jul/31/21 as BCHU F @ REDC. Don't know mistake
# 810008431: Banded in 2021 @ REDC as BCHU F age 1, then band destroyed in 2022
# 810008438: Banded in 2021 as RUHU F age 1, then recaptured in 2022 as BCHU F age 1 @ ESC

# To remove the band numbers with inconsistencies use:
# new_data <- filter(new_data, !Band.Number %in% errors_species)
# This code removes 21 records from the 10 band numbers with mistakes   
new.data <- filter(new.data, !Band.Number %in% errors.species)

# Is each band number associated with a single sex?
check.sex <- new.data %>% 
  filter(Band.Number != !is.na(new.data$Band.Number))%>%
  group_by(Band.Number) %>%
  summarize(Sex = length(unique(Sex)))

# Get the band numbers with errors
(errors.sex <- check.sex$Band.Number[check.sex$Sex > 1])

# Band Numbers with errors:
# 310089627: VCHU individual not in breeding condition when recaptured in 2021
# If working with VCHU data consider reviewing the sex column and reading comments
# for all VCHU records

# Has age been recorded correctly? 

# First: Check for grooves and age. Are they correct? 
# Grooves are recorder as 1, 2 or 3 which represent the % of grooves on the bird's bill
# Age 1(After Hatch Year) should have grooves 0

# Check the following question for all three grooves values

# If grooves are greater than 0, is the age 2 (Hatch Year)? 

grooves.question1 <- new.data[which(new.data$Grooves == "1" & new.data$Age == "1"),]
# There are 141 records with age 1 and grooves 1

grooves.question2 <- new.data[which(new.data$Grooves == "2" & new.data$Age == "1"),]
# There are 4 records with age 1 and grooves 2

grooves.question3 <- new.data[which(new.data$Grooves == "3" & new.data$Age == "1"),]
# There are 5 records with age 1 and grooves 3

grooves.question4 <- new.data[which(new.data$Grooves == '0' & new.data$Age == '2'),]
# There are 5929 records with age 2 and grooves 0
# This is not uncommon as hatch-year birds can loose their grooves fast and keep
# other juvenile traits in plumage

# Combine the results for the first three questions 
# Leaving question four out
df.list <- list(grooves.question1, grooves.question2, grooves.question3)
grooves.errors <- rbindlist(df.list)

# How many are BTLH
number <- count(grooves.errors, Band.Number)
grooves.errors$Band.Number[grooves.errors$Species == 'BTLH']

# 15 records are BTLH. I need to check them
# 310015943 310015945 310016099 310091505 310091508 410091589 410092224 
# 610034582 610039369 610096818 710028068 710064113 710064123 810026513 
# 910099411

# How many BTLH with errors are in ML, DGS, WCAT, and PCBNM
errors.BTLH.sites <- grooves.errors %>% 
  group_by(Band.Number, Species, Location) %>% 
  filter(Species == 'BTLH',
         Location %in% c('ML', 'WCAT', 'PCBNM', 'DGS'))

# There are 3 bands with errors in these sites 

# Band Numbers that will continue to show errors, but are fine
# 610039369: miss identified 1st capture, then updated when recapture, but kept original values for grooves
# 710028068: recapture two years later, traits miss identified 

# Band numbers I couldn't figure it out
# 910099411: traits of a juvenile, but identified as adult. Banded in 2021, recaptured same year

# I need to look into all records with errors to try to fix them, but for now, 
# all BTLH bands for the thesis are good to go

# Second: Has age been assigned correctly to individuals in the same year?

age.check <- new.data %>% 
  group_by(Band.Number, Year) %>% 
  summarize(Age.Category = length(unique(Age))) 

# Get band numbers with errors 
age.check$Band.Number[age.check$Age.Category > 1]

# Band numbers with errors are:
# 710066220  ANHU F caught same year first age 1 recapture age 2 2020 HC by Susan
# 710069375  ANHU F caught same year first age 1 recapture age 2 2020 HC by Susan and Birget 
# None BTLH, so for now we are good to go. I need to check these errors to update the database

# -------------------- CREATE CAPTURE HISTORIES FOR BTLH --------------------- # 

# Select BTLH data for sites that follow HMN's protocol, sex are male and female,
# and sites for thesis 
BTLH.thesis <- new.data %>% 
  filter(Species == "BTLH", 
         Protocol == "HMN",
         Sex != "U", # Removes 4 individuals with unknown sex. These haven't been recaptured
         Location %in% c('ML', 'WCAT', 'PCBNM', 'DGS'),
         Band.Number != "NA", # Removes 2 unbanded individuals
         Band.Number != '810051818') # Removes individual without age, captured once in 2022 

# Create capture history for all Mount Lemmon Data, for all years
# ch is for capture history
ch.ML <- BTLH.thesis %>% 
  arrange(Band.Number) %>% 
  select(Location, Month, Band.Number, Year, Sex) %>% 
  filter(Location == "ML",
         Year %in% 2002:2022, 
         Month %in% 5:7) %>%  # I need to think this better, ideally I'll use dates starting mid-May 
  group_by(Band.Number, Year, Sex) %>%  
  summarize(N.observation = length(Year))%>%
  mutate(Observed = 1) %>% 
  pivot_wider(names_from = Year, values_from = Observed, id_cols = c(Band.Number, Sex), 
              values_fill = 0) %>% 
  mutate('2020' = ".", '2004' = ".") %>%  # Create column for missing years and fill it with a dot
  relocate(Band.Number, '2002','2003','2004','2005','2006','2007','2008','2009',
           '2010','2011','2012','2013','2014','2015','2016','2017','2018','2019',
           '2020','2021','2022', Sex) %>% 
  unite(cap.his, c('2002','2003','2004','2005','2006','2007','2008','2009','2010',
                   '2011','2012','2013','2014','2015','2016','2017','2018','2019',
                   '2020','2021','2022'), sep = '')

# Create capture history for all Dunton Guard Station Data, for all years
# ch is for capture history
ch.DGS <- BTLH.thesis %>% 
  arrange(Band.Number) %>% 
  select(Location, Month, Band.Number, Year, Sex) %>% 
  filter(Location == "DGS",
         Year %in% 2008:2022, 
         Month %in% 5:7) %>%  # I need to think this better, ideally I'll use dates starting mid-May 
  group_by(Band.Number, Year, Sex) %>%  
  summarize(N.observation = length(Year))%>%
  mutate(Observed = 1) %>% 
  pivot_wider(names_from = Year, values_from = Observed, id_cols = c(Band.Number, Sex), 
              values_fill = 0) %>% 
  relocate(Band.Number, '2008','2009','2010','2011','2012','2013','2014','2015',
           '2016','2017','2018','2019','2020','2021','2022', Sex) %>% 
  unite(cap.his, c('2008','2009','2010','2011','2012','2013','2014','2015','2016',
                   '2017','2018','2019','2020','2021','2022'), sep = '')

# Create capture history for all Wildcat Rest Area data, for all years
# ch is for capture history
ch.WCAT <- BTLH.thesis %>% 
  arrange(Band.Number) %>% 
  select(Location, Month, Band.Number, Year, Sex) %>% 
  filter(Location == "WCAT",
         Year %in% 2014:2022, 
         Month %in% 5:7) %>% # I need to think this better, ideally I'll use dates starting mid-May 
  group_by(Band.Number, Year, Sex) %>%  
  summarize(N.observation = length(Year))%>%
  mutate(Observed = 1) %>% 
  pivot_wider(names_from = Year, values_from = Observed, id_cols = c(Band.Number, Sex), 
              values_fill = 0) %>% 
  relocate(Band.Number, '2014','2015','2016','2017','2018','2019','2020','2021',
           '2022', Sex) %>% 
  unite(cap.his, c('2014','2015','2016','2017','2018','2019','2020','2021','2022'), 
        sep = '')

# Create capture history for all Bandelier National Monument data, for all years
# ch is for capture history
ch.PCBNM <- BTLH.thesis %>% 
  arrange(Band.Number) %>% 
  select(Location, Month, Band.Number, Year, Sex) %>% 
  filter(Location == "PCBNM",
         Year %in% 2016:2021, # 2015 doesn't have data for months 5:7 
         Month %in% 5:7)%>%  # I need to think this better, ideally I'll use dates starting mid-May 
  group_by(Band.Number, Year, Sex) %>%  
  summarize(N.observation = length(Year))%>%
  mutate(Observed = 1) %>% 
  pivot_wider(names_from = Year, values_from = Observed, id_cols = c(Band.Number, Sex), 
              values_fill = 0)%>% 
  relocate(Band.Number, '2016','2017','2018','2019','2020','2021', Sex) %>% 
  unite(cap.his, c('2016','2017','2018','2019','2020','2021'), sep = '')

# --------------------------- SKIP THIS PART ------------------------------- # 

# This is the first time I tried the code to create a capture history.  
# Select one site (ML), three years (2005-2007) and data for May to July for
# each year
test.dat <- BTLH.thesis %>% 
  arrange(Band.Number)%>% 
  select(Location, Month, Band.Number, Year, Sex, Age) %>%
  filter(Location == "ML",
         Year %in% 2005:2007, 
         Month %in% 5:7) %>%  
  group_by(Band.Number, Year, Sex, Age) %>%  
  summarize(N.observation = length(Year))%>% # Gets number of observations per each year
  mutate(Observed = 1) %>% # New column with value 1 for observed that year 
  pivot_wider(names_from = Year, values_from = Observed, id_cols = c(Band.Number, Sex), 
              values_fill = 0) %>% 
  relocate(Band.Number, '2005','2006','2007', Sex) %>% 
  unite(cap.his, c('2005','2006','2007'), sep = '') 

# IT WORKS! :) 

# --------------------------------------------------------------------------- #

# Add 'age at first capture' to the capture history 

# 'age at first capture' is the age an individual was assigned the first time it
# was trapped 

# Extract the age of the individuals at first capture 
# Sort data by band number
BTLH.thesis <- BTLH.thesis %>% 
  arrange(Band.Number, Date)

# How many unique band numbers are there?
length(unique(BTLH.thesis[['Band.Number']])) # There are 6888 unique band numbers

# Extract the rows that equal a band number 
BTLH.unique.bands <- unique(BTLH.thesis$Band.Number) 

# Create a new row for 'age at first capture'
BTLH.thesis$first.age <- NA

# Fill in 'age at first capture' with a for loop
for (BN in BTLH.unique.bands) {
    BTLH.thesis$first.age <- BTLH.thesis %>% 
      filter(Band.Number == BN) %>% 
      select(Age) # Would this select the first age because the data set is sorted by date?
}

# I think the problem is that I am trying to use two data sets with different 
# number of rows
# Maybe I need to use the ifelse function instead? 





 







