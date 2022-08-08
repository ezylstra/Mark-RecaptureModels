# Clean HMN's main database before using it for anything 
# Gaby Samaniego
# gaby@savehummingbirds.org
# 2022-06-08

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

# To do! 
# Ask Susan about the columns to remove or update/change, then complete the code
# below 

# Do we need data for session, week, day of year, day block in raw data???? 
# If needed for analysis we can get this information with the date 

# Remove columns
band_data <- band_data %>% 
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
         -Field_BAND.SIZE)    

##### Change latitude, longitude and elevation #####

# To do!
# Update site data
# Join banding data with site data


##### Split columns that contain two variables #####

# To do! 
# Tarsus.Measurement and Leg.Condition
# Buffy and % Green on Back for RUHU

##### Cambiar los datos de fotos, muestras, etc, a los campos nuevos definidos
##### en el formulario nuevo de KoBo

# Actualizar la base de datos en Excel con los cambios 

##### Change column's names #####
str(band_data)

band_data <- band_data %>% 
  rename(Bander = Initials.Bdr,
         Region = region,
         Session = session,
         Week = week,
         Day.Of.Year = dayofyear,
         Day.Block = DayBlock,
         Date = date,
         Year = year,
         Month = mo,
         Day = day,
         Time = time,
         Old.Band.Status = OldBand.Status,
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

##### Band Numbers #####

# If any 'XXXXXX' in Band.Number, replace it with NA 
band_data <- band_data %>% 
  mutate(Band.Number = na_if(Band.Number, "XXXXXX"))

##### Band Status ##### 

# 


##### Format columns #####

# To do! Complete the code once the changes to each columns are done  

# Change date column from character to date
band_data <- band_data %>% 
  mutate(Date = mdy(Date))


# Change band number from character to numeric
band_data$Band.Number <- as.numeric(as.character((band_data$Band.Number)))

#####        #####

# Sort data by band number, date, species and sex
band_data <- band_data %>% 
  arrange(Band.Number, Date, Species, Sex)

# Verify that first use (date) of a band number corresponds to band status 1
# and following captures correspond to recaptures or band status R 

# Extract the rows that equal first capture and recaptures 
new__recap_bands <- subset(band_data, Band.Status %in% c("1","R"))

# Extract the rows that equal a band number 
new_bands <- unique(new__recap_bands$Band.Number)
new__recap_bands$best_band_status <- NA
unique(new__recap_bands$Band.Status)

# Create a capture number column 
new__recap_bands$capture_number <- sequence(from = 1, rle(new__recap_bands$Band.Number)$lengths)

new__recap_bands$best_band_status <- ifelse(new__recap_bands$capture_number == 1, "1","R")

# Subset other band status (4, 6, 8, F)
other_band_status <- band_data %>% 
  filter(Band.Status %in% c("4", "6", "8", "F"))

# merge all band status
all_bands <- other_band_status %>% 
  bind_rows(new__recap_bands) %>% 
  arrange(Band.Number, Date)

# Reorganize data frame 
new_data <- all_bands %>% 
  select(-capture_number) 

count(new_data, Band.Status)
count(new_data, best_band_status)








