# Exploring Broad-tailed hummingbird data from Rocky Mountain National Park
# Gaby Samaniego
# gaby@savehummingbirds.org
# 2022-08-23

library(tidyverse)
library(lubridate)
library(stringr)

##### Data wrangling #####

# Bring in raw data
raw_data <- read.csv("data/RMNP_data_new.csv")

# Capitalize all characters and factors across data frame 
# old code: band_data <- mutate_all(raw_data, .funs=toupper)  
band_data <- raw_data %>%
  mutate(across(.funs = toupper)) 

# Remove all leading and trailing white spaces
band_data <- band_data %>% 
  mutate_all(str_trim, side=c("both"))

# Remove unnecessary columns
band_data <- band_data %>% 
  select(-latitude, -longitude, -elevation, -region, -session, -week, -X0, 
         -X0.1, -TArsus.LEG, -Size.Material)    

# Change column's names 
band_data <- band_data %>% 
  rename(Bander = Initials.Bdr,
         Date = DATE,
         Year = year,
         Month = mo,
         Day = day,
         Time = time,
         Band.Number = BAND,
         Sex = SEX,
         Age = AGE)

# Remove - from Band.Number
band_data <- band_data %>%
  (.cols(Band.Number), str_remove_all, "-"))    

band_data %>% 
  mutate(str_remove_all("[-]"))
