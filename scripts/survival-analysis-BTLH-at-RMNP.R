# Survival analysis for Broad-tailed Hummingbird at Rocky Mountain National Park
# Gaby Samaniego
# gabysamaniego@arizona.edu
# 2024-06-12

# Load packages
library(tidyverse)
library(RMark)

# Clear environment
rm(list = ls()) 

# Load data
dat <- read.csv('output/cleaned-capture-data-RMNP.csv')

# Delete X column created in the csv file and remove data for year 2012 because
# we still don't have the recapture data for this year. I asked Fred for it
dat1 <- dat %>% 
  select(-X) %>% 
  filter(year != 2012)

# Our data has a bunch of NA values in column age because most of the recapture
# data did not include the age of an individual at a recapture event. 
# The relevant information regarding age is 'age at first capture' for all 
# individuals, so we need to add this column to the data set

# Create data frame to add age at first capture for all captures 
dat1 <- dat1 %>%
  arrange(UBI_band, year) %>%
  group_by(UBI_band) %>%
  mutate(age_fc = age[1]) %>% # [1] automatically takes the age at first capture
  ungroup() %>%
  data.frame()

# Checks:
bandcheck <- unique(dat1$UBI_band[dat1$age_fc == 'HY' & 
                                   dat1$band_status == "R"])
# 295 individuals captured multiple times, first as juveniles
dat1[dat1$UBI_band == bandcheck[5],]
dat1[dat1$UBI_band == bandcheck[200],]

# -------------------- CREATE CAPTURE HISTORIES FOR BTLH --------------------- # 

# -------------- Capture histories without age at first capture -------------- #
# --------------------- includes just adult individuals ---------------------- #

# Create capture history 
ch.adults <- dat1 %>% 
  filter(age_fc == 'AHY') %>%   
  group_by(UBI_band, year, sex) %>%  
  summarize(N.observation = length(year))%>%
  mutate(Observed = 1) %>% 
  pivot_wider(names_from = year, 
              values_from = Observed, 
              id_cols = c(UBI_band, sex), 
              values_fill = 0) %>% 
  relocate(UBI_band, '2003','2004','2005','2006','2007','2008','2009',
           '2010','2011', sex) %>% 
  unite(cap.his, c('2003','2004','2005','2006','2007','2008','2009','2010',
                   '2011'), sep = '') %>% 
  as.data.frame()

# ------------------ Capture histories with age at first capture ------------- #
# ------------------------ includes juveniles and adults --------------------- #

# Create capture history 
ch.with.age <- dat1 %>%
  group_by(UBI_band, year, sex, age_fc) %>%  
  summarize(N.observation = length(year))%>%
  mutate(Observed = 1) %>% 
  pivot_wider(names_from = year, 
              values_from = Observed, 
              id_cols = c(UBI_band, sex, age_fc), 
              values_fill = 0) %>% 
  relocate(UBI_band, '2003','2004','2005','2006','2007','2008','2009',
           '2010','2011', sex, age_fc) %>% 
  unite(cap.his, c('2003','2004','2005','2006','2007','2008','2009','2010', '2011'), 
        sep = '') %>% 
  as.data.frame()

