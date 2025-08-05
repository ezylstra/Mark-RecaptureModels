# Calculate banding effort at RMNP
# Gaby Samaniego
# gabysamaniego@arizona.edu
# 2023-12-27

# Load packages
library(tidyverse)
library(data.table)
library(janitor)
library(lubridate)

# Clear environment
rm(list = ls())

# Read all csv files to merge
# Create a list of all files to merge
effort_files <- list.files(path = "data/RMNP-trap-effort//", 
                           pattern = "*.csv", 
                           full.names = TRUE)
effort_dat <- lapply(effort_files, fread, sep = ",")  

# Combine all csv files in a data frame
effort_dat <- rbindlist(effort_dat, fill = TRUE)

# Replace space in column names with '_' and sort column site
effort_dat <- effort_dat %>% 
  clean_names() %>% 
  arrange(site)

# Explore data
unique(effort_dat$year)
unique(effort_dat$month)
unique(effort_dat$site)
unique(effort_dat$active_days)
unique(effort_dat$trap_hours)

# Fix site's name
effort_dat$site[effort_dat$site == 'GNMNT'] <- 'GNMTN'
effort_dat$site[effort_dat$site == 'NFCP'] <- 'NFPC'
effort_dat$site[effort_dat$site == 'BIGM'] <- 'BGMD'
effort_dat$site[effort_dat$site == 'HPK3'] <- 'HPK2'
# According to Fred's reports HPK2 and HPK3 have the same coordinates 

# Format column trap_hour to calculate effort

# 1) Separate hours from minutes when format is hh:mm
effort_dat <- separate(effort_dat,
                       col = trap_hours, 
                       into = c('hours', 'minutes'),
                       sep = ':',
                       remove = FALSE)

# 2) Change columns minutes and hours from character to numeric
effort_dat$minutes <- as.numeric(effort_dat$minutes)
effort_dat$hours <- as.numeric(effort_dat$hours)

# 3) Change trap_hours from format hh:mm to h.m 
effort_dat <- effort_dat %>%
  mutate(minutes_dec = minutes/60) %>% 
  mutate(across('minutes_dec', round, 1)) %>% 
  mutate(effort = hours + minutes_dec) %>% 
  mutate(effort_final = ifelse(is.na(effort), trap_hours, effort)) %>% 
  select(-c(trap_hours, hours, minutes, minutes_dec, effort)) %>% 
  rename(trap_hours = effort_final)

# 4) Change column trap_hours from character to numeric
effort_dat$trap_hours <- as.numeric(effort_dat$trap_hours)

# Summarize effort data per site
effort <- effort_dat %>%
  filter(month != 9, # exclude September
         !site %in% c('CLP', 'BGMD', 'WB2','WB1', 'WPK1', 'NFPC', 'POLC', 'SHIP')) %>% 
  group_by(year, site) %>% 
  summarize(total_banding_days = sum(active_days),
            average_banding_days = mean(active_days),
            total_trap_hours = sum(trap_hours),
            average_trap_hours = mean(trap_hours)) %>% 
  mutate(across(c('average_trap_hours','average_banding_days'), round, 1)) 

# Export csv of data frame with effort by year
write.csv(effort, 'output/banding-effort-data/banding-effort-all-sites-RMNP.csv',
          row.names = FALSE)
