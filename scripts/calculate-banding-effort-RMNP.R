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
effort.files <- list.files(path = "data/RMNP-trap-effort//", 
                           pattern = "*.csv", 
                           full.names = TRUE)
effort.dat <- lapply(effort.files, fread, sep = ",")  

# Combine all csv files in a data frame
effort.dat <- rbindlist(effort.dat, fill = TRUE)

# Replace space in column names with '_' and sort column site
effort.dat <- effort.dat %>% 
  clean_names() %>% 
  arrange(site)

# Explore data
unique(effort.dat$year)
unique(effort.dat$month)
unique(effort.dat$site)
unique(effort.dat$active_days)
unique(effort.dat$trap_hours)

# Fix site's name
effort.dat$site[effort.dat$site == 'GNMNT'] <- 'GNMTN'
effort.dat$site[effort.dat$site == 'NFCP'] <- 'NFPC'

# Format column trap_hour to calculate effort

# 1) Separate hours from minutes when format is hh:mm
effort.dat <- separate(effort.dat,
         col = trap_hours, 
         into = c('hours', 'minutes'), 
         sep = ':',
         remove = FALSE)

# 2) Change columns minutes and hours from character to numeric
effort.dat$minutes <- as.numeric(effort.dat$minutes)
effort.dat$hours <- as.numeric(effort.dat$hours)

# 3) Change trap_hours from format hh:mm to h.m 
effort.dat <- effort.dat %>%
  mutate(minutes_dec = minutes/60) %>% 
  mutate(across('minutes_dec', round, 1)) %>% 
  mutate(effort = hours + minutes_dec) %>% 
  mutate(effort_final = ifelse(is.na(effort), trap_hours, effort)) %>% 
  select(-c(trap_hours, hours, minutes, minutes_dec, effort)) %>% 
  rename(trap_hours = effort_final)

# 4) Change column trap_hours from character to numeric
effort.dat$trap_hours <- as.numeric(effort.dat$trap_hours)

# Summarize effort data for analysis 
effort <- effort.dat %>% 
  group_by(site, year) %>% 
  summarize(total_banding_days = sum(active_days),
            average_banding_days = mean(active_days),
            total_trap_hours = sum(trap_hours),
            average_trap_hours = mean(trap_hours)) %>% 
  mutate(across(c('average_trap_hours','average_banding_days'), round, 1)) 

# Once we decide what sites we are using for the analysis, we can filter them. 
# For now, totals and averages have been calculated
effort_covariate <- effort %>% 
  group_by(site) %>% 
  summarize(total_years = length(unique((year))),
            total_banding_days_year = sum(total_banding_days),
            average_banding_days_year = mean(total_banding_days),
            total_trap_hours_year = sum(total_trap_hours),
            average_trap_hours_year = mean(total_trap_hours)) %>% 
  mutate(across(c('average_banding_days_year','average_trap_hours_year'), round, 1))

# Export csv of data frame with effort
write.csv(effort_covariate, 'output/banding-effort-all-sites-RMNP.csv')
