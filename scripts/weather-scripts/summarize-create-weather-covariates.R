# Create weather covariates for survival analysis
# Gaby Samaniego
# gabysamaniego@arizona.edu
# 2025-01-06

# Load packages
library(tidyverse)

# Clear environment
rm(list = ls())

# --------------------------------- Load data -------------------------------- #

# Load daymet data
daymet <- read.csv('output/weather-data/cleaned-daymet-data-all-sites.csv') %>% 
  select(-X)

# dayl = day length s/day
# prcp = precipitation mm/day
# srad = shortwave radiation W/m2
# swe = snow water equivalent kg/m2
# tmax = maximum air temperature C
# tmin = minimum air temperature C
# vp = water vapor pressure Pa 

# Load MODIS data
ndvi <- read.csv('output/weather-data/cleaned-ndvi-data-all-sites.csv')

# Load thinned winter sites
winter.sites <- read.csv('data/sites-BTLH-range-map-dem/thinned-winter-sites.csv')

# Load summer sites
summer.sites <- read.csv('data/sites-BTLH-range-map-dem/RMNP-sites-data.csv')

# --------------------------- Winter Covariates ------------------------------ # 

# Prepare daymet data set to analyze it for winter months and winter periods 
daymet.winter <- daymet %>%
  filter(month %in% c(12, 1, 2),
         site %in% winter.sites$code) %>% 
  mutate(year_plus = ifelse(month == 12, year+1, year),
         year_less = year_plus-1) %>% 
  unite(winter_period, c(year_less, year_plus), sep = '-', remove = T) %>% 
  filter(!winter_period %in% c('2001-2002', '2012-2013'))

# 1) Overall Minimum temperature

# Extract minimum temperature over each site and winter period
min.temp <- daymet.winter %>% 
  select(site, tmin, winter_period) %>% 
  group_by(site, winter_period) %>% 
  summarise(min_temp = min(tmin))
summary(min.temp)

# Average across sites to get an annual value for the wintering grounds
aver.min.temp <- min.temp %>% 
  group_by(winter_period) %>% 
  summarise(ave_min_temp = round(mean(min_temp), 2)) 

# 2) Average of daily minimum temperature

# Average daily minimum temperature over each site and winter period
site.aver.daily.min.temp <- daymet.winter %>% 
  select(site, tmin, winter_period) %>% 
  group_by(site, winter_period) %>% 
  summarise(aver_min_temp = round(mean(tmin), 2))
summary(site.aver.daily.min.temp)

# Average across sites to get an annual value for the wintering grounds
winter.aver.daily.min.temp <- site.aver.daily.min.temp %>% 
  group_by(winter_period) %>% 
  summarise(aver_daily_min_temp = round(mean(aver_min_temp), 2)) 

# 3) Number of days when the mean temperature was equal or under threshold 
# temperature

# Calculate daily mean temperature
mean.daily.temp <- daymet.winter %>% 
  select(site, date, tmax, tmin, winter_period) %>% 
  mutate(tmean = round((tmax + tmin) / 2, 2))  #Daily mean temperature
summary(mean.daily.temp)

# Define threshold
threshold <- 10
# In laboratory broad-tailed hummingbirds were unable to maintain body mass when 
# kept at 10°C regardless of diet quality, suggesting a physiological bottleneck 
# with regard to energy intake (McWhorter and Martinez del Rio 2000). From Graham 
# paper


# Count the number of cold days under threshold and average per winter period
cold.days <- mean.daily.temp %>% 
  mutate(cold_day = if_else(tmin <= threshold, 1, 0)) %>%  
  group_by(site, winter_period) %>%
  summarize(num_cold_days = sum(cold_day)) %>% 
  group_by(winter_period) %>%
  summarize(mean_cold_days = round(mean(num_cold_days), 0))

# Results from summarizing winter data 

# Create data set with min temp, aver min temp and cold days per winter period 
winter.full <- aver.min.temp %>% 
  left_join(winter.aver.daily.min.temp, by = 'winter_period') %>% 
  left_join(cold.days, by = 'winter_period')

# What is the correlation between the two temperature measures?
cor.test(winter.full$ave_min_temp, winter.full$aver_daily_min_temp)
# 0.53
# Medium positive correlation

# Plot temperature covariates

# Cold days
p1 <- ggplot(winter.full, aes(winter_period, mean_cold_days)) +
  geom_bar(stat = 'identity', fill = 'powderblue') +
  labs(x = 'Winter Period', y = 'Cold Days (mean)') + 
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = 'none',
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 

p1 + 
  geom_line(aes(winter_period, ave_min_temp * 10, 
                color = 'Min Temperature (mean)'), group = 1, size = 1) +
  geom_line(aes(winter_period, aver_daily_min_temp * 10, 
                color = 'Daily Min Temperature (mean)'),
            group = 1, size = 1) +
  labs(color = NULL) +
  scale_y_continuous(sec.axis = sec_axis(~. / 10,
                                         name = 'Temperature (°C)')) +  
  scale_color_manual(values = c('Min Temperature (mean)' = 'blue4', 
               'Daily Min Temperature (mean)' = 'blue')) + 
 theme(panel.grid = element_blank(),
       legend.position = 'top')


# --------------------------- Summer Covariates ------------------------------ # 
