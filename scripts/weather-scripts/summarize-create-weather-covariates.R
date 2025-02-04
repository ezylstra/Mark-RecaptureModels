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
daymet <- read.csv('output/weather-data/cleaned-daymet-data-all-sites.csv')

# dayl = day length s/day
# prcp = precipitation mm/day
# srad = shortwave radiation W/m2
# swe = snow water equivalent kg/m2
# tmax = maximum air temperature C
# tmin = minimum air temperature C
# vp = water vapor pressure Pa 

# Load MODIS data
modis <- read.csv('output/weather-data/cleaned-ndvi-data-all-sites-with-quality-flags.csv')

# Load thinned winter sites
winter.sites <- read.csv('data/sites-BTLH-range-map-dem/thinned-winter-sites.csv')

# Load summer sites
summer.sites <- read.csv('data/sites-BTLH-range-map-dem/thinned-summer-sites.csv')

# --------------------------- WINTER COVARIATES ------------------------------ # 

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

# 3) Number of days when the daily mean temperature was equal or under  a 
# threshold temperature

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

# Results from summarizing temperature data 

# Create data set with min temp, aver min temp and cold days per winter period 
temp.full <- aver.min.temp %>% 
  left_join(winter.aver.daily.min.temp, by = 'winter_period') %>% 
  left_join(cold.days, by = 'winter_period')

# What is the correlation between the two temperature measures?
cor.test(temp.full$ave_min_temp, temp.full$aver_daily_min_temp)
# 0.53
# Medium positive correlation

# Plot temperature covariates

# Cold days
p1 <- ggplot(temp.full, aes(winter_period, mean_cold_days)) +
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

# 4) Precipitation

# Total precipitation over each site and winter period
site.total.prcp <- daymet.winter %>% 
  select(site, prcp, winter_period) %>% 
  group_by(site, winter_period) %>% 
  summarise(total_prcp = sum(prcp))
summary(site.total.prcp)

# Average across sites to get an annual value for the wintering grounds
winter.total.prcp <- site.total.prcp %>% 
  group_by(winter_period) %>% 
  summarise(total_prcp = sum(total_prcp)) 

# Plot precipitation 
ggplot(winter.total.prcp, aes(winter_period, total_prcp)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = 'Winter Period', y = 'Total Precipitation (mm)') + 
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = 'none',
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# 5) NDVI

# Prepare MODIS data set to analyze it for winter months and winter periods 
modis.winter <- modis %>%
  mutate(month = month(calendar_date),
         year = year(calendar_date)) %>% 
  filter(month %in% c(12, 1, 2),
         site_name %in% winter.sites$code) %>% 
  mutate(year_plus = ifelse(month == 12, year+1, year),
         year_less = year_plus-1) %>% 
  unite(winter_period, c(year_less, year_plus), sep = '-', remove = T) %>% 
  filter(!winter_period %in% c('2001-2002', '2012-2013'))

# Filter modis data using quality flags
# Using Pixel Reliability Flag = overall pixel quality
# Rank key    Summary QA          Description
#   -1       Fill/No Data     Not Processed
#    0       Good Data        Use with confidence
#    1       Marginal data    Useful, but look at other QA information
#    2       Snow/Ice         Target covered with snow/ice
#    3       Cloudy           Target not visible, covered with cloud

# Using just good data
filtered.ndvi.winter <- modis.winter %>% 
  filter(re_value == 0)
# From 7200 observations to 6586 observations. Removes 614 observations

# Using good and marginal data
filtered.ndvi.winter.0.1 <- modis.winter %>% 
  filter(re_value %in% c(0, 1))
# From 7200 observations to 7086 observations. Removes 114 observations
# There is a tiny difference in the data when using rank key 0 vs c(0,1)
# Decided to use just good data =rank key 0

# Average ndvi values over each site and winter period
site.aver.ndvi <- filtered.ndvi.winter %>% 
  select(site_name, ndvi, winter_period) %>% 
  group_by(site_name, winter_period) %>% 
  summarise(aver_ndvi = round(mean(ndvi), 2))
summary(site.aver.ndvi)

# Average across sites to get an annual value for the wintering grounds
winter.aver.ndvi <- site.aver.ndvi %>% 
  group_by(winter_period) %>% 
  summarise(aver_ndvi = round(mean(aver_ndvi), 2)) 

# Results from summarizing precipitation and ndvi data 

# Create data set with total precip and aver ndvi per winter period 
resources.full <- winter.total.prcp %>% 
  left_join(winter.aver.ndvi, by = 'winter_period')

# What is the correlation between average ndvi and precipitation?
cor.test(winter.total.prcp$total_prcp, 
         winter.aver.ndvi$aver_ndvi)
# 0.66
# Medium positive correlation

# Plot ndvi 
ggplot(winter.aver.ndvi, aes(winter_period, aver_ndvi)) +
  geom_bar(stat = 'identity', fill = 'darkblue') +
  labs(x = 'Winter Period', y = 'Average NDVI') + 
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = 'none',
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# NDVI values go from -1 to 1

# ----------------- Create data set with all winter covariates --------------- #

winter.covariates <- temp.full %>% 
  left_join(resources.full, by = 'winter_period')

# ------------------------------ SUMEER COVARIATES --------------------------- # 



