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

# Edit summer sites
# Remove sites that don't have any recapture information and just a few captured
# birds.
summer.sites <- summer.sites %>% 
  filter(!site %in% c('WB1', 'WPK1', 'NFPC', 'POLC', 'SHIP'))

################################################################################

# --------------------------- WINTER COVARIATES ------------------------------ # 
# ------------------------------- In Mexico ---------------------------------- #

# Prepare daymet data set to analyze it for winter months and winter periods in
# Mexico (mx)
daymet.winter.mx <- daymet %>%
  filter(month %in% c(12, 1, 2),
         site %in% winter.sites$code) %>% 
  mutate(year_plus = ifelse(month == 12, year+1, year),
         year_less = year_plus-1) %>% 
  unite(winter_period, c(year_less, year_plus), sep = '-', remove = T) %>% 
  filter(!winter_period %in% c('2001-2002', '2012-2013'))

# 1) Overall Minimum temperature

# Extract minimum temperature over each site and winter period
min.temp.mx <- daymet.winter.mx %>% 
  select(site, tmin, winter_period) %>% 
  group_by(site, winter_period) %>% 
  summarise(min_temp = min(tmin))
summary(min.temp.mx)

# Average across sites to get an annual value for the wintering grounds
aver.min.temp.mx <- min.temp.mx %>% 
  group_by(winter_period) %>% 
  summarise(ave_min_temp = round(mean(min_temp), 2)) 

# 2) Average of daily minimum temperature

# Average daily minimum temperature over each site and winter period
site.aver.daily.min.temp.mx <- daymet.winter.mx %>% 
  select(site, tmin, winter_period) %>% 
  group_by(site, winter_period) %>% 
  summarise(aver_min_temp = round(mean(tmin), 2))
summary(site.aver.daily.min.temp.mx)

# Average across sites to get an annual value for the wintering grounds
winter.aver.daily.min.temp.mx <- site.aver.daily.min.temp.mx %>% 
  group_by(winter_period) %>% 
  summarise(aver_daily_min_temp = round(mean(aver_min_temp), 2)) 

# 3) Number of days when the daily mean temperature was equal or under  a 
# threshold temperature

# Calculate daily mean temperature
winter.mean.daily.temp.mx <- daymet.winter.mx %>% 
  select(site, date, tmax, tmin, winter_period) %>% 
  mutate(tmean = round((tmax + tmin) / 2, 2))  #Daily mean temperature
summary(winter.mean.daily.temp.mx)

# Define threshold
winter.threshold <- 10
# In laboratory broad-tailed hummingbirds were unable to maintain body mass when 
# kept at 10°C regardless of diet quality, suggesting a physiological bottleneck 
# with regard to energy intake (McWhorter and Martinez del Rio 2000). From Graham 
# paper

# Count the number of cold days when the daily mean temperature was equal or under 
# the winter threshold and average per winter period
winter.cold.days.mx <- winter.mean.daily.temp.mx %>% 
  mutate(cold_day = if_else(tmean <= winter.threshold, 1, 0)) %>%  
  group_by(site, winter_period) %>%
  summarize(num_cold_days = sum(cold_day)) %>% 
  group_by(winter_period) %>%
  summarize(mean_cold_days = round(mean(num_cold_days), 0))

# Results from summarizing temperature data 

# Create data set with min temp, aver min temp and cold days per winter period 
temp.full.mx <- aver.min.temp.mx %>% 
  left_join(winter.aver.daily.min.temp.mx, by = 'winter_period') %>% 
  left_join(winter.cold.days.mx, by = 'winter_period')

# What is the correlation between the two temperature measures?
cor.test(temp.full.mx$ave_min_temp, temp.full.mx$aver_daily_min_temp)
# 0.53
# Medium positive correlation

# Plot temperature covariates

# Cold days
p1 <- ggplot(temp.full.mx, aes(winter_period, mean_cold_days)) +
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
winter.site.total.prcp.mx <- daymet.winter.mx %>% 
  select(site, prcp, winter_period) %>% 
  group_by(site, winter_period) %>% 
  summarise(total_prcp = sum(prcp))
summary(winter.site.total.prcp.mx)

# Average across sites to get an annual value for the wintering grounds
winter.total.prcp.mx <- winter.site.total.prcp.mx %>% 
  group_by(winter_period) %>% 
  summarise(total_prcp = sum(total_prcp)) 

# Plot precipitation 
ggplot(winter.total.prcp.mx, aes(winter_period, total_prcp)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = 'Winter Period', y = 'Total Precipitation (mm)') + 
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = 'none',
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# 5) NDVI

# Prepare MODIS data set to analyze it for winter months and winter periods 
modis.winter.mx <- modis %>%
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
filtered.ndvi.winter.mx <- modis.winter.mx %>% 
  filter(re_value == 0)
# From 7200 observations to 6586 observations. Removes 614 observations

# Average ndvi values over each site and winter period
winter.site.aver.ndvi.mx <- filtered.ndvi.winter.mx %>% 
  select(site_name, ndvi, winter_period) %>% 
  group_by(site_name, winter_period) %>% 
  summarise(aver_ndvi = round(mean(ndvi), 2))
summary(winter.site.aver.ndvi.mx)

# Average across sites to get an annual value for the wintering grounds
winter.aver.ndvi.mx <- winter.site.aver.ndvi.mx %>% 
  group_by(winter_period) %>% 
  summarise(aver_ndvi = round(mean(aver_ndvi), 2)) 

# Results from summarizing precipitation and ndvi data 

# Create data set with total precip and aver ndvi per winter period 
resources.full.mx <- winter.total.prcp.mx %>% 
  left_join(winter.aver.ndvi.mx, by = 'winter_period')

# What is the correlation between average ndvi and precipitation?
cor.test(winter.total.prcp.mx$total_prcp, 
         winter.aver.ndvi.mx$aver_ndvi)
# 0.66
# Medium positive correlation

# Plot ndvi 
ggplot(winter.aver.ndvi.mx, aes(winter_period, aver_ndvi)) +
  geom_bar(stat = 'identity', fill = 'lightgreen') +
  labs(x = 'Winter Period', y = 'Average NDVI') + 
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = 'none',
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# ----------------- Create data set with all winter covariates --------------- #

winter.covariates.mx <- temp.full.mx %>% 
  left_join(resources.full.mx, by = 'winter_period')

################################################################################

# ------------------------------ SUMMER COVARIATES --------------------------- # 
# --------------------------------- In Colorado ------------------------------ #

# Prepare daymet data set to analyze it for summer months 
daymet.summer.co <- daymet %>% 
  filter(month %in% c(5, 6, 7, 8),
         site %in% summer.sites$code) %>%
  rename(code = site) %>% 
  left_join(select(summer.sites, code, site, location), by = 'code')

# 1) Overall temperature in summer grounds

# Explore overall mean temperature in the summer grounds
summer.temp.co <- daymet.summer.co %>% 
  select(site, location, year, month, day, tmax, tmin) %>% 
  mutate(tmean = round((tmax + tmin) / 2, 2)) 
summary(summer.temp.co)

# Calculate averages for tmax, tmin and tmean per site and year
aver.summer.temp.co <- summer.temp.co %>% 
  group_by(site, location, year) %>% 
  summarize(aver_tmax = round(mean(tmax), 2),
            aver_tmin = round(mean(tmin), 2),
            aver_tmean = round(mean(tmean), 2))

# Pivot data before creating the plot
pivot.summer.dat.co <- aver.summer.temp.co %>%
  pivot_longer(cols = c(aver_tmin, aver_tmax, aver_tmean), 
               names_to = "temperature_type", 
               values_to = "temperature") %>% 
  arrange(location, site) 

# Convert 'site' into a factor to preserve the order in the plot. The last three
# sites in the plot are those for the west location
pivot.summer.dat.co$site <- factor(pivot.summer.dat.co$site, 
                                levels = unique(pivot.summer.dat.co$site), 
                                ordered = TRUE)

# Plot data by site
ggplot(pivot.summer.dat.co, 
       aes(x = year, y = temperature, 
           color = temperature_type, 
           group = temperature_type)) +
  geom_line(size = 1) +          
  geom_point(size = 2) +         
  facet_wrap(~ site) +  # Site order follows factor levels
  labs(title = 'Temperature Trends by Year and Site',
       x = 'Year',
       y = 'Temperature (°C)',
       color = 'Mean:') +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(pivot.summer.dat.co$year), 
                                  max(pivot.summer.dat.co$year), by = 2)) +
  scale_color_manual(values = c('red3', 'darkgray', 'darkblue'), 
                     labels = c('Tmax', 'Tmean', 'Tmin'))

# Summarize data for east sites

# Calculate averages for tmax, tmin and tmean per site and year in east locations
east.aver.summer.temp <- summer.temp.co %>% 
  filter(location == 'east') %>% 
  group_by(year) %>% 
  summarize(aver_tmax = round(mean(tmax), 2),
            aver_tmin = round(mean(tmin), 2),
            aver_tmean = round(mean(tmean), 2))

# Summarize data for the west sites  

# Calculate averages for tmax, tmin and tmean per site and year in west locations
west.aver.summer.temp <- summer.temp.co %>% 
  filter(location == 'west') %>% 
  group_by(year) %>% 
  summarize(aver_tmax = round(mean(tmax), 2),
            aver_tmin = round(mean(tmin), 2),
            aver_tmean = round(mean(tmean), 2))

# 2) Overall Maximum temperature

# For Summer grounds
# Summer grounds means all 12 sites, no distinction by east and west

# Extract maximum temperature over each site and year 
max.temp.summer.co <- daymet.summer.co %>% 
  select(site, tmax, year) %>% 
  group_by(site, year) %>% 
  summarise(max_temp = max(tmax))
summary(max.temp.summer.co)

# Average across sites to get an annual value for the summer grounds
aver.max.temp.summer.co <- max.temp.summer.co %>% 
  group_by(year) %>% 
  summarise(ave_max_temp = round(mean(max_temp), 2)) 

# For east locations
# Total of 9 sites

# Extract maximum temperature over each site and year 
max.temp.east <- daymet.summer.co %>%
  filter(location == 'east') %>% 
  select(site, tmax, year) %>% 
  group_by(site, year) %>% 
  summarise(max_temp = max(tmax))
summary(max.temp.east)

# Average across sites to get an annual value for the east sites
aver.max.temp.east <- max.temp.east %>% 
  group_by(year) %>% 
  summarise(ave_max_temp = round(mean(max_temp), 2)) 

# For west locations
# Total of 3 sites

# Extract maximum temperature over each site and year 
max.temp.west <- daymet.summer.co %>%
  filter(location == 'west') %>% 
  select(site, tmax, year) %>% 
  group_by(site, year) %>% 
  summarise(max_temp = max(tmax))
summary(max.temp.west)

# Average across sites to get an annual value for the west sites
aver.max.temp.west <- max.temp.west %>% 
  group_by(year) %>% 
  summarise(ave_max_temp = round(mean(max_temp), 2)) %>% 
  print()

# 3) Overall Minimum Temperature

# For Summer grounds

# Extract minimum temperature over each site and year 
min.temp.summer.co <- daymet.summer.co %>% 
  select(site, tmin, year) %>% 
  group_by(site, year) %>% 
  summarise(min_temp = min(tmin))
summary(min.temp.summer.co)

# Average across sites to get an annual value for the summer grounds
aver.min.temp.summer.co <- min.temp.summer.co %>% 
  group_by(year) %>% 
  summarise(ave_min_temp = round(mean(min_temp), 2)) %>% 
  print()

# For east locations

# Extract minimum temperature over each site and year 
min.temp.east <- daymet.summer.co %>%
  filter(location == 'east') %>% 
  select(site, tmin, year) %>% 
  group_by(site, year) %>% 
  summarise(min_temp = min(tmin))
summary(min.temp.east)

# Average across sites to get an annual value for the east sites
aver.min.temp.east <- min.temp.east %>% 
  group_by(year) %>% 
  summarise(ave_min_temp = round(mean(min_temp), 2)) %>% 
  print()

# For west locations

# Extract minimum temperature over each site and year 
min.temp.west <- daymet.summer.co %>%
  filter(location == 'west') %>% 
  select(site, tmin, year) %>% 
  group_by(site, year) %>% 
  summarise(min_temp = min(tmin))
summary(min.temp.west)

# Average across sites to get an annual value for the west sites
aver.min.temp.west <- min.temp.west %>% 
  group_by(year) %>% 
  summarise(ave_min_temp = round(mean(min_temp), 2)) %>% 
  print()

# 4) Average of daily maximum temperature

# For summer grounds 

# Average daily maximum temperature over each site and year
aver.daily.max.temp.summer.co <- daymet.summer.co %>% 
  select(site, tmax, year) %>% 
  group_by(site, year) %>% 
  summarise(aver_max_temp = round(mean(tmax), 2))
summary(aver.daily.max.temp.summer.co)

# Average across sites to get an annual value for the summer grounds
aver.aver.daily.max.temp.summer.co <- aver.daily.max.temp.summer.co %>% 
  group_by(year) %>% 
  summarise(aver_daily_max_temp = round(mean(aver_max_temp), 2)) %>% 
  print()

# For east sites 

# Average daily maximum temperature over each site and year
aver.daily.max.temp.east <- daymet.summer.co %>% 
  filter(location == 'east') %>% 
  select(site, tmax, year) %>% 
  group_by(site, year) %>% 
  summarise(aver_max_temp = round(mean(tmax), 2))
summary(aver.daily.max.temp.east)

# Average across sites to get an annual value
aver.aver.daily.max.temp.east <- aver.daily.max.temp.east %>% 
  group_by(year) %>% 
  summarise(aver_daily_max_temp = round(mean(aver_max_temp), 2)) %>% 
  print()

# For west sites 

# Average daily maximum temperature over each site and year
aver.daily.max.temp.west <- daymet.summer.co %>% 
  filter(location == 'west') %>% 
  select(site, tmax, year) %>% 
  group_by(site, year) %>% 
  summarise(aver_max_temp = round(mean(tmax), 2))
summary(aver.daily.max.temp.west)

# Average across sites to get an annual value
aver.aver.daily.max.temp.west <- aver.daily.max.temp.west %>% 
  group_by(year) %>% 
  summarise(aver_daily_max_temp = round(mean(aver_max_temp), 2)) %>% 
  print()

# 5) Average of daily minimum temperature

# For summer grounds 

# Average daily minimum temperature over each site and year
aver.daily.min.temp.summer.co <- daymet.summer.co %>% 
  select(site, tmin, year) %>% 
  group_by(site, year) %>% 
  summarise(aver_min_temp = round(mean(tmin), 2))
summary(aver.daily.min.temp.summer.co)

# Average across sites to get an annual value for the summer grounds
aver.aver.daily.min.temp.summer.co <- aver.daily.min.temp.summer.co %>% 
  group_by(year) %>% 
  summarise(aver_daily_min_temp = round(mean(aver_min_temp), 2)) %>% 
  print()

# 6) Number of days when the daily mean temperature was equal or higher  a 
# threshold temperature

# Define threshold
summer.threshold.co <- 20
# Keep looking for a citation. Maybe this is not a good covariate?

# With 30 degrees the mean days was zero for all sites and years
# With 20 degrees there were still many zeros and a max of 5 days fo one year

# For summer grounds 

# Calculate daily mean temperature
mean.daily.temp.summer.co <- daymet.summer.co %>% 
  select(site, date, tmax, tmin, year) %>% 
  mutate(tmean = round((tmax + tmin) / 2, 2))  #Daily mean temperature
summary(mean.daily.temp.summer.co)

# Average of tmean per site and year
aver.mean.temp.summer.co <- mean.daily.temp.summer.co %>% 
  group_by(year) %>% 
  summarize(aver_daily_mean_temp = round(mean(tmean),2)) %>% 
  print()

# Count the number of warm days above threshold and average per summer period
warm.days.summer.co <- mean.daily.temp.summer.co %>% 
  mutate(warm_day = if_else(tmean >= summer.threshold.co, 1, 0)) %>%  
  group_by(site, year) %>%
  summarize(num_warm_days = sum(warm_day)) %>% 
  group_by(year) %>%
  summarize(mean_warm_days = round(mean(num_warm_days), 0)) %>% 
  print()

# For east sites

# Calculate daily mean temperature
mean.daily.temp.east <- daymet.summer.co %>%
  filter(location == 'east') %>% 
  select(site, date, tmax, tmin, year) %>% 
  mutate(tmean = round((tmax + tmin) / 2, 2))  #Daily mean temperature
summary(mean.daily.temp.east)

# Average of tmean per site and year
aver.mean.daily.temp.east <- mean.daily.temp.east %>% 
  group_by(year) %>% 
  summarize(aver_daily_mean_temp = round(mean(tmean),2)) %>% 
  print()

# days.....

# For east sites

# Calculate daily mean temperature
mean.daily.temp.west <- daymet.summer.co %>%
  filter(location == 'west') %>% 
  select(site, date, tmax, tmin, year) %>% 
  mutate(tmean = round((tmax + tmin) / 2, 2))  #Daily mean temperature
summary(mean.daily.temp.west)

# Average of tmean per site and year
aver.mean.daily.temp.west <- mean.daily.temp.west %>% 
  group_by(year) %>% 
  summarize(aver_daily_mean_temp = round(mean(tmean),2)) %>% 
  print()

# days.....

# What about cold days with same threshold we are using in winter in MX?

# For summer grounds 

# Calculate daily mean temperature
mean.daily.temp.summer.co <- daymet.summer.co %>% 
  select(site, date, tmax, tmin, year) %>% 
  mutate(tmean = round((tmax + tmin) / 2, 2))  #Daily mean temperature
summary(mean.daily.temp.summer.co)

# Average of tmean per site and year
aver.mean.temp.summer.co <- mean.daily.temp.summer.co %>% 
  group_by(year) %>% 
  summarize(aver_daily_mean_temp = round(mean(tmean),2)) %>% 
  print()

# Count the number of cold days equal or below winter threshold and average per 
# year
cold.days.summer.co <- mean.daily.temp.summer.co %>% 
  mutate(cold_day = if_else(tmean <= winter.threshold, 1, 0)) %>%  
  group_by(site, year) %>%
  summarize(num_cold_days = sum(cold_day)) %>% 
  group_by(year) %>%
  summarize(mean_cold_days = round(mean(num_cold_days), 0)) %>% 
  print()


# Results from summarizing temperature data for summer grounds
temp.full.summer.co <- aver.max.temp.summer.co %>% 
  left_join(aver.min.temp.summer.co, by = 'year') %>% 
  left_join(aver.max.temp.summer.co, by = 'year') %>% 
  left_join(aver.aver.daily.max.temp.summer.co, by = 'year') %>% 
  left_join(aver.aver.daily.min.temp.summer.co, by = 'year') %>% 
  left_join(aver.mean.temp.summer.co, by = 'year') %>% 
  left_join(cold.days.summer.co, by = 'year')

# Results from summarizing temperature data for east sites
temp.full.east <- aver.max.temp.east %>% 
  left_join(aver.min.temp.east, by = 'year') %>% 
  left_join(aver.aver.daily.max.temp.east, by = 'year') %>% 
  left_join(aver.mean.daily.temp.east, by = 'year')

# Results from summarizing temperature data for west sites
temp.full.west <- aver.max.temp.west %>% 
  left_join(aver.min.temp.west, by = 'year') %>% 
  left_join(aver.aver.daily.max.temp.west, by = 'year') %>% 
  left_join(aver.mean.daily.temp.west, by = 'year')

# 4) Precipitation

# For summer grounds

# Total precipitation over each site and summer years
summer.total.prcp.co <- daymet.summer.co %>% 
  select(site, prcp, year) %>% 
  group_by(site, year) %>% 
  summarise(total_prcp = sum(prcp))
summary(summer.total.prcp.co)

# Average across sites to get an annual value for the summer grounds
summer.total.prcp.co <- summer.total.prcp.co %>% 
  group_by(year) %>% 
  summarise(total_prcp = sum(total_prcp)) 

# Plot precipitation 
ggplot(summer.total.prcp.co, aes(factor(year), total_prcp)) +
  geom_bar(stat = 'identity', fill = 'steelblue') +
  labs(x = 'Year', y = 'Total Precipitation (mm)') + 
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = 'none',
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# For east sites

# Total precipitation over each site and summer years
summer.total.prcp.east <- daymet.summer.co %>%
  filter(location =='east') %>% 
  select(site, prcp, year) %>% 
  group_by(site, year) %>% 
  summarise(total_prcp = sum(prcp))
summary(summer.total.prcp.east)

# Average across sites to get an annual value for the summer grounds
summer.total.prcp.east <- summer.total.prcp.east %>% 
  group_by(year) %>% 
  summarise(total_prcp = sum(total_prcp)) 

# For west sites

# Total precipitation over each site and summer years
summer.total.prcp.west <- daymet.summer.co %>%
  filter(location == 'west') %>% 
  select(site, prcp, year) %>% 
  group_by(site, year) %>% 
  summarise(total_prcp = sum(prcp))
summary(summer.total.prcp.west)

# Average across sites to get an annual value for the summer grounds
summer.total.prcp.west <- summer.total.prcp.west %>% 
  group_by(year) %>% 
  summarise(total_prcp = sum(total_prcp)) 

# 5) NDVI

# For summer grounds

# Prepare MODIS data set to analyze it for summer months and year 
modis.summer.co <- modis %>%
  mutate(month = month(calendar_date),
         year = year(calendar_date)) %>% 
  filter(month %in% c(5, 6, 7, 8),
         site_name %in% summer.sites$code) 

# Filter modis data using quality flags
# Using Pixel Reliability Flag = overall pixel quality
# Rank key    Summary QA          Description
#   -1       Fill/No Data     Not Processed
#    0       Good Data        Use with confidence
#    1       Marginal data    Useful, but look at other QA information
#    2       Snow/Ice         Target covered with snow/ice
#    3       Cloudy           Target not visible, covered with cloud

filtered.ndvi.summer.co <- modis.summer.co %>% 
  filter(re_value == 0)
# From 1656 observations to 842 observations. Removes 814 observations

# Average ndvi values over each site and year
summer.aver.ndvi.co <- filtered.ndvi.summer.co %>% 
  select(site_name, ndvi, year) %>% 
  group_by(site_name, year) %>% 
  summarise(aver_ndvi = round(mean(ndvi), 2))
summary(summer.aver.ndvi.co)

# Average across sites to get an annual value for the summer grounds
summer.aver.ndvi.co <- summer.aver.ndvi.co %>% 
  group_by(year) %>% 
  summarise(aver_ndvi = round(mean(aver_ndvi), 2)) 

# For east sites 

# Prepare MODIS data set to analyze it for summer months, year and east sites 
modis.east <- modis %>%
  mutate(month = month(calendar_date),
         year = year(calendar_date)) %>% 
  filter(month %in% c(5, 6, 7, 8),
         site_name %in% summer.sites$code) 

# Filter modis data using quality flags
# Using Pixel Reliability Flag = overall pixel quality
# Rank key    Summary QA          Description
#   -1       Fill/No Data     Not Processed
#    0       Good Data        Use with confidence
#    1       Marginal data    Useful, but look at other QA information
#    2       Snow/Ice         Target covered with snow/ice
#    3       Cloudy           Target not visible, covered with cloud

filtered.ndvi.summer.co <- modis.summer.co %>% 
  filter(re_value == 0)
# From 1656 observations to 842 observations. Removes 814 observations

# Average ndvi values over each site and year
summer.aver.ndvi.co <- filtered.ndvi.summer.co %>% 
  select(site_name, ndvi, year) %>% 
  group_by(site_name, year) %>% 
  summarise(aver_ndvi = round(mean(ndvi), 2))
summary(summer.aver.ndvi.co)

# Average across sites to get an annual value for the summer grounds
summer.aver.ndvi.co <- summer.aver.ndvi.co %>% 
  group_by(year) %>% 
  summarise(aver_ndvi = round(mean(aver_ndvi), 2)) 


# Results from summarizing precipitation and ndvi data 

# Create data set with total precip and aver ndvi per year 
resources.full.summer <- summer.total.prcp.co %>% 
  left_join(summer.aver.ndvi.co, by = 'year')

# Add the winter in co data to the resources full co data set


# What is the correlation between average ndvi and precipitation?
cor.test(summer.total.prcp.co$total_prcp, 
         summer.aver.ndvi.co$aver_ndvi)
# 0.7
# Medium positive correlation

# Plot ndvi 
ggplot(summer.aver.ndvi.co, aes(factor(year), aver_ndvi)) +
  geom_bar(stat = 'identity', fill = 'lightgreen') +
  labs(x = 'Year', y = 'Average NDVI') + 
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = 'none',
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


# Winter data in Colorado

# Prepare daymet data set to analyze it for winter months in Colorado 
daymet.winter.co <- daymet %>% 
  filter(month %in% c(12, 1, 2),
         site %in% summer.sites$code) %>%
  rename(code = site) %>% 
  left_join(select(summer.sites, code, site, location), by = 'code') %>% 
  mutate(year_plus = ifelse(month == 12, year+1, year),
         year_less = year_plus-1) %>% 
  unite(winter_period, c(year_less, year_plus), sep = '-', remove = T) %>% 
  filter(!winter_period %in% c('2012-2013'))

# x) SWE

# Total SWE over each site and winter period
winter.site.total.swe.co <- daymet.winter.co %>% 
  select(site, swe, winter_period) %>% 
  group_by(site, winter_period) %>% 
  summarise(total_swe = sum(swe))
summary(winter.site.total.swe.co)

# Average across sites to get an annual value for the wintering period in CO
winter.total.swe.co <- winter.site.total.swe.co %>% 
  group_by(winter_period) %>% 
  summarise(total_swe = sum(total_swe)) 

# Plot precipitation 
ggplot(winter.total.swe.co, aes(winter_period, total_swe)) +
  geom_bar(stat = 'identity', fill = 'lightblue') +
  labs(x = 'Winter Period', y = 'Total Snow Water Equivalent (kg/m2)') + 
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = 'none',
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# Frost threshold
frost.threshold <- -5

# Count the number of days when the min temperature was equal or under 
# the frost threshold and average per winter period for the growing season 
# In RMBL late May to September, so I'm going to use summer months in CO
winter.frost.days.co <- daymet.summer.co %>% 
  filter(month %in% c(5,6)) %>%  
  mutate(frost = if_else(tmin <= frost.threshold, 1, 0)) %>%  
  group_by(site, year) %>%
  summarize(num_frost_days = sum(frost)) %>% 
  group_by(year) %>%
  summarize(mean_frost_days = round(mean(num_frost_days), 0))

# Test for correlations between winter SWE and summer NDVI and precip

# But first plot data
plot(summer.total.prcp.co$total_prcp, summer.aver.ndvi.co$aver_ndvi) # looks linear

plot(winter.total.swe.co$total_swe, summer.aver.ndvi.co$aver_ndvi) # looks linear

