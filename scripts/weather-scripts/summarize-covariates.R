# Summarize covariates for survival analysis using functions
# Gaby Samaniego
# gabysamaniego@arizona.edu
# 2025-03-10

# Load packages
library(tidyverse)

# Clear environment
rm(list = ls())

# --------------------------------- LOAD DATA -------------------------------- #

# Load daymet data
daymet <- read.csv('output/weather-data/cleaned-daymet-data-all-sites.csv')

# dayl = day length s/day
# prcp = precipitation mm/day
# srad = shortwave radiation W/m2
# swe = snow water equivalent kg/m2
# tmax = maximum air temperature C
# tmin = minimum air temperature C
# vp = water vapor pressure Pa 

# Calculate tmean
daymet <- daymet %>% 
  mutate(tmean = round((tmin + tmax) / 2, 2))

# Load MODIS data
modis <- read.csv('output/weather-data/cleaned-ndvi-data-all-sites-with-quality-flags.csv')

# Load thinned winter sites
winter.sites <- read.csv('data/sites-BTLH-range-map-dem/thinned-winter-sites.csv')

# Load summer sites
summer.sites <- read.csv('data/sites-BTLH-range-map-dem/thinned-summer-sites.csv')

# Edit summer sites
# Remove sites that don't have any recapture information and just a few captured
# birds
summer.sites <- summer.sites %>% 
  filter(!site %in% c('WB1', 'WPK1', 'NFPC', 'POLC', 'SHIP'))


# ------------------------------  CREATE FUNCTIONS --------------------------- #

# For summarizing temperature covariates
temperature <- function(data, 
                        covar, 
                        period, 
                        summary_function, # (tmin, tmax, tmean)
                        location_filter = NULL) {
  
  # If we need to filter by location (east vs west sites in summer grounds)
  if (!is.null(location_filter)) {
    data <- data %>% filter(location %in% location_filter)
  }
  
  # Calculate summary statistic per site and period/year
  site.summary <- data %>%
    group_by(site, {{ period }}) %>% # Need {{  }} to use unquoted variable's names in dplyr
    summarize(site_value = summary_function({{ covar }}, na.rm = TRUE),
              .groups = 'drop') # For consistent results in further analysis 
  
  # Calculate overall average per period/year across all sites
  period.summary <- site.summary %>%
    group_by({{ period }}) %>%
    summarize(average_temp_value = round(mean(site_value, na.rm = TRUE), 2),
              .groups = 'drop')
  
  return(period.summary)
}

# For summarizing number of cold/warm days based in a threshold
threshold.days <- function(data, 
                           covar, 
                           period, 
                           threshold, 
                           condition = 'cold', 
                           location_filter = NULL) {
  
  if (!is.null(location_filter)) {
    data <- data %>% filter(location %in% location_filter)
  }
  
  # Define cold and warm condition 
  if (condition == 'cold') {
    data <- data %>% mutate(threshold_day = if_else({{ covar }} <= threshold, 1, 0))
  } else if (condition == 'warm') {
    data <- data %>% mutate(threshold_day = if_else({{ covar }} >= threshold, 1, 0))
  } else {
    stop("Invalid condition. Use 'cold' or 'warm'.") 
  }
  
  # Calculate the number of threshold days per site and period/year
  site.summary <- data %>%
    group_by(site, {{ period }}) %>%
    summarize(num_days = sum(threshold_day, na.rm = TRUE), .groups = 'drop')
  
  # Calculate the average number of days under/over the threshold per period/year
  period.summary <- site.summary %>%
    group_by({{ period }}) %>%
    summarize(average_days = round(mean(num_days, na.rm = TRUE), 0), .groups = 'drop')
  
  return(period.summary)
}

# For summarizing precipitation and snow water equivalent (SWE)
precip.and.swe <- function(data, 
                           covar, 
                           period, 
                           location_filter = NULL) {
  
  if (!is.null(location_filter)) {
    data <- data %>% filter(location %in% location_filter)
  }
  
  # Calculate cumulative value per site and period/year
  site.summary <- data %>%
    group_by(site, {{ period }}) %>%
    summarize(total_value = sum({{ covar }}, na.rm = TRUE), .groups = 'drop')
  
  # Calculate cumulative value per period/year across all sites
  period.summary <- site.summary %>%
    group_by({{ period }}) %>%
    summarize(total_value = sum(total_value, na.rm = TRUE), .groups = 'drop')
  
  return(period.summary)
}

# For summarizing NDVI
ndvi <- function(data, 
                 covar, 
                 period, 
                 location_filter = NULL) {
  
  if (!is.null(location_filter)) {
    data <- data %>% filter(location %in% location_filter)
  }
  
  # Filter for high-quality NDVI data using quality flags: re_value == 0
  data <- data %>% filter(re_value == 0)
  
  # Calculate mean NDVI per site and period/year
  site.summary <- data %>%
    group_by(site_name, {{ period }}) %>%
    summarize(ndvi_value = round(mean({{ covar }}, na.rm = TRUE), 2), 
              .groups = 'drop')
  
  # Calculate overall NDVI per period
  period.summary <- site.summary %>%
    group_by({{ period }}) %>%
    summarize(average_ndvi = round(mean(ndvi_value, na.rm = TRUE), 2), .groups = 'drop')
  
  return(period.summary)
}


# --------------------------- WINTER COVARIATES ------------------------------ # 
# ------------------------------- In Mexico ---------------------------------- #

# Prepare data sets

# Daymet data set for winter months and winter periods in winter grounds
daymet.winter.mx <- daymet %>%
  filter(month %in% c(12, 1, 2),
         site %in% winter.sites$code) %>% 
  mutate(year_plus = ifelse(month == 12, year+1, year),
         year_less = year_plus-1) %>% 
  unite(winter_period, c(year_less, year_plus), sep = '-', remove = T) %>% 
  filter(!winter_period %in% c('2001-2002', '2012-2013'))

# MODIS data set for winter months and winter periods in winter grounds 
modis.winter.mx <- modis %>%
  mutate(month = month(calendar_date),
         year = year(calendar_date)) %>% 
  filter(month %in% c(12, 1, 2),
         site_name %in% winter.sites$code) %>% 
  mutate(year_plus = ifelse(month == 12, year+1, year),
         year_less = year_plus-1) %>% 
  unite(winter_period, c(year_less, year_plus), sep = '-', remove = T) %>% 
  filter(!winter_period %in% c('2001-2002', '2012-2013'))

# Run functions to summarize covariates

# 1) Overall minimum temperature
min.temp.mx <- temperature(daymet.winter.mx, 
                           tmin, 
                           winter_period, 
                           summary_function = min) %>% 
  rename(aver_min_temp = average_temp_value)

# 2) Average of daily minimum temperature
aver.daily.min.temp.mx <- temperature(daymet.winter.mx, 
                                      tmin, 
                                      winter_period, 
                                      summary_function = mean) %>% 
  rename(aver_daily_min_temp = average_temp_value)

# 3) Number of days when the daily mean temperature was equal or under 10 C
cold.days.mx <- threshold.days(daymet.winter.mx, 
                               tmean, 
                               winter_period, 
                               threshold = 10, 
                               condition = 'cold') %>% 
  rename(aver_cold_days = average_days)

# 4) Precipitation
total.precip.mx <- precip.and.swe(daymet.winter.mx, 
                                  prcp, 
                                  winter_period) %>% 
  rename(total_precip = total_value)

# 5) NDVI
ndvi.mx <- ndvi(modis.winter.mx, 
                ndvi, 
                winter_period)
                  
# Merge all winter covariates into a data frame
winter.covar.mx <- reduce(list(min.temp.mx,  # Combines elements in a single value and applies the left_joint to all elements in the list
                               aver.daily.min.temp.mx, 
                               cold.days.mx, 
                               total.precip.mx, 
                               ndvi.mx), 
                          left_join, 
                          by = 'winter_period') %>% 
  as.data.frame()

# Plot and check for correlation in data

# aver_min_temp vs Aver_daily_min_temp
plot(winter.covar.mx$aver_min_temp, winter.covar.mx$aver_daily_min_temp)
# Looks kind of linear...

# Run correlation 
cor.test(winter.covar.mx$aver_min_temp, winter.covar.mx$aver_daily_min_temp)
# Moderate positive correlation (0.534), but not statistically significant (p = 0.1119)

# aver_ndvi vs total_precip
plot(winter.covar.mx$average_ndvi, winter.covar.mx$total_precip)
# Does not look super linear

# Run correlation 
cor.test(winter.covar.mx$total_precip, winter.covar.mx$average_ndvi)
# Moderate to strong positive correlation (0.659), statistically significant (p = 0.03818)

# Select covariates
winter.covar.mx.final <- winter.covar.mx %>% 
  select(-c(aver_min_temp, average_ndvi))

# Export winter covariates
write.csv(winter.covar.mx.final, 
          'output/weather-data/covariates-output/winter-covar-mexico.csv',
          row.names = FALSE)


# --------------------------- SUMMER COVARIATES ------------------------------ # 
# ------------------------------ In Colorado --------------------------------- #

# Prepare data sets 

# Daymet data set for summer months in the summer grounds 
daymet.summer.co <- daymet %>% 
  filter(month %in% c(5, 6, 7, 8),
         site %in% summer.sites$code) %>%
  rename(code = site) %>% 
  left_join(select(summer.sites, code, site, location), by = 'code')

# Daymet data for winter months and winter period in the summer grounds
daymet.winter.co <- daymet %>% 
  filter(month %in% c(12, 1, 2),
         site %in% summer.sites$code) %>%
  rename(code = site) %>% 
  left_join(select(summer.sites, code, site, location), by = 'code') %>% 
  mutate(year_plus = ifelse(month == 12, year+1, year),
         year_less = year_plus-1) %>% 
  unite(winter_period, c(year_less, year_plus), sep = '-', remove = T) %>% 
  filter(!winter_period %in% c('2012-2013'))

# MODIS data set for summer months in summer grounds
modis.summer.co <- modis %>%
  mutate(month = month(calendar_date),
         year = year(calendar_date)) %>% 
  filter(month %in% c(5, 6, 7, 8),
         site_name %in% summer.sites$code) 

# Run functions to summarize covariates

# 1) Overall maximum temperature
max.temp.co <- temperature(daymet.summer.co, 
                           tmax, 
                           year, 
                           summary_function = max) %>% 
  rename(aver_max_temp = average_temp_value)

# 2) Overall minimum temperature
min.temp.co <- temperature(daymet.summer.co,
                           tmin,
                           year,
                           summary_function = min) %>% 
  rename(aver_min_temp = average_temp_value)

# 3) Average of daily maximum temperature
aver.daily.max.temp.co <- temperature(daymet.summer.co, 
                                      tmax, 
                                      year, 
                                      summary_function = mean) %>% 
  rename(aver_daily_max_temp = average_temp_value)

# 4) Average of daily minimum temperature
aver.daily.min.temp.co <- temperature(daymet.summer.co, 
                                      tmin, 
                                      year, 
                                      summary_function = mean) %>% 
  rename(aver_daily_min_temp = average_temp_value)

# 5) Number of days when the daily mean temperature was equal or over 20 C
warm.days.co <- threshold.days(daymet.summer.co, 
                               tmean, 
                               year, 
                               threshold = 20, 
                               condition = 'warm') %>% 
  rename(aver_warm_days = average_days)

# 6) Number of days when the daily mean temperature was equal or lower 10 C
cold.days.co <- threshold.days(daymet.summer.co, 
                               tmean, 
                               year, 
                               threshold = 10, 
                               condition = 'cold') %>% 
  rename(aver_cold_days = average_days)

# 7) Precipitation
total.precip.co <- precip.and.swe(daymet.summer.co, 
                                  prcp, 
                                  year) %>% 
  rename(total_precip = total_value)

# 8) NDVI
ndvi.co <- ndvi(modis.summer.co, 
                ndvi, 
                year)

# 9) SWE
total.swe.co <- precip.and.swe(daymet.winter.co,
                               swe,
                               winter_period) %>% 
  rename(total_swe_winter_co = total_value)

# 10) Frost days
frost.days.co <- threshold.days(daymet.summer.co, 
                                tmin, 
                                year, 
                                -7, 
                                'cold') %>% 
  rename(frost_days = average_days)

# Merge all summer covariates into a data frame
summer.covar.co <- reduce(list(max.temp.co,
                               min.temp.co,
                               aver.daily.max.temp.co,
                               aver.daily.min.temp.co,
                               warm.days.co,
                               cold.days.co,
                               total.precip.co,
                               ndvi.co,
                               frost.days.co), 
                          left_join, 
                          by = 'year') %>% 
  as.data.frame()

# Plot and check for correlation in data

# aver_max_temp vs aver_daily_max_temp
plot(summer.covar.co$aver_max_temp, summer.covar.co$aver_daily_max_temp)
# Looks linear

cor.test(summer.covar.co$aver_max_temp, summer.covar.co$aver_daily_max_temp)
# Moderate to strong positive correlation (0.624), statistically significant (p = 0.04008)

# aver_min_temp vs aver_daily_min_temp
plot(summer.covar.co$aver_min_temp, summer.covar.co$aver_daily_min_temp)
# Looks linear

cor.test(summer.covar.co$aver_min_temp, summer.covar.co$aver_daily_min_temp)
# Medium positive correlation (0.424), not statistically significant (p = 0.1935)

# aver_daily_max_temp vs aver_daily_min_temp
plot(summer.covar.co$aver_daily_max_temp, summer.covar.co$aver_daily_min_temp)
# Looks linear

cor.test(summer.covar.co$aver_daily_max_temp, summer.covar.co$aver_daily_min_temp)
# Moderate to strong positive correlation (0.61), statistically significant (0.04626) 

# aver_ndvi vs total_precip
plot(summer.covar.co$average_ndvi, summer.covar.co$total_precip)
# Looks linear

cor.test(summer.covar.co$average_ndvi, summer.covar.co$total_precip)
# Moderate to strong positive correlation (0.697), statistically significant (0.01705) 

# Select covariates 
summer.covar.co.final <- summer.covar.co %>% 
  select(-c(aver_max_temp, aver_min_temp, aver_warm_days, aver_daily_min_temp, 
            average_ndvi))

# Export summer covariates
write.csv(summer.covar.co.final, 
          'output/weather-data/covariates-output/summer-covar-colorado.csv',
          row.names = FALSE)

# Export winter snow water equivalent data for winter months in Colorado
write.csv(total.swe.co, 'output/weather-data/covariates-output/winter-swe-colorado.csv',
          row.names = FALSE)
