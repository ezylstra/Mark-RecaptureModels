# Clean and summarize daymet and MODIS data
# Gaby Samaniego
# gabysamaniego@arizona.edu
# 2024-08-27

# Load packages
library(tidyverse)

# Clear environment
rm(list = ls())

# Load data 
daymet <- read.csv('output/cleaned-daymet-data-all-sites.csv')

# Remove X column created in data set 
daymet <- daymet %>% 
  select(-X)

# dayl = day length s/day
# prcp = precipitation mm/day
# srad = shortwave radiation W/m2
# swe = snow water equivalent kg/m2
# tmax = maximum air temperature C
# tmin = minimum air temperature C
# vp = water vapor pressure Pa 

# Plot annual values  

# Calculate annual values 
summer.sites <- filter(daymet, str_detect(site, 'ss')) %>% 
  group_by(year) %>% 
  summarize(ave.precip = mean(prcp),
            ave.tmax = mean(tmax),
            ave.tmin = mean(tmin),
            min.tmin = min(tmin),
            max.tmax = max(tmax))

# Plot data in histograms
ggplot(summer.sites, aes(x = year, y = ave.precip)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_gradient(name = 'ºC', low = "#47BEFB", high = "#ED6AA8") +
  xlab('Year') +
  ylab('Average annual precipitation in summer sites') 


+scale_color_gradient(name = "ºC", low = "#47BEFB", high = "#ED6AA8")+ggtitle("Historical air temperature trend in Berkeley, CA")+xlab("Year")+ylab("Annual Mean Temperature in Berkeley [ºC]")
ggplot(data = df, aes(x = DATE, y = TAVG, color = TAVG))+geom_point(size = 7, alpha = 0.8)+scale_color_gradient(name = "ºC", low = "#47BEFB", high = "#ED6AA8")+ggtitle("Historical air temperature trend in Berkeley, CA")+xlab("Year")+ylab("Annual Mean Temperature in Berkeley [ºC]")


# Change the width of bins
ggplot(df, aes(x=weight)) + 
  geom_histogram(binwidth=1)
# Change colors
p<-ggplot(df, aes(x=weight)) + 
  geom_histogram(color="black", fill="white")
p


# This is my first exploration of the climate data, therefore these covariates
# are not the final ones. I need to think better how to group the data, because 
# the winter months overlap two years (November 2001 to March 2002) so grouping
# by year won't work. I should filter by date first (between dates) and then assign
# a new name to those filtered rows. Something like 'winter period' '2001-2002'
# For now I'm going to leave this script like this.



barplot(ave.precip ~ year, data = summer.sites, xlab = 'Year', 
        ylab = 'Average Precipitation', main = "Summer Sites")
