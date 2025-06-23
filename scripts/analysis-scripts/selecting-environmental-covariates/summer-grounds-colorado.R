# Selecting environmental covariates for survival analysis

# Covariates for summer period: May-June-July-August in Colorado

  # average max temperature 
  # average min temperature
  # average daily max temperature
  # average daily min temperature
  # average warm days
  # average cold days
  # average ndvi
  # average max precipitation
  # frost days
  # snow water equivalent

# Gaby Samaniego
# gabysamaniego@arizona.edu
# 2024-05-30

# Load packages
library(tidyverse)
library(RMark)

# Clear environment
rm(list = ls()) 

# Load banding data
dat.raw <- read.csv('output/capture-data/cleanded-capture-data-RMNP-full.csv')

# Prepare data set for survival analysis 
dat <- dat.raw %>%
  filter(!band_site %in% c('WB2','WB1', 'WPK1', 'NFPC', 'POLC', 'SHIP')) %>% 
  select(band, band_status, year, sex, obssite, band_age, band_site) %>% 
  rename(age = band_age) %>% 
  distinct() %>% 
  arrange(band, band_status, year)

# -------------------- CREATE CAPTURE HISTORIES FOR BTLH --------------------- # 

# -------------- Capture histories without age at first capture -------------- #
# --------------------- includes just adult individuals ---------------------- #

# Create capture history 
ch.adults <- dat %>% 
  filter(age == 'AHY') %>%   
  group_by(band, year, sex) %>%  
  summarize(n.observation = length(year)) %>%
  mutate(observed = 1) %>% 
  pivot_wider(names_from = year, 
              values_from = observed, 
              id_cols = c(band, sex), 
              values_fill = 0) %>% 
  relocate(band, '2003','2004','2005','2006','2007','2008','2009','2010','2011', 
           '2012', sex) %>% 
  unite(ch, c('2003','2004','2005','2006','2007','2008','2009','2010','2011','2012'), 
        sep = '')%>% 
  as.data.frame() %>% 
  select(-band) 

# Make sex variable factor (specifying levels for clarity)
ch.adults$sex <- factor(ch.adults$sex, levels = c('F', 'M'))

# Checks
head(ch.adults)
str(ch.adults)

# ------------------------------ PRPEPARE COVARIATES ------------------------- #

# Creates function to z-standardize the covariates
z.stand <- function(x) {
  (x - mean(x)) / sd(x)
}

# -------------------------------- Trapping Effort --------------------------- #

# Load effort data
effort.raw <- read.csv('output/banding-effort-data/banding-effort-all-sites-RMNP.csv')
# Total banding days per year

# Edit effort data and standardize it
effort.z <- effort.raw %>% 
  # Sites not included in capture data for analysis:
  filter(!site %in% c('CLP', 'BGMD', 'WB2','WB1', 'WPK1', 'NFPC', 'POLC', 'SHIP')) %>% 
  group_by(year) %>%
  summarize(total_days = sum(total_banding_days, na.rm = TRUE), 
            .groups = 'drop') %>% 
  rename(time = year,
         effort_raw = total_days) %>% 
  mutate(effort = z.stand(effort_raw)) %>%
  select(time, effort) %>% 
  as.data.frame()

# ---------------------------- Environmental Covariates ---------------------- # 

# Load data
summer.co <- read.csv('output/weather-data/covariates-output/summer-covar-colorado.csv')
winter.co <- read.csv('output/weather-data/covariates-output/winter-swe-colorado.csv')

# Standardize summer covariates in Co
summer.co.stand <- summer.co %>%
  mutate(aver_max_temp_z = z.stand(aver_max_temp),
         aver_min_temp_z = z.stand(aver_min_temp),
         aver_daily_max_temp_z = z.stand(aver_daily_max_temp),
         aver_daily_min_temp_z = z.stand(aver_daily_min_temp), 
         aver_warm_days_z = z.stand(aver_warm_days),
         aver_cold_days_z = z.stand(aver_cold_days),
         aver_precip_z = z.stand(aver_precip),
         aver_ndvi_z = z.stand(average_ndvi), 
         frost_days_z = z.stand(frost_days), .keep = 'unused') %>% 
  rename(time = year)

# Standardize winter covariate in Co
winter.co.stand <- winter.co %>% 
  mutate(time = 2002:2012, .after = winter_period, # to match time in Phi
         swe_z = z.stand(total_swe_winter_co), .keep = 'unused') %>% 
  select(-winter_period)

# ----------------- PROCESS CAPTURE HISTORIES FOR MARK ANALYSIS -------------- #

# Process capture histories
ahy.process <- process.data(ch.adults,
                            model = 'CJS',
                            begin.time = 2003,
                            groups = c('sex'))

# Create design data frame
ahy.ddl <- make.design.data(ahy.process)

# Add effort to ddl 
ahy.ddl$p <- merge_design.covariates(
  ahy.ddl$p, effort.z)

# Add temperature covariates to ddl 
ahy.ddl$Phi <- merge_design.covariates(
  ahy.ddl$Phi, summer.co.stand)

# Add snow water equivalent to ddl
ahy.ddl$Phi <- merge_design.covariates(
  ahy.ddl$Phi, winter.co.stand)

# ------------------------ RUN MODELS TO SELECT COVARIATES ------------------- #

# ----------------- All max temperature covariates at same time -------------- #

# Which max temperature variable in the summer grounds better explains survival?

# Create function to run models 
ahy.max.temp.co <- function()
{
  Phi.sex <- list(formula = ~sex)
  Phi.sexMaxTemp <- list(formula = ~sex + aver_max_temp_z)
  Phi.sexDailyMaxTemp <- list(formula = ~sex + aver_daily_max_temp_z)
  Phi.sexWarmdDays <- list(formula = ~sex + aver_warm_days_z)
  
  p.sexEffort <- list(formula = ~sex + effort)
  
  # Create a data frame of all combinations of parameter specifications for each 
  # parameter
  cml <- create.model.list('CJS')  
  
  # Construct and run a set of MARK models from the cml data frame
  results <- mark.wrapper(cml, 
                          data = ahy.process,
                          ddl = ahy.ddl,
                          adjust = FALSE) # Accepts the parameter counts from MARK
  return(results)
}

# Run the function
ahy.max.temp.co.results <- ahy.max.temp.co()
ahy.max.temp.co.results

# Model with lowest Delta AIC 
# Phi(~sex)p(~sex + effort) 0.0
# Closely followed by 
# Phi(~sex + aver_warm_days_z)p(~sex + effort_z) 0.86
# Phi(~sex + aver_max_temp_z)p(~sex + effort) 0.86
# and
# Phi(~sex + aver_daily_max_temp_z)p(~sex + effort) 1.62

# Look at estimates and standard errors 
results.1 <- ahy.max.temp.co.results[[2]]
results.1$results$beta

# Looking at the estimates in all models, none of the max temperature covariates 
# had a significant effect on survival. Although all candidate models had delta AIC <2, 
# the effects were low. 

# Are these covaraites correlated?
cor(summer.co.stand[, c('aver_warm_days_z', 
                        'aver_max_temp_z', 
                        'aver_daily_max_temp_z')])

# All variables are moderate to highly correlated. 

# As suggested by Erin, I should choose one of theme to add to the more complex model 
# with the other covaraites. I'm going to use aver_warm_days as it can be use a 
# threshold when days were to hot for the birds and/or availability of resources

# Remove mark files so they don't clog repo
invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))


# ----------------- All min temperature covariates at same time -------------- #

# Which min temperature variable in the summer grounds better explains survival?

# Create function to run models 
ahy.min.temp.co <- function()
{
  Phi.sex <- list(formula = ~sex)
  Phi.sexMinTemp <- list(formula = ~sex + aver_min_temp_z)
  Phi.sexDailyMinTemp <- list(formula = ~sex + aver_daily_min_temp_z)
  Phi.sexColdDays <- list(formula = ~sex + aver_cold_days_z)
  
  p.sexEffort <- list(formula = ~sex + effort)
  
  # Create a data frame of all combinations of parameter specifications for each 
  # parameter
  cml <- create.model.list('CJS')  
  
  # Construct and run a set of MARK models from the cml data frame
  results <- mark.wrapper(cml, 
                          data = ahy.process,
                          ddl = ahy.ddl,
                          adjust = FALSE) # Accepts the parameter counts from MARK
  return(results)
}

# Run the function
ahy.min.temp.co.results <- ahy.min.temp.co()
ahy.min.temp.co.results

# Model with lowest Delta AIC 
# Phi(~sex + aver_min_temp_z)p(~sex + effort) 0.0
# Not closely followed by other model

# Look at estimates and standard errors 
results.2 <- ahy.min.temp.co.results[[4]]
results.2$results$beta

# Warmer summers decrease the probability of survival (-, significant)

# Based on delta AIC values, aver_min_temp is the covariate that explained survival 
# better as the other candidate models had delta AIC > 2. Also aver_min_temp
# had a significant effect on survival.

# I'm keeping aver min temp as a covariate for the summer grounds and dropping
# the others off

# Remove mark files so they don't clog repo
invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))


# -------------------- All resources covariates at same time ----------------- #

# Which resource variable in the summer grounds better explains survival?

# Create function to run models
ahy.resources.co <- function()
{
  Phi.sex <- list(formula = ~sex)
  Phi.sexPrecip <- list(formula = ~sex + aver_precip_z)
  Phi.sexNDVI <- list(formula = ~sex + aver_ndvi_z)
  Phi.sexFrostDays <- list(formula = ~sex + frost_days_z)
  Phi.sexSWE <- list(formula = ~sex + swe_z)
  
  p.sexeffort <- list(formula = ~sex + effort)
  
  # Create a data frame of all combinations of parameter specifications for each 
  # parameter
  cml <- create.model.list('CJS')  
  
  # Construct and run a set of MARK models from the cml data frame
  results <- mark.wrapper(cml, 
                          data = ahy.process,
                          ddl = ahy.ddl,
                          adjust = FALSE) # Accepts the parameter counts from MARK
  return(results)
}

# Run the function
ahy.resources.co.results <- ahy.resources.co()
ahy.resources.co.results

# Model with lowest Delta AIC 
# Phi(~sex + frost_days_z)p(~sex + effort) 0.0
# Followed by 
# Phi(~sex + aver_precip_z)p(~sex + effort) 3.12

# Look at estimates and standard errors 
results.3 <- ahy.resources.co.results[[4]]
results.3$results$beta

# The number of frost days has a positive effect on survival (+, significant),
# As the number of frost days increases survival also increases. Doesn't make
# sense! 

# Exploring the results for Phi(~sex + aver_precip_z)p(~sex + effort), it seems
# like more precipitation increases the probability of survival as CI barely
# overlap zero.

# Look at correlation between covariates
cor.test(summer.co.stand$frost_days_z,
         summer.co.stand$aver_precip_z)

# No correlation between covariates (0.002) statistically not significant (0.995)

# I'm inclined not to include the number of frost days in further analysis and
# choose average precip

# Remove mark files so they don't clog repo
invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))

# What if I remove the number of frost days from the candidate models?

# Create function to run models
ahy.resources.co.2 <- function()
{
  Phi.sex <- list(formula = ~sex)
  Phi.sexPrecip <- list(formula = ~sex + aver_precip_z)
  Phi.sexNDVI <- list(formula = ~sex + aver_ndvi_z)
  Phi.sexSWE <- list(formula = ~sex + swe_z)
  
  p.sexeffort <- list(formula = ~sex + effort)
  
  # Create a data frame of all combinations of parameter specifications for each 
  # parameter
  cml <- create.model.list('CJS')  
  
  # Construct and run a set of MARK models from the cml data frame
  results <- mark.wrapper(cml, 
                          data = ahy.process,
                          ddl = ahy.ddl,
                          adjust = FALSE) # Accepts the parameter counts from MARK
  return(results)
}

# Run the function
ahy.resources.co.results.2 <- ahy.resources.co.2()
ahy.resources.co.results.2

# Model with lowest Delta AIC 
# Phi(~sex + aver_precip_z)p(~sex + effort) 0.0
# Followed by 
# Phi(~sex)p(~sex + effort) 0.57
# Phi(~sex + aver_ndvi_z)p(~sex + effort) 1.16
# Phi(~sex + swe_z)p(~sex + effort) 2.24

# Look at estimates and standard errors 
results.4 <- ahy.resources.co.results.2[[3]]
results.4$results$beta

# Look at correlation between covariates
cor.test(summer.co.stand$aver_ndvi_z,
         summer.co.stand$aver_precip_z)

# Moderate positive correlation between covariates (0.7) statistically significant 
# (0.017)

# Remove mark files so they don't clog repo
invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))

