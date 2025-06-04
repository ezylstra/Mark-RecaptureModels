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
  select(band, band_status, year, sex, obssite, band_age, band_site, location) %>% 
  rename(age = band_age) %>% 
  distinct() %>% 
  arrange(band, band_status, year)

# -------------------- CREATE CAPTURE HISTORIES FOR BTLH --------------------- # 

# -------------- Capture histories without age at first capture -------------- #
# --------------------- includes just adult individuals ---------------------- #

# Create capture history 
ch.adults <- dat %>% 
  filter(age == 'AHY') %>%   
  group_by(band, year, sex, location) %>%  
  summarize(n.observation = length(year)) %>%
  mutate(observed = 1) %>% 
  pivot_wider(names_from = year, 
              values_from = observed, 
              id_cols = c(band, sex, location), 
              values_fill = 0) %>% 
  relocate(band, '2003','2004','2005','2006','2007','2008','2009','2010','2011', 
           '2012', sex, location) %>% 
  unite(ch, c('2003','2004','2005','2006','2007','2008','2009','2010','2011','2012'), 
        sep = '')%>% 
  as.data.frame() %>% 
  select(-band) 

# Make several variables factors (specifying levels for clarity)
ch.adults$sex <- factor(ch.adults$sex, levels = c('F', 'M'))
ch.adults$location <- factor(ch.adults$location, levels = c('east', 'west'))

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

# 1) LOCATION INCLUDED
ahy.process.loc <- process.data(ch.adults,
                                model = 'CJS',
                                begin.time = 2003,
                                groups = c('sex', 'location'))

# Create design data frame
ahy.ddl.loc <- make.design.data(ahy.process.loc)

# Add effort to ddl 
ahy.ddl.loc$p <- merge_design.covariates(
  ahy.ddl.loc$p, effort.z)

# Add temperature covariates to ddl 
ahy.ddl.loc$Phi <- merge_design.covariates(
  ahy.ddl.loc$Phi, summer.co.stand)

# 2) LOCATION NOT INCLUDED
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

# 1) LOCATION INCLUDED
ahy.max.temp.co.1 <- function()
{
  Phi.sexLocation <- list(formula = ~sex + location)
  Phi.sexLocationMaxTemp <- list(formula = ~sex + location + aver_max_temp_z)
  Phi.sexLocationDailyMaxTemp <- list(formula = ~sex + location + aver_daily_max_temp_z)
  Phi.sexLocationWarmdDays <- list(formula = ~sex + location + aver_warm_days_z)
  
  p.sexEffort <- list(formula = ~sex + effort)
  
  # Create a data frame of all combinations of parameter specifications for each 
  # parameter
  cml <- create.model.list('CJS')  
  
  # Construct and run a set of MARK models from the cml data frame
  results <- mark.wrapper(cml, 
                          data = ahy.process.loc,
                          ddl = ahy.ddl.loc,
                          adjust = FALSE) # Accepts the parameter counts from MARK
  return(results)
}

# Run the function and explore results
ahy.max.temp.co.results.1 <- ahy.max.temp.co.1()
ahy.max.temp.co.results.1

# Model with lowest Delta AIC 
# Phi(~sex + location)p(~sex + effort)
# Closely followed by 
# Phi(~sex + location + aver_cold_days_z)p(~sex + effort_z)
# and
# Phi(~sex + location + aver_max_temp_z)p(~sex + effort)

# Based on DeltaAIC values, neither max temperature covariate explained survival

# Look at estimates and standard errors 
results.1 <- ahy.max.temp.co.results.1[[2]]
results.1$results$beta

# Exploring the estimates in all models, neither max temp covariate have a significant
# effect on survival

# Remove mark files so they don't clog repo
invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))

# 2) LOCATION NOT INCLUDED
# As location did not have an effect in survival, I remove it and run new models
ahy.max.temp.co.2 <- function()
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
ahy.max.temp.co.results.2 <- ahy.max.temp.co.2()
ahy.max.temp.co.results.2

# Model with lowest Delta AIC 
# Phi(~sex)p(~sex + effort)
# Closely followed by 
# Phi(~sex + aver_cold_days_z)p(~sex + effort_z)
# and
# Phi(~sex + aver_max_temp_z)p(~sex + effort)

# Based on DeltaAIC values, neither max temperature covariate explained survival

# Look at estimates and standard errors 
results.2 <- ahy.max.temp.co.results.2[[3]]
results.2$results$beta

# Exploring the estimates in all models, neither max temp covariate have a significant
# effect on survival

# Remove mark files so they don't clog repo
invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))


# ----------------- All min temperature covariates at same time -------------- #

# Which min temperature variable in the summer grounds better explains survival?

# Create function to run models 

# LOCATION NOT INCLUDED
# As location did not have an effect in survival, I remove it and run new models
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
# Phi(~sex + aver_min_temp_z)p(~sex + effort)
# Not closely followed by other model

# Based on DeltaAIC values, aver_min_temp is the covariate that explained survival 
# better

# Look at estimates and standard errors 
results.3 <- ahy.min.temp.co.results[[4]]
results.3$results$beta

# Adult males have less probability of survival than females (-, significant) 
# Warmer summers decrease the probability of survival (-, significant)
# Sex does not have an effect on the probability of recapture (not significant)
# More effort increases the probability of recapture (+, significant)

# Remove mark files so they don't clog repo
invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))


# ------------------- All temperature covariates at same time ---------------- #

# Which temperature variable in the summer grounds better explains survival?

# Create function to run models 
ahy.temp.co <- function()
{
  Phi.sex <- list(formula = ~sex)
  Phi.sexMinTemp <- list(formula = ~sex + aver_min_temp_z)
  Phi.sexDailyMinTemp <- list(formula = ~sex + aver_daily_min_temp_z)
  Phi.sexColdDays <- list(formula = ~sex + aver_cold_days_z)
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
ahy.temp.co.results <- ahy.temp.co()
ahy.temp.co.results

# Model with lowest Delta AIC 
# Phi(~sex + aver_min_temp_z)p(~sex + effort)
# Not closely followed by other model

# Again, based on DeltaAIC values, aver_min_temp is the covariate that explained 
# survival better

# Look at estimates and standard errors 
results.4 <- ahy.temp.co.results[[6]]
results.4$results$beta

# Adult males have less probability of survival than females (-, significant) 
# Warmer summers decrease the probability of survival (-, significant)
# Sex does not have an effect on the probability of recapture (not significant)
# More effort increases the probability of recapture (+, significant)

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
# Phi(~sex + frost_days_z)p(~sex + effort)
# Followed by 
# Phi(~sex + aver_precip_z)p(~sex + effort)

# Look at estimates and standard errors 
results.5 <- ahy.resources.co.results[[2]]
results.5$results$beta

# Adult males have less probability of survival than females (-, significant) 
# The number of frost days have a positive effect on survival (+, significant),
# As the number of frost days increases survival also increases. Doesn't make
# sense!
# Sex does not have an effect on the probability of recapture (not significant)
# More effort increases the probability of recapture (+, significant)

# Remove mark files so they don't clog repo
invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))

