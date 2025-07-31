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

# Identify birds banded in September
sept.birds <- dat.raw %>% 
  filter(band_status == 1 & month == 9) %>% 
  select(band)

# Prepare data set for survival analysis 
dat <- dat.raw %>%
  filter(!band %in% sept.birds$band,# exclude September birds
         !band_site %in% c('WB2','WB1', 'WPK1', 'NFPC', 'POLC', 'SHIP'),
         month != 9) %>% # exclude September recaptures
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

# Edit effort data and standardize it
effort.z <- effort.raw %>% 
  # Sites not included in capture data for analysis:
  filter(!site %in% c('CLP', 'BGMD', 'WB2','WB1', 'WPK1', 'NFPC', 'POLC', 'SHIP')) %>% 
  group_by(year) %>%
  summarize(total_days = sum(total_banding_days, na.rm = TRUE),
            total_trap_hours = sum(total_trap_hours, na.rm = TRUE),
            .groups = 'drop') %>% 
  rename(time = year,
         effort_days = total_days,
         effort_hours = total_trap_hours) %>% 
  mutate(effort_days_z = z.stand(effort_days),
         effort_hours_z = z.stand(effort_hours)) %>%
  select(time, effort_days_z, effort_hours_z) %>% 
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
  
  p.sexEffort <- list(formula = ~effort_hours_z)
  
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
# Phi(~sex + aver_warm_days_z)p(~effort_hours_z) 0.0
# Followed by 
# Phi(~sex + aver_max_temp_z)p(~effort_hours_z) 2.46

# Look at estimates and standard errors of best model
results.1 <- ahy.max.temp.co.results[[4]]
results.1$results$beta

# Increase in the number of warm days in the summer grounds increases the 
# probability of survival (+, significant)

# Look at estimates and standard errors of second best model
results.2 <- ahy.max.temp.co.results[[3]]
results.2$results$beta

# Increase in temperature increases the probability of survival (+, significant)

# Check for correlation
cor.test(summer.co.stand$aver_max_temp_z,
         summer.co.stand$aver_warm_days_z)

# Highly positive correlation (0.93), statistically significant (p < 0.001)

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
  
  p.sexEffort <- list(formula = ~effort_hours_z)
  
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
# Phi(~sex + aver_min_temp_z)p(~ effort_hours_z)

# Look at estimates and standard errors 
results.3 <- ahy.min.temp.co.results[[4]]
results.3$results$beta

# Increase of min temperature (warmer summers) decreases the probability of survival
# (-, significant) 

# Check for correlation between max temp covariate and min temp covariate
cor.test(summer.co.stand$aver_min_temp_z,
         summer.co.stand$aver_warm_days_z)

# Weak positive correlation (0.2) not significant (p = 0.54)

# I could use both covaraites in the model

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
  
  p.sexeffort <- list(formula = ~effort_hours_z)
  
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
# Phi(~sex)p(~effort_hours_z) 0.0
# Followed very closely by 
# Phi(~sex + aver_precip_z)p(~effort_hours_z) 0.21
# Phi(~sex + frost_days_z)p(~effort_hours_z) 0.28
# Phi(~sex + swe_z)p(~effort_hours_z) 1.04

# It seems like adding the covariates does not improve model fit. They don't 
# explain survival better that just sex (base model)

# Look at estimates and standard errors of second best model 
results.4 <- ahy.resources.co.results[[4]]
results.4$results$beta

# Increase in precipitation has a weak positive effect on survival 
# (+, barely not significant) 

# Look at estimates and standard errors of third best model 
results.5 <- ahy.resources.co.results[[2]]
results.5$results$beta

# Similar results. Increase in frost days has a weak effect on survival 
# (+, not significant)

# Look at estimates and standard errors of fourth best model 
results.6 <- ahy.resources.co.results[[5]]
results.6$results$beta

# Similar results. Increase in swe has a weak effect on survival 
# (+, not significant)

# These results don't support the use of these covariates in the full model.

# Remove mark files so they don't clog repo
invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))
