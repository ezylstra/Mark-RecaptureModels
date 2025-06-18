# Selecting environmental covariates for survival analysis

# Covariates for winter period: December-January-February in Mexico

  # average min temperature 
  # average daily min temperature
  # number of cold days
  # average mean ndvi
  # average max precipitation

# Gaby Samaniego
# gabysamaniego@arizona.edu
# 2024-05-01

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
winter.mx <- read.csv('output/weather-data/covariates-output/winter-covar-mexico.csv')

# Standardize winter mx covariates
winter.mx.stand <- winter.mx %>% 
  mutate(aver_min_temp_z = z.stand(aver_min_temp),
         aver_daily_min_temp_z = z.stand(aver_daily_min_temp), 
         aver_cold_days_z = z.stand(aver_cold_days),
         aver_precip_z = z.stand(aver_precip),
         average_ndvi_z = z.stand(average_ndvi), .keep = 'unused') %>% 
  mutate(time = 2002:2011, .after = winter_period) %>% # so time matches Phi
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
  ahy.ddl$Phi, winter.mx.stand)

# ------------------------ RUN MODELS TO SELECT COVARIATES ------------------- #

# ------------------- All temperature covariates at same time ---------------- #

# Which temperature variable in the wintering grounds better explains survival?

# Create function to run models 
ahy.temp.mx <- function()
{
  Phi.sex <- list(formula = ~sex)
  Phi.sexMinTemp <- list(formula = ~sex + aver_min_temp_z)
  Phi.sexDailyTemp <- list(formula = ~sex + aver_daily_min_temp_z)
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
ahy.temp.mx.results <- ahy.temp.mx()
ahy.temp.mx.results

# Model with lowest Delta AIC 
# Phi(~sex + aver_min_temp_z)p(~sex + effort_z) 0.0
# Followed by
# Phi(~sex + aver_cold_days_z)p(~sex + effort) 1.4

# Look at estimates and standard errors 
results.1 <- ahy.temp.mx.results[[4]]
results.1$results$beta

# Adult males have less probability of survival than females (-, significant) 
# Warmer winters (0.14) increase the probability of survival (+, significant)
# Sex does not have an effect on the probability of recapture (not significant)
# More effort increases the probability of recapture (+, significant)

# When explored the results of model 2, same as before, aver_cold_days had a similar
# effect on survival (-0.14) but the effect was negative. This makes total sense.

# Explore correlation between covariates in the two first candidate models
cor.test(winter.mx.stand$aver_min_temp_z, 
         winter.mx.stand$aver_cold_days_z)

# High negative correlation (-0.860), statistically significant (p = 0.001)

# I'm keeping aver_min_temp and dropping aver_cold_days

# Remove mark files so they don't clog repo
invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))


# -------------------- All resources covariates at same time ----------------- #

# Which resource variable in the wintering grounds better explains survival?

# Create function to run models
ahy.resources.mx <- function()
{
  Phi.sex <- list(formula = ~sex)
  Phi.sexPrecip <- list(formula = ~sex + aver_precip_z)
  Phi.sexNDVI <- list(formula = ~sex + average_ndvi_z)

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
ahy.resources.mx.results <- ahy.resources.mx()
ahy.resources.mx.results

# Model with lowest Delta AIC 
# Phi(~sex + aver_precip_z)p(~sex + effort) 0.0
# Followed by 
# Phi(~sex)p(~sex + effort) 0.38
# and
# Phi(~sex + average_ndvi_z)p(~sex + effort) 1.75

# Look at estimates and standard errors 
results.2 <- ahy.resources.mx.results[[3]]
results.2$results$beta

# Phi(~sex + aver_precip_z)p(~sex + effort) 0.0
# It's likely that more precipitation might be associated with lower survival,
# the 95% CI overlaps zero, but not by much (-, barely not significant)

# Phi(~sex + average_ndvi_z)p(~sex + effort)
# NDVI is not significant 

# Check for correlation, as delta AIC < 2 in model with NDVI
cor.test(winter.mx.stand$aver_precip_z,
         winter.mx.stand$average_ndvi_z)

# Moderate positive correlation (0.66), statistically significant (p = 0.04)

# I'm keeping aver_precip as a covariate based on delta AIC and significance 

# Remove mark files so they don't clog repo
invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))

