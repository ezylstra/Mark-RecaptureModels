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

# Prepare effort data to add to ddl
effort.stand <- effort.raw %>% 
  # Sites not included in capture data for analysis:
  filter(!site %in% c('CLP', 'BGMD', 'WB2','WB1', 'WPK1', 'NFPC', 'POLC', 'SHIP')) %>%  
  group_by(year) %>% 
  summarize(total_days = sum(total_banding_days)) %>% 
  rename(time = year,
         effort = total_days) %>% 
  mutate(effort = z.stand(effort), .keep = 'unused') %>% 
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

# ------------------------------ RUN CJS ANALYSIS ---------------------------- #
# ------------------------- Two covariates at a time ------------------------- #

# Process the encounter history data frame for Mark analysis
# No location included. ahy = after hatch year = adults
ahy.process <- process.data(ch.adults,
                            model = 'CJS',
                            begin.time = 2003,
                            groups = c('sex')) 

# Create design data frame
ahy.ddl <- make.design.data(ahy.process)

# Add effort to ddl 
ahy.ddl$p <- merge_design.covariates(
  ahy.ddl$p, effort.stand)

# Add temperature covariates to ddl 
ahy.ddl$Phi <- merge_design.covariates(
  ahy.ddl$Phi, winter.mx.stand)

# 1) Which temperature variable in the wintering grounds better explains survival?

# ~~~~~~~~~~~~~~~~~~~~~~ aver_min_temp vs aver_daily_min_temp ~~~~~~~~~~~~~~~~ #

# Create function to run models
ahy.temp.mx.1 <- function()
{
  Phi.dot <- list(formula = ~1) 
  Phi.sex <- list(formula = ~sex)
  Phi.time <- list(formula = ~time)
  Phi.minTemp <- list(formula = ~aver_min_temp_z)
  Phi.dailyTemp <- list(formula = ~aver_daily_min_temp_z)
  
  p.dot <- list(formula = ~1)
  p.sex <- list(formula = ~sex)
  p.time <- list(formula = ~time)
  p.effort <- list(formula = ~effort)
  
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
ahy.temp.mx.results.1 <- ahy.temp.mx.1()
ahy.temp.mx.results.1

# Model with lowest Delta AIC 
# Phi(~sex)p(~time)

# Looking at Delta AIC:
# Neither temperature variable included in the formulas explain survival better 
# than sex; aver_min_temp performed better than aver_daily_min_temp 

# Look at estimates and standard errors 
results.1 <- ahy.temp.mx.results.1[[16]]
results.1$results$beta
results.1$results$real

# Since p(~sex)p(~effort) was the second best model, I tried to include an 
# additive model p(~effort + time). This resulted in two models with AIC 0
# p(~sex)p(~effort) and p(~sex)p(~effort + time). After exploring SE in both 
# results I decided to exclude p(~effort + time) as SE were huge.   

# Remove mark files so they don't clog repo
invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))

# ~~~~~~~~~~~~~~~~~~~~~~~~ aver_min_temp vs aver_cold_days ~~~~~~~~~~~~~~~~~~~ #

# Create function to run models
ahy.temp.mx.2 <- function()
{
  Phi.dot <- list(formula = ~1) 
  Phi.sex <- list(formula = ~sex)
  Phi.time <- list(formula = ~time)
  Phi.minTemp <- list(formula = ~aver_min_temp_z)
  Phi.coldDays <- list(formula = ~aver_cold_days_z)
  
  p.dot <- list(formula = ~1)
  p.sex <- list(formula = ~sex)
  p.time <- list(formula = ~time)
  p.effort <- list(formula = ~effort)
  
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
ahy.temp.mx.results.2 <- ahy.temp.mx.2()
ahy.temp.mx.results.2

# Model with lowest Delta AIC 
# Phi(~sex)p(~time)

# Looking at Delta AIC:
# Neither temperature variable included in the formulas explain survival better 
# than sex; aver_min_temp performed better than aver_cold_days by a little bit

# Look at estimates and standard errors 
results.2 <- ahy.temp.mx.results.2[[3]]
results.2$results$beta
results.2$results$real

# Remove mark files so they don't clog repo
invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))

# ~~~~~~~~~~~~~~~~~~~~ aver_daily_min_temp vs aver_cold_days ~~~~~~~~~~~~~~~~~ #

# Create function to run models
ahy.temp.mx.3 <- function()
{
  Phi.dot <- list(formula = ~1) 
  Phi.sex <- list(formula = ~sex)
  Phi.time <- list(formula = ~time)
  Phi.minTemp <- list(formula = ~aver_daily_min_temp_z)
  Phi.coldDays <- list(formula = ~aver_cold_days_z)
  
  p.dot <- list(formula = ~1)
  p.sex <- list(formula = ~sex)
  p.time <- list(formula = ~time)
  p.effort <- list(formula = ~effort)
  
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
ahy.temp.mx.results.3 <- ahy.temp.mx.3()
ahy.temp.mx.results.3

# Model with lowest Delta AIC 
# Phi(~sex)p(~time)

# Looking at Delta AIC:
# Neither temperature variable included in the formulas explain survival better 
# than sex; aver_cold_days performed better than aver_daily_min_temp

# Look at estimates and standard errors 
results.3 <- ahy.temp.mx.results.3[[3]]
results.3$results$beta
results.3$results$real

# Remove mark files so they don't clog repo
invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))

# Is this right?:
# Even though neither temperature variable explained survival better than sex, 
# it is preferable to use aver_min_temp in further analysis because it performed 
# better than aver_daily_min_temp and cold_days when used in different combinations.

# 2) Which resource availability variable in the wintering grounds better 
# explains survival?

# ~~~~~~~~~~~~~~~~~~~~~~~~~ aver_NDVI vs aver_precip  ~~~~~~~~~~~~~~~~~~~~~~~~ #

# Create function to run models
ahy.resources.mx <- function()
{
  Phi.dot <- list(formula = ~1) 
  Phi.sex <- list(formula = ~sex)
  Phi.time <- list(formula = ~time)
  Phi.ndvi <- list(formula = ~average_ndvi_z)
  Phi.precip <- list(formula = ~aver_precip_z)
  
  p.dot <- list(formula = ~1)
  p.sex <- list(formula = ~sex)
  p.time <- list(formula = ~time)
  p.effort <- list(formula = ~effort)
  
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
# Phi(~sex)p(~time)

# Looking at Delta AIC:
# Neither resource availability variable included in the formulas explain survival 
# better than sex; aver_precip performed better than aver_ndvi 

# Look at estimates and standard errors 
results.4 <- ahy.resources.mx.results[[11]]
results.4$results$beta
results.4$results$real

# Remove mark files so they don't clog repo
invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))

################################################################################

# Trying to run a more complete set of models

# Create function to run models
ahy.mx.full <- function()
{
  Phi.dot <- list(formula = ~1) 
  Phi.sex <- list(formula = ~sex)
  Phi.time <- list(formula = ~time)
  # Added this model to identify if survival of F and M varies by year
  Phi.sexandtime <- list(formula = ~ sex + time) 
  # Using the two covariates that performed the best
  Phi.minTemp <- list(formula = ~aver_min_temp_z)
  Phi.precip <- list(formula = ~aver_precip_z)
  
  p.dot <- list(formula = ~1)
  p.sex <- list(formula = ~sex)
  p.time <- list(formula = ~time)
  p.effort <- list(formula = ~effort)
  
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
ahy.mx.full.results <- ahy.mx.full()
ahy.mx.full.results

# Model with lowest Delta AIC 
# Phi(~sex + time)p(~time)
# Models with environmental covarites are not in the top 5 models

# Look at estimates and standard errors 
results.5 <- ahy.mx.full.results[[20]]
results.5$results$beta
results.5$results$real

# Survival probability for males is lower than females and also varies by year.
# Declines in 2006, 2009 and 2010 and goes way high in 2011

# Should I include Phi(~sex + time) in the functions in this script to select the 
# environmental covariates? 

# Remove mark files so they don't clog repo
invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))