# Survival analysis for Broad-tailed Hummingbird at Rocky Mountain National Park
# Adults only and selected covariates for wintering and summer grounds included

# Gaby Samaniego
# gabysamaniego@arizona.edu
# 2024-06-06

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
           '2012', sex,) %>% 
  unite(ch, c('2003','2004','2005','2006','2007','2008','2009','2010','2011','2012'), 
        sep = '')%>% 
  as.data.frame() %>% 
  select(-band) 

# Make sex variable a factor (specifying levels for clarity)
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
summer.co <- read.csv('output/weather-data/covariates-output/summer-covar-colorado.csv')

# Edit data sets and prepare data
winter <- winter.mx %>% 
  mutate(time = 2002:2011, .after = winter_period) %>%
  select(time, aver_min_temp, aver_precip) %>% 
  mutate(winter_min_temp = z.stand(aver_min_temp),
         winter_precip = z.stand(aver_precip), .keep = 'unused')

summer <- summer.co %>% 
  rename(time = year) %>% 
  select(time, aver_warm_days, aver_min_temp, aver_precip, frost_days) %>% 
  mutate(summer_warm_days = z.stand(aver_warm_days),
         summer_min_temp = z.stand(aver_min_temp),
         summer_precip = z.stand(aver_precip),
         summer_frost_days = z.stand(frost_days), .keep = 'unused')
  
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

# Add summer covariates to ddl 
ahy.ddl$Phi <- merge_design.covariates(
  ahy.ddl$Phi, summer)

# Add winter covariates to ddl
ahy.ddl$Phi <- merge_design.covariates(
  ahy.ddl$Phi, winter)

# ----------------------------------- RUN MODELS ----------------------------- #

# Which of the selected covariates in the wintering and summer grounds better 
# explains survival of adults?

# Create function
ahy.full <- function()
{
  Phi.sex <- list(formula = ~sex)
  Phi.Time <- list(formula = ~Time)
  Phi.sexWMinTemp <- list(formula = ~sex + winter_min_temp) 
  Phi.sexSWarmDays <- list(formula = ~sex + summer_warm_days) 
  Phi.sexSMinTemp <- list(formula = ~sex + summer_min_temp)
  Phi.sexWPrecip <- list(formula = ~sex + winter_precip)
  Phi.sexSPrecip <- list(formula = ~sex + summer_precip)
  Phi.sexWSFrost <- list(formula = ~sex + summer_frost_days)
  
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
ahy.full.results <- ahy.full()
ahy.full.results

# Model with lowest Delta AIC
# Phi(~sex + winter_min_temp)p(~sex + effort) 0.0
# No other model with <2 Delta AIC
# Followed by
# Phi(~sex + summer_min_temp)p(~sex + effort) 9.3

# I included Time as a candidate model to test if survival has changed linearly 
# over the study period. The model Phi ~ Time performed poorly with the highest
# Delta AIC. Survival did not change over the 10 years of the study? Can I 
# make this claim? Variation in survival is better explained by winter temperature.

# Look at estimates and standard errors 
results.1 <- ahy.full.results[[5]]
results.1$results$beta

# Adult males have less probability of survival than females (-, significant) 
# Warmer winters (0.14) increase the probability of survival (+, significant)
# More effort increases the probability of recapture (+, significant)

# Remove mark files so they don't clog repo
invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Does winter_min_temp have a different effect in adult females and males?

# Create function
ahy.short <- function()
{
  Phi.sex <- list(formula = ~sex)
  Phi.sexWMinTemp <- list(formula = ~sex + winter_min_temp) 
  Phi.sexAndWMinTemp <- list(formula = ~ sex * winter_min_temp)
  
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
ahy.short.results <- ahy.short()
ahy.short.results

# Model with lowest Delta AIC
# Phi(~sex * winter_min_temp)p(~sex + effort) 0.0
# Followed by far by
# Phi(~sex + winter_min_temp)p(~sex + effort) 21.9

# Look at estimates and standard errors 
results.2 <- ahy.short.results[[2]]
results.2$results$beta

# Adult males have a lower probability of survival than females (–, significant)
# Warmer winters are associated with higher survival for adults males and females 
# (+, significant) 
# The positive effect of warmer winters is stronger for males (+ interaction, significant)
# More effort increases the probability of recapture (+, significant)

# Look at real estimates
results.2$results$real

# The estimates for survival of males are almost half of what they are for 
# females

# Remove mark files so they don't clog repo
invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))

