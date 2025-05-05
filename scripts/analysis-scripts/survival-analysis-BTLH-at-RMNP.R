# Survival analysis for Broad-tailed Hummingbird at Rocky Mountain National Park
# Gaby Samaniego
# gabysamaniego@arizona.edu
# 2024-06-12

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

# ------------------ Capture histories with age at first capture ------------- #
# ------------------------ includes juveniles and adults --------------------- #

# Create capture histories
# (Not including location now -- can easily add in later)
ch.age <- dat %>%
  group_by(band, year, sex, age) %>%  
  summarize(n.observation = length(year), .groups = 'keep') %>%
  mutate(observed = 1) %>% 
  pivot_wider(names_from = year, 
              values_from = observed, 
              id_cols = c(band, sex, age), 
              values_fill = 0) %>% 
  relocate(band, '2003','2004','2005','2006','2007','2008','2009','2010','2011', 
           '2012', sex, age) %>%
  unite(ch, c('2003','2004','2005','2006','2007','2008','2009','2010', '2011','2012'), 
        sep = '') %>% 
  mutate(age = if_else(age == 'AHY', 'adult', 'young')) %>% 
  as.data.frame() %>% 
  select(-band)

# Make several variables factors (specifying levels for clarity)
ch.age$sex <- factor(ch.age$sex, levels = c('F', 'M'))
ch.age$age <- factor(ch.age$age, levels = c('young', 'adult'))

# Checks
head(ch.age)
str(ch.age)

# ------------------------------ PRPEPARE COVARIATES ------------------------- #

# From Erin about effort:
# It's usually a good idea to standardize covariates (to avoid estimation problems
# and to help with interpretation). If effort is standardized to a mean of 0 and
# SD = 1, then the intercept will represent recapture probability at the mean
# effort level and the coefficient represents the expected change in recapture 
# probability for a 1-SD increase in effort.

# Createa function to z-standardize the covariates
z.stand <- function(x) {
  (x - mean(x)) / sd(x)
}

# -------------------------------- Trapping Effort --------------------------- #

# Load effort data
effort.raw <- read.csv('output/banding-effort-data/banding-effort-all-sites-RMNP.csv')
# Total banding days per year

# Prepare effort data to add to ddl
effort.df <- effort.raw %>% 
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
summer.co <- read.csv('output/weather-data/covariates-output/summer-covar-colorado.csv')
winter.co <- read.csv('output/weather-data/covariates-output/winter-swe-colorado.csv')

# Standardize winter mx covariates
winter.mx.stand <- winter.mx %>% 
  mutate(aver_min_temp_z = z.stand(aver_min_temp),
         aver_daily_min_temp_z = z.stand(aver_daily_min_temp), 
         aver_cold_days_z = z.stand(aver_cold_days),
         aver_precip_z = z.stand(aver_precip),
         average_ndvi_z = z.stand(average_ndvi), .keep = 'unused') %>% 
  mutate(time = 2002:2011, .after = winter_period) %>% # so time matches Phi
  select(-winter_period)

# Standardize summer co covariates
summer.co.stand.2 <- summer.co %>% 
  mutate(aver_max_temp_z = z.stand(aver_max_temp),
         aver_min_temp_z = z.stand(aver_min_temp),
         aver_daily_max_temp_z = z.stand(aver_daily_max_temp),
         aver_daily_min_temp_z = z.stand(aver_daily_min_temp),
         aver_warm_days_z = z.stand(aver_warm_days),
         aver_cold_days_z = z.stand(aver_cold_days),
         aver_precip_z = z.stand(aver_precip),
         average_ndvi_z = z.stand(average_ndvi),
         frost_days_z = z.stand(frost_days), .keep = 'unused')

# Standardize winter co covariates
winter.co.stand <- winter.co %>% 
  mutate(total_swe_winter_co_z = z.stand(total_swe_winter_co), .keep = 'unused') %>% 
  mutate(time = 2002:2012) %>% 
  select(-winter_period)

# ------------------------------ RUN CJS ANALYSIS ---------------------------- #
# --------------------- Without environmental covariates --------------------- #

# 1) ADULTS

# Process the encounter history data frame for Mark analysis
adults.process <- process.data(ch.adults,
                               model = 'CJS',
                               begin.time = 2003,
                               groups = c('sex','location')) 

# Create design data frame for Mark model specification based in PIM (parameter 
# index matrix)
adults.ddl <- make.design.data(adults.process)

# Add effort to ddl 
adults.ddl$p <- merge_design.covariates(adults.ddl$p, effort.df)

# Run models using a function

# Create a function that defines and runs a set of models and returns a marklist 
# with the results and a model table. It uses the processed and designed data 
# created previously 

adults.models <- function()
{
  Phi.dot <- list(formula = ~1) # intercept/dot 
  Phi.Time <- list(formula = ~Time) # trend 
  Phi.sex <- list(formula = ~sex)
  Phi.time <- list(formula = ~time) # each year
  Phi.sexPlusTime <- list(formula = ~sex + Time)
  Phi.sexandTime <- list(formula = ~sex * Time)
  Phi.location <- list(formula = ~location)
  
  p.dot <- list(formula = ~1)
  p.sex <- list(formula = ~sex)
  p.time <- list(formula = ~time)
  p.effort <- list(formula = ~effort)
  p.timePluseffort <- list(formula = ~time + effort)
  p.timePlusEffortPluslussex <- list(formula = ~time + sex)
  p.location <- list(formula = ~location)
  
  # Create a data frame of all combinations of parameter specifications for each 
  # parameter
  cml <- create.model.list('CJS')  
  
  # Construct and run a set of MARK models from the cml data frame
  results <- mark.wrapper(cml, 
                          data = adults.process,
                          ddl = adults.ddl,
                          adjust = FALSE) # Accepts the parameter counts from MARK
  return(results)
}

# Run the function and store the results in a marklist
adults.results <- adults.models()
adults.results

# Two models with lowest Delta AIC of 0. 
# Phi(~sex * Time)p(~time)
# Phi(~sex * Time)p(~time + effort) 

# Using model 26
best <- adults.results[[26]]

# Look at beta-hats
best$results$beta
best$results$real

# Adult male broad-tailed hummingbirds' survival probability in RMNP has decreased 
# over time (β = -0.05). Males have a lower probability of survival (β = -0.47) 
# than females. The top model used for inference shows a time effect 
# on the probability of recapture.    

# Extract real estimates, separate row names into useful columns and do a little 
# clean up
reals <- best$results$real %>%
  rownames_to_column('rowname') %>%
  select(-c(fixed, note)) %>%
  separate_wider_delim(rowname,  # Splits this column into different columns
                       delim = ' ',
                       names = c('parameter', 'sex', NA, NA, 'year')) %>%
  mutate(sex = str_sub(sex, 2, 2), 
         year = as.numeric(str_sub(year, 2, 5))) %>%
  data.frame()

# Visualize results

# Plot estimates of recapture probability
p.reals <- reals %>%
  filter(parameter == 'p') 

p.fig <- ggplot(p.reals, aes(x = year, y = estimate)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0) +
  theme_classic() +
  ylab(expression(atop('Estimated recapture probability', paste('(95% CI)')))) + 
  xlab('Year') +
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(breaks = 2004:2012)
p.fig

# Plot estimates of survival probabilities 
phi.reals <- reals %>%
  filter(parameter == 'Phi')

phi.fig <- ggplot(phi.reals, aes(x = year, y = estimate)) +
  geom_point(size = 1.5, aes(color = sex)) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0) +
  scale_color_manual(values = c('#8E3E6E', '#4F7942')) +
  scale_fill_manual (values = c('#8E3E6E', '#4F7942')) +
  theme_classic() +
  ylab(expression(atop('Estimated survival probability', paste('(95% CI)')))) + 
  xlab('Year') +
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(breaks = 2003:2011)
phi.fig

# Remove mark files so they don't clog repo
invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))


# 2) ADULTS AND JUVENILES

# Process the encounter history data frame for Mark analysis
age.process <- process.data(data = ch.age,
                            model = 'CJS',
                            begin.time = 2003,
                            groups = c('sex', 'age'),
                            age.var = 2,  # Provides the index for age variable in groups vector 
                            initial.ages = c(0, 1)) # Initial ages for each level of age variable

# Create design matrix
age.ddl <- make.design.data(age.process)

# This is the key step: creating age classes for survival: juvenile = age 0, 
#                                                          adult = 1+
age.ddl <- add.design.data(data = age.process,
                           ddl = age.ddl,
                           parameter = 'Phi',
                           type = 'age',
                           bins = c(0,1,9), 
                           right = FALSE, # Determines how bins are truncated, If TRUE, bin intervals are closed on the right
                                          # so the intervals are 0 young, 1 to 9 adult
                           name = 'ageclass') 
                           
# At this point, not planning on including age or age classes in recapture model, 
# since all individuals are at least 1 year old (adults) when recaptured.

# Add effort to ddl 
age.ddl$p <- merge_design.covariates(age.ddl$p, effort.stand)

# Create a couple other variables to help with model construction
# Why these groups? 

# Creating 3 groups: Juveniles (0; both sexes combined) 
#                    AdultF (1) 
#                    AdultM (2)
age.ddl$Phi$sexadult <- ifelse(age.ddl$Phi$ageclass == '[0,1)', 0,
                               ifelse(age.ddl$Phi$sex == 'F', 1, 2))

# Change new variable to a class factor
age.ddl$Phi$sexadult <- factor(age.ddl$Phi$sexadult)

# Indicator for adults: Juvenile (0)
#                       Adult (1)
age.ddl$Phi$adult <- ifelse(age.ddl$Phi$ageclass == '[0,1)', 0, 1)

#Why this one was not a factor? Should it be a factor?
# Change new variable to a class factor
age.ddl$Phi$adult <- factor(age.ddl$Phi$adult)

# Inspect the newly created RMark objects
str(age.ddl)

# Phi
head(age.ddl$Phi, 10)
tail(age.ddl$Phi, 10)
summary(age.ddl$Phi)

# p
head(age.ddl$p, 10)
summary(age.ddl$p)

# Run models exploring effects of sex and age class on survival (no time/Trends 
# yet, except in recapture probability model)

# Create function
age.sex.models <- function()
{
  Phi.dot <- list(formula = ~1) 
  Phi.sex <- list(formula = ~sex)
  Phi.age <- list(formula = ~ageclass)
  Phi.agePlusSex <- list(formula = ~ageclass + sex)
  Phi.ageXSex <- list(formula = ~ageclass * sex)
  Phi.adultSex <- list(formula = ~sexadult)
  
  p.dot <- list(formula = ~1)
  p.time <- list(formula = ~time)
  p.effort <- list(formula = ~effort)
  p.sex <- list(formula = ~sex)
  p.timePlusSex <- list(formula = ~time + sex)
  p.effortPlusSex <- list(formula = ~effort + sex)
  
  cml <- create.model.list('CJS') 
  results <- mark.wrapper(cml, 
                          data = age.process,
                          ddl = age.ddl,
                          output = FALSE,
                          adjust = FALSE)
  return(results)
}

# Run function and store the results in a marklist
age.sex.results <- age.sex.models()
age.sex.results

# Pick index of model you'd like to look at more closely
model.ind <- 17

# Look at estimates from one of the models and interpret coefficients
age.sex.results[[model.ind]]$results$beta

# Using model 17 
# Phi(~ageclass + sex)p(~time)

# The probability of survival is different by age class and sex. Adults have a 
# higher probability of survival than juveniles and males have a lower probability
# of survival than females. Recapture probability differs by year 

# Look at real estimates
age.sex.results[[model.ind]]$results$real

# Code below organizes the real estimates for phi and p into new data frames

# Extract real estimates and add columns to identify grouping variables
real.ests <- age.sex.results[[model.ind]]$results$real %>%
  rownames_to_column(var = 'group') %>%
  mutate(param = ifelse(str_sub(group, 1, 3) == 'Phi', 'Phi', 'p'),
         group = ifelse(param == 'Phi', 
                        str_remove(group, 'Phi '), 
                        str_remove(group, 'p ')),
         year = as.numeric(str_sub(group, -4, -1)),
         a = str_sub(group, -7, -7),
         age = ifelse(a == '0', 'J', 'A'),
         sexMF = str_sub(group, 2, 2)) %>%
  select(param, year, age, sexMF, estimate, se, lcl, ucl)

real.ests.Phi <- real.ests %>%
  filter(param == 'Phi')

real.ests.p <- real.ests %>%
  filter(param == 'p') %>%
  select(-age)

# Create dataframe to organize the real survival estimates
time.range <- min(age.ddl$Phi$Time):max(age.ddl$Phi$Time)
n.years <- length(time.range)

Phi.ests <- tibble(Time = rep(time.range, 4),
                   ageclass = rep(c(0, 0, 1, 1), each = n.years),
                   sex = rep(c(0, 1, 0, 1), each = n.years)) %>%
  mutate(year = Time + 2003,
         age = ifelse(ageclass == 0, 'J', 'A'),
         sexMF = ifelse(sex == 0, 'F', 'M')) %>% 
  left_join(real.ests.Phi, by = c('year', 'age', 'sexMF')) %>%
  filter(!is.na(estimate)) %>%
  group_by(age, sexMF) %>%
  mutate(nyears = length(year)) %>%
  ungroup() %>%
  data.frame() %>%
  mutate(year = ifelse(nyears == 1, 'All years', as.character(year))) %>%
  select(-c(Time, ageclass, sex, nyears)) %>%
  relocate(param)

# Create a data frame to organize the real recapture probability estimates
p.ests <- data.frame(Time = rep(time.range, 2),
                     sex = rep(c(0, 1), each = n.years)) %>%
  mutate(year = Time + 2003,
         sexMF = ifelse(sex == 0, 'F', 'M')) %>%
  left_join(real.ests.p, by = c('year', 'sexMF')) %>%
  filter(!is.na(estimate)) %>%
  group_by(sexMF) %>%
  mutate(nyears = n()) %>%
  ungroup() %>%
  mutate(year = ifelse(nyears == 1, 'All years', as.character(year))) %>%
  select(-c(Time, sex, nyears)) %>%
  relocate(param)

Phi.ests
p.ests
# If just one sex is listed, then that means that males and females were
# assumed to have the same survival (or recapture) probability

# Remove mark files so they don't clog repo
invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))

# ------------------------------ RUN CJS ANALYSIS ---------------------------- #
# ----------------------- With environmental covariates ---------------------- #

################################################################################

# But first select covariates when more than one was summarized, for example:
# NDVI vs precipitation 
# aver_min_temp vs daily_aver_min_temp

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Which temperature variable in the wintering grounds better explains survival?

# 1) Run models exploring effects of covariates in adult survival

# Process the encounter history data frame for Mark analysis
adults.temperature.winter.process <- process.data(ch.adults,
                                                  model = 'CJS',
                                                  begin.time = 2003,
                                                  groups = 'sex') 

# Create design data frame for Mark model specification based in PIM (parameter 
# index matrix)
adults.temperature.winter.ddl <- make.design.data(adults.temperature.winter.process)

# Add weather covariates to ddl 
adults.temperature.winter.ddl$Phi <- merge_design.covariates(
  adults.temperature.winter.ddl$Phi, winter.mx.stand)

# Add effort to ddl 
adults.temperature.winter.ddl$p <- merge_design.covariates(
  adults.temperature.winter.ddl$p, effort.df)

# Run models using a function

# I'm using the same function I used before but adding temperature. I'm not including
# interactions or additive effects
adults.temp.winter.models <- function()
{
  Phi.dot <- list(formula = ~1) # intercept/dot 
  Phi.Time <- list(formula = ~Time) # trend 
  Phi.sex <- list(formula = ~sex)
  Phi.time <- list(formula = ~time) # each year
  Phi.aver_min_temp_z <- list(formula = ~aver_min_temp_z)
  Phi.aver_daily_min_temp_z <- list(formula = ~aver_daily_min_temp_z)
  
  p.dot <- list(formula = ~1)
  p.sex <- list(formula = ~sex)
  p.time <- list(formula = ~time)
  p.effort <- list(formula = ~effort)
  
  # Create a data frame of all combinations of parameter specifications for each 
  # parameter
  cml <- create.model.list('CJS')  
  
  # Construct and run a set of MARK models from the cml data frame
  results <- mark.wrapper(cml, 
                          data = adults.temperature.winter.process,
                          ddl = adults.temperature.winter.ddl,
                          adjust = FALSE) # Accepts the parameter counts from MARK
  return(results)
}

# Run the function and store the results in a marklist
adults.temperature.results <- adults.temp.winter.models()
adults.temperature.results

# Looking at Delta AIC:
# Neither temperature variable included in the formulas explain survival. 
# aver_min_temp performed better than aver_daily_min_temp by a little bit

# Maybe I need to try just with formulas for temperature

# Run models using a function
just.temp.winter.models <- function()
{
  Phi.dot <- list(formula = ~1)
  Phi.aver_min_temp_z <- list(formula = ~aver_min_temp_z)
  Phi.aver_daily_min_temp_z <- list(formula = ~aver_daily_min_temp_z)
  
  p.dot <- list(formula = ~1)
  p.time <- list(formula = ~time)
  
  # Create a data frame of all combinations of parameter specifications for each 
  # parameter
  cml <- create.model.list('CJS')  
  
  # Construct and run a set of MARK models from the cml data frame
  results <- mark.wrapper(cml, 
                          data = adults.temperature.winter.process,
                          ddl = adults.temperature.winter.ddl,
                          adjust = FALSE) # Accepts the parameter counts from MARK
  return(results)
}

# Run the function and store the results in a marklist
just.temp.winter.results <- just.temp.winter.models()
just.temp.winter.results

# aver_daily_min_temp explains more than aver_min_temp, but before it was the
# oposite based in the position of the models in the table of results
# Phi(~aver_daily_min_temp_z)p(~time)
two <- just.temp.winter.results[[2]]
two$results$beta


# Remove mark files so they don't clog repo
invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))
