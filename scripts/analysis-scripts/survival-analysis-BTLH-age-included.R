# Survival analysis for Broad-tailed Hummingbird at Rocky Mountain National Park
# Including age at first capture 

# Edited from original code by Erin Zylstra
# Gaby Samaniego
# gabysamaniego@arizona.edu
# 2025-06-11

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

# ------------------ Capture histories with age at first capture ------------- #
# ------------------------ includes juveniles and adults --------------------- #

# Create capture histories
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
  select(year, aver_min_temp, aver_precip) %>% 
  rename(time = year) %>% 
  mutate(summer_min_temp = z.stand(aver_min_temp),
         summer_precip = z.stand(aver_precip), .keep = 'unused')

# ----------------- PROCESS CAPTURE HISTORIES FOR MARK ANALYSIS -------------- #

# Process capture histories
age.process <- process.data(data = ch.age,
                            model = 'CJS',
                            begin.time = 2003,
                            groups = c('sex', 'age'),
                            age.var = 2,  
                            # Indicates that the second variable in 'groups' (age) 
                            # should be used to assign initial ages and track aging 
                            # over time
                            initial.ages = c(0, 1)) 
                            # Assigns starting age values for each level of 'age':
                            # 0 for birds first captured as juveniles
                            # 1 for birds first captured as adults

# Create design matrix
age.ddl <- make.design.data(age.process)

# This is the key step!
# Create age classes for survival where juvenile = age 0 and adults = 1+
age.ddl <- add.design.data(data = age.process,
                           ddl = age.ddl,
                           parameter = 'Phi',
                           type = 'age',
                           bins = c(0,1,9), 
                           # Creates 2 age classes using internal Age variable:
                           # [0, 1) for juveniles (Age = 0)
                           # [1, 9) for adults (Age = 1 to 8)
                           # Age = 9 is excluded with right = FALSE. This is fine
                           # as we don't have birds that survived from 2003 to 2012
                           right = FALSE, 
                           # If FALSE (default), intervals are left-closed, right-open:
                           # [a, b) includes a, excludes b
                           # If TRUE, intervals would be right-closed: (a, b]
                           name = 'ageclass') 
                           # The new variable 'ageclass' will be added to the design data
                           # and can now be used in model formulas like:
                           # Phi ~ ageclass

# Add effort to ddl 
age.ddl$p <- merge_design.covariates(age.ddl$p, effort.z)

# Add winter covariates to ddl
age.ddl$Phi <- merge_design.covariates(
  age.ddl$Phi, winter)

# Add summer covaraites to ddl
age.ddl$Phi <- merge_design.covariates(
  age.ddl$Phi, summer)

# Create a couple of other variables to help with model construction

# This code creates a new grouping variable called 'sexadult' in the survival
# design data. It defines three distinct groups:
  # Juveniles (0, both sexes combined). This will be the intercept when interpreting coeficients
  # Adult females (1)
  # Adult males (2)
age.ddl$Phi$sexadult <- ifelse(age.ddl$Phi$ageclass == '[0,1)', 0,
                               ifelse(age.ddl$Phi$sex == 'F', 1, 2))

# This categorical variable lets us model survival using a three-level factor.
# The model: Phi ~ sexadult translates to:
  # One survival estimate for juveniles
  # One for adult females
  # One for adult males

# Change new variable to a class factor
age.ddl$Phi$sexadult <- factor(age.ddl$Phi$sexadult)

# Create indicator variable for adults were Juvenile = 0 and Adult = 1
# I think this will be helpful for plots later on?
age.ddl$Phi$adult <- ifelse(age.ddl$Phi$ageclass == '[0,1)', 0, 1)

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

# ---------------------------------- RUN MODELS ------------------------------ #

# Run models exploring effects of sex and age class on survival in a 'building
# up model complexity' strategy

# -------------------------------- Using ageclass ---------------------------- #

# Is survival different between juveniles and adults?

# Create function
base.age.sex.models.1 <- function()
{
  Phi.dot <- list(formula = ~1) # based model
  Phi.age <- list(formula = ~ageclass) 
  
  p.sexEffort <- list(formula = ~sex + effort)
  
  cml <- create.model.list('CJS') 
  results <- mark.wrapper(cml, 
                          data = age.process,
                          ddl = age.ddl,
                          output = FALSE,
                          adjust = FALSE)
  return(results)
}

# Run function and store the results in a marklist
base.age.sex.results.1 <- base.age.sex.models.1()
base.age.sex.results.1

# Model with lowest Delta AIC
# Phi(~ageclass)p(~sex + effort) 0.0

# Look at estimates and standard errors 
results.1 <- base.age.sex.results.1[[1]]
results.1$results$beta

# The probability of survival differs by age class were adults have a significantly
# higher probability of survival than juveniles. 

# The probability of recapture increases with banding effort and differs by sex were
# males have a lower probability of being recaptured than females.

# The data supports the use of age as a variable in the survival models based on
# SE 

# Remove mark files so they don't clog repo
invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))


# -------------------------------- Using sexadult ---------------------------- #

# Is survival different among juveniles (females and males), adult females
# and adult males?

# Create function
base.age.sex.models.2 <- function()
{
  Phi.dot <- list(formula = ~1) 
  Phi.age <- list(formula = ~ageclass) 
  Phi.adultSex <- list(formula = ~sexadult)
  
  p.sexEffort <- list(formula = ~sex + effort)
  
  cml <- create.model.list('CJS') 
  results <- mark.wrapper(cml, 
                          data = age.process,
                          ddl = age.ddl,
                          output = FALSE,
                          adjust = FALSE)
  return(results)
}

# Run function and store the results in a marklist
base.age.sex.results.2 <- base.age.sex.models.2()
base.age.sex.results.2

# Model with lowest Delta AIC
# Phi(~sexadult)p(~sex + effort) 0.0
# Followed by far by 
# Phi(~ageclass)p(~sex + effort) 47.9

# Look at estimates and standard errors 
results.2 <- base.age.sex.results.2[[1]]
results.2$results$beta

# Juveniles (intercept) have a significantly lower probability (-) of survival 
# than both adult females and adult males. Among adults, females have a 
# significantly higher probability of survival than males.

# The probability of recapture increases with banding effort and differs by sex were
# males have a lower probability of being recaptured than females.

# Remove mark files so they don't clog repo
invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))


# ----------------------------- Including covariates ------------------------- #

# --------- Using the covariate that best explained survival of adults ------- #

# Is survival of juveniles and adults affected differently by warmer winters in
# Mexico?

# Create function
base.age.sex.models.3 <- function()
{
  Phi.sexAdult <- list(formula = ~sexadult)
  Phi.sexAdultCovar <- list(formula = ~sexadult + winter_min_temp)
  
  p.sexEffort <- list(formula = ~sex + effort)
  
  cml <- create.model.list('CJS') 
  results <- mark.wrapper(cml, 
                          data = age.process,
                          ddl = age.ddl,
                          output = FALSE,
                          adjust = FALSE)
  return(results)
}

# Run function and store the results in a marklist
base.age.sex.results.3 <- base.age.sex.models.3()
base.age.sex.results.3

# Model with lowest Delta AIC
# Phi(~sexadult + winter_min_temp)p(~sex + effort) 0.0
# Followed by far by 
# Phi(~sexadult)p(~sex + effort) 27.6

# Look at estimates and standard errors 
results.3 <- base.age.sex.results.3[[2]]
results.3$results$beta

# Survival of juveniles, adult females and adult males increases whit warmer winters
# (+, significant)

# Look at real estimates
results.3$results$real

# Estimates for juveniles are around 0.2, for adult females are around 0.5
# and for adult males are around  0.3

# Remove mark files so they don't clog repo
invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Are juveniles more or less affected by warmer winters in Mexico than adult 
# females and adult males? 

# Create function
base.age.sex.models.4 <- function()
{
  Phi.sexAdult <- list(formula = ~sexadult)
  Phi.sexAdultPluCovar <- list(formula = ~sexadult + winter_min_temp)
  Phi.sexAdultxCovar <- list(formula = ~sexadult * winter_min_temp)
  
  p.sexEffort <- list(formula = ~sex + effort)
  
  cml <- create.model.list('CJS') 
  results <- mark.wrapper(cml, 
                          data = age.process,
                          ddl = age.ddl,
                          output = FALSE,
                          adjust = FALSE)
  return(results)
}

# Run function and store the results in a marklist
base.age.sex.results.4 <- base.age.sex.models.4()
base.age.sex.results.4

# Model with lowest Delta AIC
# Phi(~sexadult * winter_min_temp)p(~sex + effort) 0.0
# Followed by far by 
# Phi(~sexadult + winter_min_temp)p(~sex + effort) 28.1

# Look at estimates and standard errors 
results.4 <- base.age.sex.results.4[[3]]
results.4$results$beta

# Warmer winters are associated with increase survival in juveniles (+, but not 
# significant) 
# Adult females show a weaker response to warmer winters than juveniles 
# (- interaction, but not significant) 
# Adult males show stronger positive response to warmer winters compared to 
# juveniles (+ interaction, significant)

# Remove mark files so they don't clog repo
invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))


# --------------------- Adding a summer grounds covariate -------------------- #

# summer_min_temp was in the second best model for survival of adults, although
# the Delta AIC for that model was 9.3

# Is survival of juveniles and adults affected differently by warmer summers in
# Colorado?

# Create function
base.age.sex.models.5 <- function()
{
  Phi.sexAdult <- list(formula = ~sexadult)
  Phi.sexAdultPluCovar <- list(formula = ~sexadult + summer_min_temp)
  Phi.sexAdultxCovar <- list(formula = ~sexadult * summer_min_temp)
  
  p.sexEffort <- list(formula = ~sex + effort)
  
  cml <- create.model.list('CJS') 
  results <- mark.wrapper(cml, 
                          data = age.process,
                          ddl = age.ddl,
                          output = FALSE,
                          adjust = FALSE)
  return(results)
}

# Run function and store the results in a marklist
base.age.sex.results.5 <- base.age.sex.models.5()
base.age.sex.results.5

# Model with lowest Delta AIC
# Phi(~sexadult + summer_min_temp)p(~sex + effort) 0.0
# Closely followed by 
# Phi(~sexadult * summer_min_temp)p(~sex + effort) 2.52

# Look at estimates and standard errors 
results.5 <- base.age.sex.results.5[[2]]
results.5$results$beta

# Warmer summers are associated with a decrease in survival across all groups
# (-, significant) 
# Among groups, juveniles have lower probability of survival than adult females 
# and adult males (-, significant), adult males have better probability of 
# survival than juveniles (+, significant) but females have better probability 
# of survival than males (+, significant)

# Look at real estimates
results.5$results$real

# Estimates very similar to those in model with winter_min_temp

# When exploring the estimates for the model with the interaction, none were
# statistically significant

# Remove mark files so they don't clog repo
invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))

# ----------------- Combining summer and winter covariates ------------------- # 

# What happens with survival of juveniles, adult females and adult males when 
# considering temperature conditions in summer and winter?

# Create function
base.age.sex.models.6 <- function()
{
  Phi.sexAdultxCovarW <- list(formula = ~sexadult * winter_min_temp) 
  Phi.sexAdultPlusCovarS <- list(formula = ~sexadult + summer_min_temp)
  Phi.sexAdultCovarWPlusCovarS <- list(formula = ~sexadult + winter_min_temp + summer_min_temp)
  Phi.sexAdultxCovarWPlusCovarS <- list(formula = ~sexadult * winter_min_temp + summer_min_temp)
  Phi.sexAdultxCovarWPlusSexAdultxCovarS <- list(formula = ~sexadult * winter_min_temp + sexadult * summer_min_temp)
  
  p.sexEffort <- list(formula = ~sex + effort)
  
  cml <- create.model.list('CJS') 
  results <- mark.wrapper(cml, 
                          data = age.process,
                          ddl = age.ddl,
                          output = FALSE,
                          adjust = FALSE)
  return(results)
}

# Run function and store the results in a marklist
base.age.sex.results.6 <- base.age.sex.models.6()
base.age.sex.results.6

# Model with lowest Delta AIC
# Phi(~sexadult * winter_min_temp + summer_min_temp)p(~sex + effort) 0.0
# Followed by  
# Phi(~sexadult * winter_min_temp + sexadult * summer_min_temp)p(~sex + effort) 1.7

# Look at estimates and standard errors of best model
results.6 <- base.age.sex.results.6[[4]]
results.6$results$beta

# Adding summer min temp improved model fit.
# Warmer summers decrease survival probability for all groups! (-, significant) 

# Look at estimates of second best model 
results.7 <- base.age.sex.results.6[[5]]
results.7$results$beta

# There is no evidence that summer min temp affects survival differently among 
# groups. All interaction terms are not significant. 

# Remove mark files so they don't clog repo
invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))


# ---------------------- What if I try using precip? ------------------------- #

# Is precipitation in the wintering grounds affecting survival?

# Create function
base.age.sex.models.7 <- function()
{
  Phi.sexAdult <- list(formula = ~sexadult)
  Phi.sexAdultPluCovar <- list(formula = ~sexadult + winter_precip)
  Phi.sexAdultxCovar <- list(formula = ~sexadult * winter_precip)
  
  p.sexEffort <- list(formula = ~sex + effort)
  
  cml <- create.model.list('CJS') 
  results <- mark.wrapper(cml, 
                          data = age.process,
                          ddl = age.ddl,
                          output = FALSE,
                          adjust = FALSE)
  return(results)
}

# Run function and store the results in a marklist
base.age.sex.results.7 <- base.age.sex.models.7()
base.age.sex.results.7

# Model with lowest Delta AIC 
# Phi(~sexadult + winter_precip)p(~sex + effort) 0.0
# Followed by 
# Phi(~sexadult)p(~sex + effort) 2.7

# Look at estimates of best model
results.8 <- base.age.sex.results.7[[2]]
results.8$results$beta

# Precipitation slightly reduces the probability of survival for all groups (-,
# significant) 

# Remove mark files so they don't clog repo
invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Is precipitation in the summer grounds affecting survival?

# Create function
base.age.sex.models.8 <- function()
{
  Phi.sexAdult <- list(formula = ~sexadult)
  Phi.sexAdultPluCovar <- list(formula = ~sexadult + summer_precip)
  Phi.sexAdultxCovar <- list(formula = ~sexadult * summer_precip)
  
  p.sexEffort <- list(formula = ~sex + effort)
  
  cml <- create.model.list('CJS') 
  results <- mark.wrapper(cml, 
                          data = age.process,
                          ddl = age.ddl,
                          output = FALSE,
                          adjust = FALSE)
  return(results)
}

# Run function and store the results in a marklist
base.age.sex.results.8 <- base.age.sex.models.8()
base.age.sex.results.8

# Model with lowest Delta AIC 
# Phi(~sexadult * summer_precip)p(~sex + effort) 0.0
# Followed very closelly by 
# Phi(~sexadult + summer_precip)p(~sex + effort) 0.18

# Look at estimates of best model
results.9 <- base.age.sex.results.8[[3]]
results.9$results$beta

# There is no evidence that summer precip has an effect on survival probability 
# among groups. All interaction terms are not significant. 

# Looking at estimates of best second model
results.10 <- base.age.sex.results.8[[2]]
results.10$results$beta

# There is a small positive effect of summer precip on survival for all groups 
# (+ significant)

# Remove mark files so they don't clog repo
invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))

# Is it wise to add precip in both summer and winter grounds as covariates to my
# best model so far? Or due to the little effect (very small estimates) is it better
# to leave them out of the candidate models?

