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


# ------------------------------- Run full model ----------------------------- #

# But first, check for correlation of cavariates 
covars <- left_join(winter, summer, by = 'time')

# Between winter and summer min temp
cor.test(covars$winter_min_temp, covars$summer_min_temp)
# No correlation, r= -0.016, p = 0.96

# Between winter and summer precip
cor.test(covars$winter_precip, covars$summer_precip)
# No correlation, r = 0.042, p = 0.91

# Create function
base.age.sex.full <- function() 
{
  Phi.full <- list(formula = ~sexadult * winter_min_temp + summer_min_temp +
                     winter_precip + summer_precip)
  
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
base.age.sex.full.results <- base.age.sex.full()

# Look at estimates
results.10 <- base.age.sex.full.results[[1]]
results.10$results$beta

# Phi:
# Juveniles have a lower probability of survival than adult females and adult males 
# (-, significant)
# Adult females have a higher probability of survival than both juveniles and 
# adult males (+, significant)
# Adult males have a higher probability of survival than juveniles but lower than 
# adult females (+, significant)
# Warmer summers decrease the probability of survival across all groups 
# (-, significant)
# Warmer winters do not affect survival across all groups (not significant)
# Neither winter nor summer precipitation affect survival across all groups 
# (not significant)

# p:
# Increase banding effort improves the probability of recapture (+, significant)
# Sex does not have an effect on recapture probability (-, not significant)

# Remove mark files so they don't clog repo
invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))


# --------------------------------- PLOT FINDINGS ---------------------------- #

# --------- Prepare data frames with real estimates for Phi and p ------------ #

# Extract real estimates and clean them up
real.ests <- results.10$results$real %>% 
  rownames_to_column(var = 'group') %>%
  mutate(parameter = ifelse(str_sub(group, 1, 3) == 'Phi', 'Phi', 'p'),
         group = ifelse(parameter == 'Phi', 
                        str_remove(group, 'Phi '), 
                        str_remove(group, 'p ')),
         year = as.numeric(str_sub(group, -4, -1)),
         a = str_sub(group, -7, -7),
         age = ifelse(a == '0', 'J', 'A'),
         sexMF = str_sub(group, 2, 2)) %>%
  select(parameter, year, age, sexMF, estimate, se, lcl, ucl)

# Extract real Phi estimates
real.Phi.ests <- real.ests %>%
  filter(parameter == 'Phi')

# Extract real p estimates
real.p.ests <- real.ests %>%
  filter(parameter == 'p') %>%
  select(-age)

# -------------------------------- Create plots ------------------------------ # 
# --------------------------------- Phi and p -------------------------------- #

# 1) Probability of survival of juveniles, adult females and adult males over time

# Prepare labels for plotting
real.Phi.ests <- real.Phi.ests %>%
  mutate(group = case_when(age == 'J' ~ 'Juvenile',
                           age == 'A' & sexMF == 'F' ~ 'Adult Female',
                           age == 'A' & sexMF == 'M' ~ 'Adult Male'))

# Plot survival probability
Phi.plot <- ggplot(real.Phi.ests, aes(x = as.numeric(year), 
                                  y = estimate, 
                                  color = group)) +
  geom_line(aes(group = group), size = 0.3) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0, linewidth = 0.3) +
  scale_color_manual(values = c('Juvenile' = '#505050', 
                                'Adult Female' = '#6B8E23', 
                                'Adult Male' = '#C71585')) +
  scale_y_continuous(limits = c(0,1), 
                     breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(breaks = 2003:2011) +
  labs(x = 'Year',
       y = 'Estimated anual survival probability\n(95% CI)',
       color = 'Group') +
  theme_classic() +
  theme(legend.position = 'right',
        plot.title = element_text(hjust = 0.5))
Phi.plot

# 2) Recapture probability of females and males over time

# Prepare labels for plotting
real.p.ests <- real.p.ests %>%
  mutate(group = ifelse(sexMF == 'F', 'Female', 'Male'))

# Plot recapture probability
p.fig <- ggplot(real.p.ests, aes(x = as.numeric(year), 
                            y = estimate, 
                            color = group)) +
  geom_line(aes(group = group), size = 0.3) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0, linewidth = 0.5) +
  scale_color_manual(values = c('Female' = '#6B8E23', 
                                'Male' = '#C71585')) +
  theme_classic() +
  labs(y = 'Estimated recapture probability\n(95% CI)',
       x = 'Year') +
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(breaks = 2003:2012) +
  theme(legend.title = element_blank())
p.fig

# ---------------------------- Effect of covariates -------------------------- #


# Original base code from Erin Zylstra using BTLH data from Mount Lemmon
# Plots for NAEP poster

# Code adapted and expanded using ChatGPT

# ------------------- First, prepare data needed for the plots --------------- #

# Extract Phi beta estimates
phi.betas <- results.10$results$beta %>%
  as.data.frame() %>%
  rownames_to_column('term') %>%
  filter(str_starts(term, 'Phi:')) %>%
  mutate(term = str_remove(term, 'Phi:'),
         term = str_replace(term, '\\(Intercept\\)', 'Intercept')) 

# Create a range of values for all covariates to plot

# Create function to build ranges for each covariate
build.range <- function(data, covar, n = 100) { # 100 seems standard? 
  values <- data[[covar]]  # Get the column named in 'covar' as a vector using [[ ]]
  seq(min(values), max(values), length.out = n)
}

# Create ranges 
winter.temp.range <- build.range(covars, 'winter_min_temp')
summer.temp.range <- build.range(covars, 'summer_min_temp')
winter.precip.range <- build.range(covars, 'winter_precip')
summer.precip.range <- build.range(covars, 'summer_precip')

# Extract beta estimates with lower and upper CIs for Phi parameter

# Create beta vectors from phi.betas
betas     <- phi.betas$estimate
betas.lcl <- phi.betas$lcl
betas.ucl <- phi.betas$ucl

# Assign names to the beta vectors using the 'term' column in phi.betas
# This ensures that the coefficients match the correct columns in the 
# prediction data frames during matrix multiplication. Without the names funtion
# I always got an error
names(betas)     <- phi.betas$term
names(betas.lcl) <- phi.betas$term
names(betas.ucl) <- phi.betas$term

# -------------------------------- Create plots ------------------------------ #

# 3) Effect of winter min temp on probability of survival on each group (juveniles,
# adult females and adult males)

# Build prediction data frame
pred.df.winter.temp <- data.frame(
  Group = rep(c('Juvenile', 'Adult Female', 'Adult Male'), 
              each = length(winter.temp.range)),
  winter_min_temp = rep(winter.temp.range, times = 3)) # 3 for each group

# Add other covariates to predict data frame, holding them constant at 0 
# (standardized mean) 
pred.df.winter.temp$Intercept <- 1 
# Dummy variables for adult female and adult male
pred.df.winter.temp$sexadult1 <- ifelse(pred.df.winter.temp$Group == 'Adult Female', 1, 0)
pred.df.winter.temp$sexadult2 <- ifelse(pred.df.winter.temp$Group == 'Adult Male', 1, 0)
pred.df.winter.temp$summer_min_temp <- 0
pred.df.winter.temp$winter_precip <- 0
pred.df.winter.temp$summer_precip <- 0

# Include interaction terms. Let the effect of temperature depend on sex
# Use `` (`sexadult2:winter_min_temp`) so the code works!
pred.df.winter.temp$`sexadult1:winter_min_temp` <- 
  pred.df.winter.temp$sexadult1 * pred.df.winter.temp$winter_min_temp
pred.df.winter.temp$`sexadult2:winter_min_temp` <- 
  pred.df.winter.temp$sexadult2 * pred.df.winter.temp$winter_min_temp

# Predict survival on the logit scale
# Selecting columns by names ensures that the order of variables in the 
# prediction data frame matches the order of the beta coefficients
estimate.winter.temp.logit <- as.matrix(pred.df.winter.temp[, names(betas)]) %*% 
  as.matrix(betas)
lcl.winter.temp.logit <- as.matrix(pred.df.winter.temp[, names(betas.lcl)]) %*% 
  as.matrix(betas.lcl)
ucl.winter.temp.logit <- as.matrix(pred.df.winter.temp[, names(betas.ucl)]) %*% 
  as.matrix(betas.ucl)

# Convert to probability scale
pred.df.winter.temp$estimate <- plogis(estimate.winter.temp.logit)
pred.df.winter.temp$lcl <- plogis(lcl.winter.temp.logit)
pred.df.winter.temp$ucl <- plogis(ucl.winter.temp.logit)

# Plot
winter.temp.plot <- ggplot(pred.df.winter.temp, aes(x = winter_min_temp, 
                                                    y = estimate, 
                                                    color = Group, 
                                                    fill = Group)) +
  geom_line(size = 0.5) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.1, color = NA) +
  labs(x = 'Winter minimum temperature (standardized)',
       y = 'Estimated survival probability\n(95% CI)',
       title = 'Effect of Winter Minimum Temperature on Survival') +
  scale_color_manual(values = c('Juvenile' = '#505050', 
                                'Adult Female' = '#6B8E23', 
                                'Adult Male' = '#C71585')) +
  scale_fill_manual(values = c('Juvenile' = '#505050', 
                               'Adult Female' = '#6B8E23', 
                               'Adult Male' = '#C71585')) +
  theme_classic()
winter.temp.plot

# 4) Effect of summer min temp on probability of survival of all groups

# Build a prediction data frame
pred.df.summer.temp <- data.frame(
  Group = rep(c('Juvenile', 'Adult Female', 'Adult Male'), 
              each = length(summer.temp.range)),
  summer_min_temp = rep(summer.temp.range, times = 3)) 

# Add other covariates to predict data frame, holding them constant at 0 
# (standardized mean) 
pred.df.summer.temp$Intercept <- 1
pred.df.summer.temp$sexadult1 <- ifelse(pred.df.summer.temp$Group == 'Adult Female', 1, 0)
pred.df.summer.temp$sexadult2 <- ifelse(pred.df.summer.temp$Group == 'Adult Male', 1, 0)
pred.df.summer.temp$winter_min_temp <- 0
pred.df.summer.temp$winter_precip <- 0
pred.df.summer.temp$summer_precip <- 0
pred.df.summer.temp$`sexadult1:winter_min_temp` <- 0 # Use `` so the code works!
pred.df.summer.temp$`sexadult2:winter_min_temp` <- 0

# Predict survival on the logit scale
# Matrix multiplication
estimate.summer.temp.logit <- as.matrix(pred.df.summer.temp[, names(betas)]) %*% as.matrix(betas)
lcl.summer.temp.logit <- as.matrix(pred.df.summer.temp[, names(betas.lcl)]) %*% as.matrix(betas.lcl)
ucl.summer.temp.logit <- as.matrix(pred.df.summer.temp[, names(betas.ucl)]) %*% as.matrix(betas.ucl)

# Convert to probability scale
pred.df.summer.temp$estimate <- plogis(estimate.summer.temp.logit)
pred.df.summer.temp$lcl <- plogis(lcl.summer.temp.logit)
pred.df.summer.temp$ucl <- plogis(ucl.summer.temp.logit)

# Average predictions for each group
aver.df.summer.temp <- pred.df.summer.temp %>%
  group_by(summer_min_temp) %>%
  summarize(estimate = mean(estimate),
            lcl = mean(lcl),
            ucl = mean(ucl), .groups = 'drop')

# Plot for each group
summer.temp.plot.all <- ggplot(pred.df.summer.temp, aes(x = summer_min_temp, 
                                                        y = estimate, 
                                                        color = Group, 
                                                        fill = Group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.1, color = NA) +
  labs(x = 'Summer minimum temperature (standardized)',
       y = 'Estimated survival probability\n(95% CI)',
       title = 'Effect of Summer Minimum Temperature on Survival') +
  scale_color_manual(values = c(
    'Juvenile' = '#505050', 
    'Adult Female' = '#6B8E23', 
    'Adult Male' = '#C71585')) +
  scale_fill_manual(values = c(
    'Juvenile' = '#505050', 
    'Adult Female' = '#6B8E23', 
    'Adult Male' = '#C71585')) +
  theme_classic()
summer.temp.plot.all

# Plot average
aver.summer.temp.plot <- ggplot(aver.df.summer.temp, aes(x = summer_min_temp, 
                                                    y = estimate)) +
  geom_line(color = "#444444", size = 0.5) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), fill = "#999999", alpha = 0.3) +
  labs(x = 'Summer minimum temperature (standardized)',
       y = 'Estimated average survival probability\n(95% CI)',
       title = 'Average Effect of Summer Min Temperature on Survival') +
  theme_classic()
aver.summer.temp.plot

# 5) Effect of summer precip on probability of survival of all groups

# Build a prediction data frame
pred.df.summer.precip <- data.frame(
  Group = rep(c('Juvenile', 'Adult Female', 'Adult Male'), 
              each = length(summer.precip.range)),
  summer_precip = rep(summer.precip.range, times = 3)) 

# Add other covariates to predict data frame, holding them constant at 0 
# (standardized mean) 
pred.df.summer.precip$Intercept <- 1
pred.df.summer.precip$sexadult1 <- ifelse(pred.df.summer.precip$Group == 'Adult Female', 1, 0)
pred.df.summer.precip$sexadult2 <- ifelse(pred.df.summer.precip$Group == 'Adult Male', 1, 0)
pred.df.summer.precip$winter_min_temp <- 0
pred.df.summer.precip$winter_precip <- 0
pred.df.summer.precip$summer_min_temp <- 0
pred.df.summer.precip$`sexadult1:winter_min_temp` <- 0 # Use `` so the code works!
pred.df.summer.precip$`sexadult2:winter_min_temp` <- 0

# Predict survival on the logit scale
# Matrix multiplication
estimate.summer.precip.logit <- as.matrix(pred.df.summer.precip[, names(betas)]) %*% as.matrix(betas)
lcl.summer.precip.logit <- as.matrix(pred.df.summer.precip[, names(betas.lcl)]) %*% as.matrix(betas.lcl)
ucl.summer.precip.logit <- as.matrix(pred.df.summer.precip[, names(betas.ucl)]) %*% as.matrix(betas.ucl)

# Convert to probability scale
pred.df.summer.precip$estimate <- plogis(estimate.summer.precip.logit)
pred.df.summer.precip$lcl <- plogis(lcl.summer.precip.logit)
pred.df.summer.precip$ucl <- plogis(ucl.summer.precip.logit)

# Average predictions for each group
aver.df.summer.precip <- pred.df.summer.precip %>%
  group_by(summer_precip) %>%
  summarize(estimate = mean(estimate),
            lcl = mean(lcl),
            ucl = mean(ucl), .groups = 'drop')

# Plot for each group
summer.precip.plot.all <- ggplot(pred.df.summer.precip, 
                                 aes(x = summer_precip, 
                                     y = estimate, 
                                     color = Group, 
                                     fill = Group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.1, color = NA) +
  labs(x = 'Summer precipitation (standardized)',
       y = 'Estimated survival probability\n(95% CI)',
       title = 'Effect of Summer Precipitation on Survival') +
  scale_color_manual(values = c(
    'Juvenile' = '#505050', 
    'Adult Female' = '#6B8E23', 
    'Adult Male' = '#C71585')) +
  scale_fill_manual(values = c(
    'Juvenile' = '#505050', 
    'Adult Female' = '#6B8E23', 
    'Adult Male' = '#C71585')) +
  theme_classic()
summer.precip.plot.all

# Plot average
aver.summer.precip.plot <- ggplot(aver.df.summer.precip, 
                                  aes(x = summer_precip, 
                                      y = estimate)) +
  geom_line(color = "#444444", size = 0.5) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), fill = "#999999", alpha = 0.3) +
  labs(x = 'Summer precipitation (standardized)',
       y = 'Estimated average survival probability\n(95% CI)',
       title = 'Average Effect of Summer Precipitation on Survival') +
  theme_classic()
aver.summer.precip.plot

# 6) Effect of winter precip on probability of survival of all groups

# Build a prediction data frame
pred.df.winter.precip <- data.frame(
  Group = rep(c('Juvenile', 'Adult Female', 'Adult Male'), 
              each = length(winter.precip.range)),
  winter_precip = rep(winter.precip.range, times = 3)) 

# Add other covariates to predict data frame, holding them constant at 0 
# (standardized mean) 
pred.df.winter.precip$Intercept <- 1
pred.df.winter.precip$sexadult1 <- ifelse(pred.df.winter.precip$Group == 'Adult Female', 1, 0)
pred.df.winter.precip$sexadult2 <- ifelse(pred.df.winter.precip$Group == 'Adult Male', 1, 0)
pred.df.winter.precip$winter_min_temp <- 0
pred.df.winter.precip$summer_precip <- 0
pred.df.winter.precip$summer_min_temp <- 0
pred.df.winter.precip$`sexadult1:winter_min_temp` <- 0 # Use `` so the code works!
pred.df.winter.precip$`sexadult2:winter_min_temp` <- 0

# Predict survival on the logit scale
# Matrix multiplication
estimate.winter.precip.logit <- as.matrix(pred.df.winter.precip[, names(betas)]) %*% as.matrix(betas)
lcl.winter.precip.logit <- as.matrix(pred.df.winter.precip[, names(betas.lcl)]) %*% as.matrix(betas.lcl)
ucl.winter.precip.logit <- as.matrix(pred.df.winter.precip[, names(betas.ucl)]) %*% as.matrix(betas.ucl)

# Convert to probability scale
pred.df.winter.precip$estimate <- plogis(estimate.winter.precip.logit)
pred.df.winter.precip$lcl <- plogis(lcl.winter.precip.logit)
pred.df.winter.precip$ucl <- plogis(ucl.winter.precip.logit)

# Average predictions for each group
aver.df.winter.precip <- pred.df.winter.precip %>%
  group_by(winter_precip) %>%
  summarize(estimate = mean(estimate),
            lcl = mean(lcl),
            ucl = mean(ucl), .groups = 'drop')

# Plot for each group
winter.precip.plot.all <- ggplot(pred.df.winter.precip, 
                                 aes(x = winter_precip, 
                                     y = estimate, 
                                     color = Group, 
                                     fill = Group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.1, color = NA) +
  labs(x = 'Winter precipitation (standardized)',
       y = 'Estimated survival probability\n(95% CI)',
       title = 'Effect of Winter Precipitation on Survival') +
  scale_color_manual(values = c(
    'Juvenile' = '#505050', 
    'Adult Female' = '#6B8E23', 
    'Adult Male' = '#C71585')) +
  scale_fill_manual(values = c(
    'Juvenile' = '#505050', 
    'Adult Female' = '#6B8E23', 
    'Adult Male' = '#C71585')) +
  theme_classic()
winter.precip.plot.all

# Plot average
aver.winter.precip.plot <- ggplot(aver.df.winter.precip, 
                                  aes(x = winter_precip, 
                                      y = estimate)) +
  geom_line(color = "#444444", size = 0.5) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), fill = "#999999", alpha = 0.3) +
  labs(x = 'Winter precipitation (standardized)',
       y = 'Estimated average survival probability\n(95% CI)',
       title = 'Average Effect of Winter Precipitation on Survival') +
  theme_classic()
aver.winter.precip.plot
