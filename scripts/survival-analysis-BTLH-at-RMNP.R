# Survival analysis for Broad-tailed Hummingbird at Rocky Mountain National Park
# Gaby Samaniego
# gabysamaniego@arizona.edu
# 2024-06-12

# Load packages
library(tidyverse)
library(RMark)

# Clear environment
rm(list = ls()) 

# Load data
dat <- read.csv('output/cleaned-capture-data-RMNP-for-CJS.csv')

# Sort data
dat1 <- dat %>% 
  arrange(band, year)

# -------------------- CREATE CAPTURE HISTORIES FOR BTLH --------------------- # 

# -------------- Capture histories without age at first capture -------------- #
# --------------------- includes just adult individuals ---------------------- #

# Create capture history 
ch_adults <- dat1 %>% 
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
ch_adults$sex <- factor(ch_adults$sex, levels = c('F', 'M'))
ch_adults$location <- factor(ch_adults$location, levels = c('east', 'west'))

# Checks
head(ch_adults)
str(ch_adults)

# ------------------ Capture histories with age at first capture ------------- #
# ------------------------ includes juveniles and adults --------------------- #

# Create capture histories
# (Not including location now -- can easily add in later)
ch_age <- dat1 %>%
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
ch_age$sex <- factor(ch_age$sex, levels = c('F', 'M'))
ch_age$age <- factor(ch_age$age, levels = c('young', 'adult'))

# Checks
head(ch_age)
str(ch_age)

# -------------------- Prepare effort to add it as a covariate --------------- #

# Load data
effort <- read.csv('output/banding-effort-all-sites-RMNP.csv')
# Total banding days per year

# Prepare effort data to add to ddl
effort1 <- effort %>% 
  filter(!site %in% c('CLP', 'BGMD')) %>%  # Sites not included in capture data for analysis
  group_by(year) %>% 
  summarize(total_days = sum(total_banding_days)) %>% 
  rename(time = year,
         effort = total_days) %>% 
  mutate_if(is.character, as.numeric) %>% 
  as.data.frame

# It's usually a good idea to standardize covariates (to avoid estimation problems
# and to help with interpretation). If effort is standardized to a mean of 0 and
# SD = 1, then the intercept will represent recapture probability at the mean
# effort level and the coefficient represents the expected change in recapture 
# probability for a 1-SD increase in effort.
effort1 <- effort1 %>%
  mutate(effort.z = (effort - mean(effort)) / sd(effort)) %>% 
  select(-effort) %>% 
  rename(effort = effort.z)
effort1

# ------------------------------ RUN CJS ANALYSIS ---------------------------- #

# 1) ADULTS

# Process the encounter history data frame for Mark analysis
adults_process <- process.data(ch_adults,
                               model = 'CJS',
                               begin.time = 2003,
                               groups = c('sex','location')) 

# Create design data frame for Mark model specification based in PIM (parameter 
# index matrix)
adults_ddl <- make.design.data(adults_process)

# Add effort to ddl 
adults_ddl$p <- merge_design.covariates(adults_ddl$p, effort1)

# Run models using a function

# The function defines and runs a set of models and returns a marklist with the 
# results and a model.table. It uses the processed and designed data created
# previously 

adults_models <- function()
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
  
  cml <- create.model.list('CJS') # Creates a dataframe of all combinations of parameter specifications for each parameter 
  results <- mark.wrapper(cml, # Constructs and runs a set of MARK models from a dataframe (cml)
                          data = adults_process,
                          ddl = adults_ddl,
                          adjust = FALSE) # Accepts the parameter counts from MARK
  return(results)
}

# Store the results in a marklist
adults_results <- adults_models()
adults_results

# Two models with lowest Delta AIC of 0. 
# Phi(~sex * Time)p(~time)
# Phi(~sex * Time)p(~time + effort) 

# Using model 26
best <- adults_results[[26]]

# Look at beta-hats
best$results$beta
best$results$real

# Adult male broad-tailed hummingbirds' survival probability in RMNP has decreased 
# over time (β = -0.05). Males have a lower probability of survival (β = -0.48) 
# than females. The top model used for inference shows a time effect 
# on the probability of recapture.    

# Extract real estimates, separate row names into useful columns and do a little 
# clean up
reals <- best$results$real %>%
  rownames_to_column('rowname') %>%
  select(-c(fixed, note)) %>%
  separate_wider_delim(rowname,  
                       delim = ' ',
                       names = c('parameter', 'sex', NA, NA, 'year')) %>%
  mutate(sex = str_sub(sex, 2, 2), 
         year = as.numeric(str_sub(year, 2, 5))) %>%
  data.frame()

# Visualize results

# Plot estimates of recapture probability
p_reals <- reals %>%
  filter(parameter == 'p')

p_fig <- ggplot(p_reals, aes(x = year, y = estimate)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0) +
  theme_classic() +
  ylab(expression(atop('Estimated recapture probability', paste('(95% CI)')))) + 
  xlab('') +
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, 0.2))
p_fig

# Plot estimates of survival probabilities 
phi_reals <- reals %>%
  filter(parameter == 'Phi')

phi_fig <- ggplot(phi_reals, aes(x = year, y = estimate)) +
  geom_point(size = 1.5, aes(color = sex)) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0) +
  scale_color_manual(values = c('#8E3E6E', '#4F7942')) +
  scale_fill_manual (values = c('#8E3E6E', '#4F7942')) +
  theme_classic() +
  ylab(expression(atop('Estimated survival probability', paste('(95% CI)')))) + 
  xlab('') +
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, 0.2))
phi_fig

#Get rid of the mark files so they don't clog repo
rm(adults_results)
cleanup(ask = F)

# 2) AGE

# Process the encounter history data frame for Mark analysis
age_process <- process.data(data = ch_age,
                            model = 'CJS',
                            begin.time = 2003,
                            groups = c('sex', 'age'),
                            age.var = 2,  # Provides the index for age variable in groups vector 
                            initial.ages = c(0, 1)) # Initial ages for each level of age variable

# Create design matrix
age_ddl <- make.design.data(age_process)

# This is the key step: creating age classes for survival: juvenile = age 0, 
#                                                          adult = 1+
age_ddl <- add.design.data(data = age_process,
                           ddl = age_ddl,
                           parameter = 'Phi',
                           type = 'age',
                           bins = c(0,1,9), 
                           right = FALSE, # Determines how bins are truncated, If TRUE, bin intervals are closed on the right
                                          # so the intervals are 0 young, 1 to 9 adult
                           name = 'ageclass') 
                           
# At this point, not planning on including age or age classes in recapture model, 
# since all individuals are at least 1 year old (adults) when recaptured.

# Add effort to ddl 
age_ddl$p <- merge_design.covariates(age_ddl$p, effort1)

# Create a couple other variables to help with model construction
# Why these groups? 

# Creating 3 groups: Juveniles (0; both sexes combined) 
#                    AdultF (1) 
#                    AdultM (2)
age_ddl$Phi$sexadult <- ifelse(age_ddl$Phi$ageclass == '[0,1)', 0,
                               ifelse(age_ddl$Phi$sex == 'F', 1, 2))

# Change new variable to a class factor
age_ddl$Phi$sexadult <- factor(age_ddl$Phi$sexadult)

# Indicator for adults: Juvenile (0)
#                       Adult (1)
age_ddl$Phi$adult <- ifelse(age_ddl$Phi$ageclass == '[0,1)', 0, 1)

#Why this one was not a factor? Should it be a factor?
# Change new variable to a class factor
age_ddl$Phi$adult <- factor(age_ddl$Phi$adult)

# Inspect the newly created RMark objects
str(age_ddl)

# Phi
head(age_ddl$Phi, 10)
tail(age_ddl$Phi, 10)
summary(age_ddl$Phi)

# p
head(age_ddl$p, 10)
summary(age_ddl$p)

# ------------------------- Run models --------------------------------------- #  

# Run models exploring effects of sex and ageclass on survival (no time/Trends 
# yet, except in recapture probability model)

age_sex_models <- function()
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
                          data = age_process,
                          ddl = age_ddl,
                          output = FALSE,
                          adjust = FALSE)
  return(results)
}

# Store the results in a marklist
age_sex_results <- age_sex_models()
age_sex_results

# Pick index of model you'd like to look at more closely
model_ind <- 18 

# Look at estimates from one of the models and interpret coefficients
age_sex_results[[model_ind]]$results$beta

# Using model 18 
# Phi(~ageclass + sex)p(~time + sex)

# The probability of survival is different by age class and sex. Adults have a 
# higher probability of survival than juveniles and males have a lower probability
# of survival than females. Males have a lower probability of recapture than 
# females and it is different by year 

# Look at real estimates
age_sex_results[[model_ind]]$results$real

# ---------------------------------------------------------------------------#
# Code below organizes the real estimates into a new dataframe

# Extract real estimates and add columns to identify grouping variables
real_ests <- age_sex_results[[model_ind]]$results$real %>%
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

real_ests_Phi <- real_ests %>%
  filter(param == 'Phi')

real_ests_p <- real_ests %>%
  filter(param == 'p') %>%
  select(-age)

# Create dataframe to organize the real survival estimates
Phi_ests <- data.frame(Time = rep(min(age_ddl$Phi$Time):max(age_ddl$Phi$Time), 4),
                       ageclass = rep(c(0, 0, 1, 1), each = 8),
                       sex = rep(c(0, 1, 0, 1), each = 8)) %>%
  mutate(year = Time + 2003,
         age = ifelse(ageclass == 0, 'J', 'A'),
         sexMF = ifelse(sex == 0, 'F', 'M'))
Phi_ests <- Phi_ests %>%
  left_join(real_ests_Phi, by = c('year', 'age', 'sexMF')) %>%
  filter(!is.na(estimate)) %>%
  group_by(age, sexMF) %>%
  mutate(nyears = length(year)) %>%
  ungroup() %>%
  data.frame() %>%
  mutate(year = ifelse(nyears == 1, 'All years', as.character(year))) %>%
  select(-c(Time, ageclass, sex, nyears)) %>%
  relocate(param)

# Create a dataframe to organize the real recapture probability estimates
p_ests <- data.frame(Time = rep(min(age_ddl$Phi$Time):max(age_ddl$Phi$Time), 2),
                     sex = rep(c(0, 1), each = 8)) %>%
  mutate(year = Time + 2003,
         sexMF = ifelse(sex == 0, 'F', 'M'))
p_ests <- p_ests %>%
  left_join(real_ests_p, by = c('year', 'sexMF')) %>%
  filter(!is.na(estimate)) %>%
  group_by(sexMF) %>%
  mutate(nyears = length(year)) %>%
  ungroup() %>%
  data.frame() %>%
  mutate(year = ifelse(nyears == 1, 'All years', as.character(year))) %>%
  select(-c(Time, sex, nyears)) %>%
  relocate(param)

Phi_ests
p_ests
# If just one sex is listed, then that means that males and females were
# assumed to have the same survival (or recapture) probability

# ---------------------------------------------------------------------------#

#Get rid of the mark files so they don't clog repo
rm(age_sex_results)
cleanup(ask = F)
