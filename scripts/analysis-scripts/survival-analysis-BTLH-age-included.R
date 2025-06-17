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

#### Later add environmental covariates here....

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

# Juveniles (intercept) have a significantly lower probability of survival than both 
# adult females and adult males. Among adults, females have a significantly higher 
# probability of survival than males.

# The probability of recapture increases with banding effort and differs by sex were
# males have a lower probability of being recaptured than females.

# Remove mark files so they don't clog repo
invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))

# ----------------------------- Including covariates ------------------------- #

# Now, what weather or resource availability covariates influence juvenile/adult 
# survival? 

# My thoughts:

# In the summer grounds when preparing to migrate, availability of resources can 
# be critical for a successful migration. Also maybe hot weather in the summer 
# grounds can affect juveniles (and adults) negatively. The variable of frost days, 
# I think affects more to adults in the summer grounds as juveniles are born later 
# in the season when the days are not that cold as in early May and June. 

# For those juveniles who survived migration and arrived to the wintering grounds, 
# the availability of resources and temperature can be important predictor of survival. 

# But first I need to select these covariates. I'll use Phi(~sexadult + covar) and
# p(sex + effort) to identify which weather and resource covariates explain survival.
# The same way I did wit the adult only data.




