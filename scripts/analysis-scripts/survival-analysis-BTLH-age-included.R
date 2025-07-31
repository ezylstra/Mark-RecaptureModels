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
winter.mx <- read.csv('output/weather-data/covariates-output/winter-covar-mexico.csv')
summer.co <- read.csv('output/weather-data/covariates-output/summer-covar-colorado.csv')

# Prepare covariates for analysis 
winter <- winter.mx %>% 
  mutate(time = 2002:2011, .after = winter_period) %>%
  select(time, aver_cold_days, aver_precip) %>% 
  mutate(winter_aver_cold_days_z = z.stand(aver_cold_days),
         winter_precip_z = z.stand(aver_precip),
         winter_aver_cold_days = aver_cold_days,
         winter_aver_precip = aver_precip, .keep = 'unused')

summer <- summer.co %>% 
  select(year, aver_min_temp, aver_warm_days, aver_precip) %>% 
  rename(time = year) %>% 
  mutate(summer_min_temp_z = z.stand(aver_min_temp),
         summer_aver_warm_days_z = z.stand(aver_warm_days),
         summer_precip_z = z.stand(aver_precip),
         summer_aver_min_temp = aver_min_temp,
         summer_aver_warm_days = aver_warm_days,
         summer_aver_precip = aver_precip, .keep = 'unused')

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
  # Juveniles (0, both sexes combined). This will be the intercept when interpreting coefficients
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
  
  p.sexEffort <- list(formula = ~effort_hours_z)
  
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
# Phi(~ageclass)p(~effort_hours_z) 0.0

# Look at estimates and standard errors 
results.1 <- base.age.sex.results.1[[1]]
results.1$results$beta

# The probability of survival differs by age class were adults have a significantly
# higher probability of survival than juveniles. 
# The probability of recapture increases with banding effort.

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
  
  p.sexEffort <- list(formula = ~effort_hours_z)
  
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
# Phi(~sexadult)p(~effort_hours_z) 0.0
# Followed by far by 
# Phi(~ageclass)p(~effort_hours_z) 152.35

# Look at estimates and standard errors 
results.2 <- base.age.sex.results.2[[1]]
results.2$results$beta

# Juveniles (intercept) have a significantly lower probability (-) of survival 
# than both adult females and adult males. Among adults, females have a 
# significantly higher probability of survival than males.

# Remove mark files so they don't clog repo
invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))

# ----------------------------- Including covariates ------------------------- #

# -------------------- Adding wintering grounds covariates ------------------- #

# Is survival of juveniles and adults affected differently by the average number
# of cold days in Mexico?

# Create function
base.age.sex.models.3 <- function()
{
  Phi.sexAdult <- list(formula = ~sexadult)
  Phi.sexAdultCovar <- list(formula = ~sexadult + winter_aver_cold_days_z)
  
  p.sexEffort <- list(formula = ~effort_hours_z)
  
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
# Phi(~sexadult + winter_aver_cold_days_z)p(~effort_hours_z) 0.0
# Followed by far by 
# Phi(~sexadult)p(~sex + effort_hours_z) 45.29

# Look at estimates and standard errors 
results.3 <- base.age.sex.results.3[[2]]
results.3$results$beta

# The increase in the number of cold days decreases the probability of survival
# of all groups (-, significant)

# Look at real estimates
results.3$results$real

# Estimates for juveniles are around 0.2, for adult females are around 0.5
# and for adult males are around  0.3

# Remove mark files so they don't clog repo
invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Are juveniles more or less affected by the average number of cold days in Mexico 
# than adult females and adult males? 

# Create function
base.age.sex.models.4 <- function()
{
  Phi.sexAdult <- list(formula = ~sexadult)
  Phi.sexAdultPluCovar <- list(formula = ~sexadult + winter_aver_cold_days_z)
  Phi.sexAdultxCovar <- list(formula = ~sexadult * winter_aver_cold_days_z)
  
  p.sexEffort <- list(formula = ~effort_hours_z)
  
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
# Phi(~sexadult * winter_aver_cold_days_z)p(~effort_hours_z) 0.0
# Followed by
# Phi(~sexadult + winter_aver_cold_days_z)p(~effort_hours_z) 10.52

# Look at estimates and standard errors 
results.4 <- base.age.sex.results.4[[3]]
results.4$results$beta

# The increase in cold days is associated with decrease survival in juveniles 
# (-, barely not significant) 
# Adult females show a weaker response to the increase in cold days than juveniles,
# and seems that this increase has a positive effect on survival (+, not significant)
# Adult males show stronger negative response to the increase in the number of 
# cold days compared decreasing survival (-, significant)

# Remove mark files so they don't clog repo
invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Is survival of juveniles and adults affected differently by the average 
# precipitation in Mexico 

# Create function
base.age.sex.models.5 <- function()
{
  Phi.sexAdult <- list(formula = ~sexadult)
  Phi.sexAdultPluCovar <- list(formula = ~sexadult + winter_precip_z)
  Phi.sexAdultxCovar <- list(formula = ~sexadult * winter_precip_z)
  
  p.sexEffort <- list(formula = ~effort_hours_z)
  
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
# Phi(~sexadult + winter_precip_z)p(~effort_hours_z) 0.0
# Followed by
# Phi(~sexadult * winter_precip_z)p(~effort_hours_z) 3.44

# Look at estimates and standard errors 
results.5 <- base.age.sex.results.5[[2]]
results.5$results$beta

# The increase in precipitation is associated with decrease survival for all groups
# (-, significant)

# Remove mark files so they don't clog repo
invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))


# ---------------------- Adding  summer grounds covariates ------------------- #

# Is survival of juveniles and adults affected differently by the average number
# of warm days in summers in Colorado?

# Create function
base.age.sex.models.6 <- function()
{
  Phi.sexAdult <- list(formula = ~sexadult)
  Phi.sexAdultPluCovar <- list(formula = ~sexadult + summer_aver_warm_days_z)
  Phi.sexAdultxCovar <- list(formula = ~sexadult * summer_aver_warm_days_z)
  
  p.sexEffort <- list(formula = ~effort_hours_z)
  
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
# Phi(~sexadult + summer_aver_warm_days_z)p(~effort_hours_z) 0.0
# Closely followed by 
# Phi(~sexadult * summer_aver_warm_days_z)p(~effort_hours_z) 1.54

# Look at estimates and standard errors 
results.6 <- base.age.sex.results.6[[2]]
results.6$results$beta

# The increase in the number of warm days is associated with an increase in the 
# probability of survival for all groups (+, significant). 

# When exploring the estimates for the model with the interaction, none were
# statistically significant

# Remove mark files so they don't clog repo
invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Is survival of juveniles and adults affected differently by the min temp in 
# summers in Colorado?

# Create function
base.age.sex.models.7 <- function()
{
  Phi.sexAdult <- list(formula = ~sexadult)
  Phi.sexAdultPluCovar <- list(formula = ~sexadult + summer_min_temp_z)
  Phi.sexAdultxCovar <- list(formula = ~sexadult * summer_min_temp_z)
  
  p.sexEffort <- list(formula = ~effort_hours_z)
  
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
# Phi(~sexadult + summer_min_temp_z)p(~effort_hours_z) 0.0
# Followed by 
# Phi(~sexadult * summer_min_temp_z)p(~effort_hours_z) 2.49

# Look at estimates and standard errors 
results.7 <- base.age.sex.results.7[[2]]
results.7$results$beta

# The increase in summer min temp (warmer nights) is associated with a decrease 
# in the probability of survival for all groups (-, significant). 

# When exploring the estimates for the model with the interaction, none were
# statistically significant

# Remove mark files so they don't clog repo
invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Is survival of juveniles and adults affected differently by the aver precip in 
# summers in Colorado?

# Even thought including a resource availability covariate in the model for 
# covariate selection did not improved fit, I still wanted to include one of them 
# to test the hypothesis that more rain or higher NDVI have an effect on survival.

# I choose to use the covariate in the second best model:
# Model with lowest Delta AIC 
# Phi(~sex)p(~sex + effort_hours_z) 0.0
# Followed very closely by 
# Phi(~sex + aver_precip_z)p(~sex + effort_hours_z) 0.24
# So I'm testing if aver precip has an effect in the probability of survival

# Create function
base.age.sex.models.8 <- function()
{
  Phi.sexAdult <- list(formula = ~sexadult)
  Phi.sexAdultPluCovar <- list(formula = ~sexadult + summer_precip_z)
  Phi.sexAdultxCovar <- list(formula = ~sexadult * summer_precip_z)
  
  p.sexEffort <- list(formula = ~effort_hours_z)
  
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
# Phi(~sexadult * summer_precip_z)p(~effort_hours_z) 0.0
# Followed closely by 
# Phi(~sexadult + summer_precip_z)p(~effort_hours_z) 0.29

# Look at estimates and standard errors of best model
results.8 <- base.age.sex.results.8[[3]]
results.8$results$beta

# None of the interaction terms are statistically significant, although for 
# juveniles the CI barely includes zero.

# Look at estimates and standard errors of second best model
results.9 <- base.age.sex.results.8[[2]]
results.9$results$beta

# It seems like higher precipitation has a small positive effect on survival
# of all groups (+, barely not significant)

# Remove mark files so they don't clog repo
invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))

# I think aver precip has week support to be considered as a covariate to add 
# to the full model. I'm going to exclude it for now.


# ------------------------------- Run full model ----------------------------- #

# But first, check for correlation of cavariates 
covars <- left_join(winter, summer, by = 'time')

# Between winter and summer cold and warm days
cor.test(covars$winter_aver_cold_days_z, covars$summer_aver_warm_days_z)
# Low negative correlation not significant, r= -0.12, p = 0.73

# Create function
base.age.sex.full <- function() 
{
  Phi.full <- list(formula = ~sexadult * winter_aver_cold_days_z 
                                       + summer_aver_warm_days_z 
                                       + summer_min_temp_z
                                       + winter_precip_z)
  
  p.sexEffort <- list(formula = ~effort_hours_z)
  
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
# Juveniles (intercept) have lower probability of survival than adult males and 
# females (-, significant)
# Adult females have higher probability of survival than juveniles and males
# (+, significant)
# Adult males have higher probability of survival than juveniles but lower than 
# females (+, significant)

# The increase in warm days in the summer grounds increases the probability of
# survival for all groups (+, significant)
# The increase in min temperature (night temperature) in the summer grounds 
# decreases the probability of survival for all groups (-, significant)
# The increase of precipitation in the wintering grounds increases the probability
# of survival for all groups (+, significant)

# Interaction:
# The increase in cold days in the wintering grounds decreases survival of juveniles
# (-, significant)
# For adult females, this increase has a small effect and suggests that the probability
# of survival of females might not be affected by it in comparison to juveniles
# (+, not significant)
# The probability of survival of adult males decreases more than juveniles as the 
# number of cold days in the wintering grounds increases (-, significant)

# p:
# There is not significant evidence that the probability of recapture is different 
# between males and females
# More effort increases the probability of recapture (+, significant)

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
                                      color = group,
                                      shape = group)) +
  geom_line(aes(group = group), size = 0.3) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0, linewidth = 0.3) +
  scale_color_manual(values = c('Juvenile' = 'gray10', 
                                'Adult Female' = 'gray26', 
                                'Adult Male' = 'gray46')) +
  scale_shape_manual(values = c('Juvenile' = 15,
                                'Adult Female' = 17,  
                                'Adult Male' = 19)) + 
  
  scale_y_continuous(limits = c(0,1), 
                     breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(breaks = 2003:2011) +
  labs(x = 'Year',
       y = 'Estimated annual survival probability\n(95% CI)',
       shape = 'Group',
       color = 'Group') +
  theme_classic() +
  theme(legend.position = 'right',
        plot.title = element_text(hjust = 0.5))
Phi.plot

# Save plot
ggsave(path = 'output/plots/New Plots Survival/',
       filename = 'survival.png',
       plot = Phi.plot,
       device = 'png',
       dpi = 300)

# 2) Recapture probability of females and males over time

# Prepare labels for plotting
real.p.ests <- real.p.ests %>%
  mutate(group = ifelse(sexMF == 'F', 'Female', 'Male'))

# Plot recapture probability
p.plot <- ggplot(real.p.ests, aes(x = as.numeric(year), 
                                  y = estimate,
                                  color = group,
                                  shape = group)) +
  geom_line(aes(group = group), size = 0.3) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0, linewidth = 0.3) +
  scale_color_manual(values = c('Female' = 'gray26', 
                                'Male' = 'gray46')) +
  scale_shape_manual(values = c('Female' = 17,  
                                'Male' = 19)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(breaks = 2003:2012) +
  labs(y = 'Estimated recapture probability\n(95% CI)',
       x = 'Year') +
  theme_classic() +
  theme(legend.title = element_blank())
p.plot

# Save plot
ggsave(path = 'output/plots/New Plots Survival/',
       filename = 'recapture.png',
       plot = p.plot,
       device = 'png',
       dpi = 300)

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
winter.days.range <- build.range(covars, 'winter_aver_cold_days_z')
summer.days.range <- build.range(covars, 'summer_aver_warm_days_z')
summer.temp.range <- build.range(covars, 'summer_min_temp_z')
winter.precip.range <- build.range(covars, 'winter_precip_z')

# Create beta vectors from phi.betas
betas <- phi.betas$estimate

# Assign names to the beta vectors using the 'term' column in phi.betas
# This ensures that the coefficients match the correct columns in the 
# prediction data frames during matrix multiplication. 
names(betas) <- phi.betas$term

# To calculate confidence intervals for predictions by hand, we'll need the 
# variance-covariate matrix. Extract the values for Phi [1:9]
var.covar.matrix <- results.10$results$beta.vcv[1:9, 1:9]


# -------------------------------- Create plots ------------------------------ #

# 3) Effect of winter average cold days on probability of survival on each group 
# (juveniles, adult females and adult males)

# Build prediction data frame
pred.df.winter.days <- data.frame(
  Group = rep(c('Juvenile', 'Adult Female', 'Adult Male'), 
              each = length(winter.days.range)),
  winter_aver_cold_days_z = rep(winter.days.range, times = 3)) # 3 for each group

# Add other covariates to predict data frame, holding them constant at 0 
# (standardized mean) 
pred.df.winter.days$Intercept <- 1 
# Dummy variables for adult female and adult male
pred.df.winter.days$sexadult1 <- ifelse(pred.df.winter.days$Group == 'Adult Female', 1, 0)
pred.df.winter.days$sexadult2 <- ifelse(pred.df.winter.days$Group == 'Adult Male', 1, 0)
pred.df.winter.days$summer_aver_warm_days_z <- 0
pred.df.winter.days$summer_min_temp_z <- 0
pred.df.winter.days$winter_precip_z <- 0 

# Include interaction terms. Let the effect of temperature depend on sex
# Use `` (`sexadult2:winter_min_temp`) so the code works!
pred.df.winter.days$`sexadult1:winter_aver_cold_days_z` <- 
  pred.df.winter.days$sexadult1 * pred.df.winter.days$winter_aver_cold_days_z
pred.df.winter.days$`sexadult2:winter_aver_cold_days_z` <- 
  pred.df.winter.days$sexadult2 * pred.df.winter.days$winter_aver_cold_days_z 

# Calculate estimates of survival on the logit scale
# Selecting columns by names ensures that the order of variables in the 
# prediction data frame matches the order of the beta coefficients
estimate.winter.days.logit <- as.matrix(pred.df.winter.days[, names(betas)]) %*% 
  as.matrix(betas)

# Create design matrix
X.winter.days <- as.matrix(pred.df.winter.days[, names(betas)])

# Calculate bounds of confidence intervals on the logit scale
# A little matrix math, using the part of the var-covar matrix that applies to 
# survival parameters and not recapture parameters
std.errors.winter.days <- sqrt(diag(X.winter.days %*%
                                      var.covar.matrix %*%
                                      t(X.winter.days)))
lcl.winter.days.logit <- estimate.winter.days.logit - 1.96 * std.errors.winter.days
ucl.winter.days.logit <- estimate.winter.days.logit + 1.96 * std.errors.winter.days

# Convert to probability scale
pred.df.winter.days$estimate <- plogis(estimate.winter.days.logit) 
pred.df.winter.days$lcl <- plogis(lcl.winter.days.logit)
pred.df.winter.days$ucl <- plogis(ucl.winter.days.logit)

# Back transform covariate to original scale
winter.days.mean <- mean(covars$winter_aver_cold_days)
winter.days.sd <- sd(covars$winter_aver_cold_days)
pred.df.winter.days$winter_aver_cold_days_c <- 
  pred.df.winter.days$winter_aver_cold_days_z * winter.days.sd + winter.days.mean  

# Plot
winter.days.plot <- ggplot(pred.df.winter.days, aes(x = winter_aver_cold_days_c, 
                                                    y = estimate, 
                                                    color = Group,
                                                    fill = Group,
                                                    linetype = Group)) +
  geom_line(size = 0.3) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.1, color = NA) +
  labs(x = 'Average number of cold days (≤ 10 °C) in wintering grounds',
       y = 'Estimated survival probability\n(95% CI)',
       color = 'Group',
       fill = 'Group',
       linetype = 'Group') +
  scale_x_continuous(labels = scales::number_format(accuracy = 1)) +
  scale_color_manual(values = c('Juvenile' = 'gray10', 
                                'Adult Female' = 'gray26', 
                                'Adult Male' = 'gray46')) +
  scale_fill_manual(values = c('Juvenile' = 'gray10', 
                               'Adult Female' = 'gray26', 
                               'Adult Male' = 'gray46')) +
  scale_linetype_manual(values = c('Juvenile' = 'solid',
                                   'Adult Female' = 'longdash',
                                   'Adult Male' = 'dotted')) +
  theme_classic()
winter.days.plot

# Save plot
ggsave(path = 'output/plots/New Plots Survival/',
       filename = 'winter cold days effect.png',
       plot = winter.days.plot,
       device = 'png',
       dpi = 300)

# 4) Effect of summer min temp on probability of survival of all groups

# Build a prediction data frame
pred.df.summer.temp <- data.frame(
  Group = rep(c('Juvenile', 'Adult Female', 'Adult Male'), 
              each = length(summer.temp.range)),
  summer_min_temp_z = rep(summer.temp.range, times = 3)) 

# Add other covariates to predict data frame, holding them constant at 0 
# (standardized mean) 
pred.df.summer.temp$Intercept <- 1 
pred.df.summer.temp$sexadult1 <- ifelse(pred.df.summer.temp$Group == 'Adult Female', 1, 0)
pred.df.summer.temp$sexadult2 <- ifelse(pred.df.summer.temp$Group == 'Adult Male', 1, 0)
pred.df.summer.temp$winter_aver_cold_days_z <- 0
pred.df.summer.temp$summer_aver_warm_days_z <- 0
pred.df.summer.temp$winter_precip_z <- 0 
pred.df.summer.temp$`sexadult1:winter_aver_cold_days_z` <- 0 # Use `` so the code works!
pred.df.summer.temp$`sexadult2:winter_aver_cold_days_z` <- 0

# Calculate estimates of survival on the logit scale
# Selecting columns by names ensures that the order of variables in the 
# prediction data frame matches the order of the beta coefficients
estimate.summer.temp.logit <- as.matrix(pred.df.summer.temp[, names(betas)]) %*% 
  as.matrix(betas)

# Create design matrix
X.summer.temp <- as.matrix(pred.df.summer.temp[, names(betas)])

# Calculate bounds of confidence intervals on the logit scale
std.errors.summer.temp <- sqrt(diag(X.summer.temp %*%
                                      var.covar.matrix %*%
                                      t(X.summer.temp)))
lcl.summer.temp.logit <- estimate.summer.temp.logit - 1.96 * std.errors.summer.temp
ucl.summer.temp.logit <- estimate.summer.temp.logit + 1.96 * std.errors.summer.temp

# Convert to probability scale
pred.df.summer.temp$estimate <- plogis(estimate.summer.temp.logit) 
pred.df.summer.temp$lcl <- plogis(lcl.summer.temp.logit)
pred.df.summer.temp$ucl <- plogis(ucl.summer.temp.logit)

# Back transform covariate to original scale
summer.temp.mean <- mean(covars$summer_aver_min_temp)
summer.temp.sd <- sd(covars$summer_aver_min_temp)
pred.df.summer.temp$summer_min_temp_c <- 
  pred.df.summer.temp$summer_min_temp_z * summer.temp.sd + summer.temp.mean  

# Plot
summer.temp.plot <- ggplot(pred.df.summer.temp, aes(x = summer_min_temp_c, 
                                                    y = estimate, 
                                                    color = Group,
                                                    fill = Group,
                                                    linetype = Group)) +
  geom_line(size = 0.3) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.1, color = NA) +
  labs(x = 'Average summer minimum temperature (°C)',
       y = 'Estimated survival probability\n(95% CI)',
       color = 'Group',
       fill = 'Group',
       linetype = 'Group') +
  scale_color_manual(values = c('Juvenile' = 'gray10', 
                                'Adult Female' = 'gray26', 
                                'Adult Male' = 'gray46')) +
  scale_fill_manual(values = c('Juvenile' = 'gray10', 
                               'Adult Female' = 'gray26', 
                               'Adult Male' = 'gray46')) +
  scale_linetype_manual(values = c('Juvenile' = 'solid',
                                   'Adult Female' = 'longdash',
                                   'Adult Male' = 'dotted')) +
  scale_x_continuous(breaks = seq(-12, -4, by = 2)) +
  theme_classic()
summer.temp.plot

# Save plot
ggsave(path = 'output/plots/New Plots Survival/', 
       filename = 'summer min temp effect.png',
       plot = summer.temp.plot,
       device = 'png',
       dpi = 300)

# 5) Effect of of the average number of warm days in summer on survival of all 
# groups

# Build prediction data frame
pred.df.summer.days <- data.frame(
  Group = rep(c('Juvenile', 'Adult Female', 'Adult Male'), 
              each = length(summer.days.range)),
  summer_aver_warm_days_z = rep(summer.days.range, times = 3)) # 3 for each group

# Add other covariates to predict data frame, holding them constant at 0 
# (standardized mean) 
pred.df.summer.days$Intercept <- 1 
pred.df.summer.days$sexadult1 <- ifelse(pred.df.summer.days$Group == 'Adult Female', 1, 0)
pred.df.summer.days$sexadult2 <- ifelse(pred.df.summer.days$Group == 'Adult Male', 1, 0)
pred.df.summer.days$winter_aver_cold_days_z <- 0
pred.df.summer.days$summer_min_temp_z <- 0
pred.df.summer.days$winter_precip_z <- 0 
pred.df.summer.days$`sexadult1:winter_aver_cold_days_z` <- 0 
pred.df.summer.days$`sexadult2:winter_aver_cold_days_z` <- 0

# Calculate estimates of survival on the logit scale
# Selecting columns by names ensures that the order of variables in the 
# prediction data frame matches the order of the beta coefficients
estimate.summer.days.logit <- as.matrix(pred.df.summer.days[, names(betas)]) %*% 
  as.matrix(betas)

# Create design matrix
X.summer.days <- as.matrix(pred.df.summer.days[, names(betas)])

# Calculate bounds of confidence intervals on the logit scale
# A little matrix math, using the part of the var-covar matrix that applies to 
# survival parameters and not recapture parameters
std.errors.summer.days <- sqrt(diag(X.summer.days %*%
                                      var.covar.matrix %*%
                                      t(X.summer.days)))
lcl.summer.days.logit <- estimate.summer.days.logit - 1.96 * std.errors.summer.days
ucl.summer.days.logit <- estimate.summer.days.logit + 1.96 * std.errors.summer.days

# Convert to probability scale
pred.df.summer.days$estimate <- plogis(estimate.summer.days.logit) 
pred.df.summer.days$lcl <- plogis(lcl.summer.days.logit)
pred.df.summer.days$ucl <- plogis(ucl.summer.days.logit)

# Back transform covariate to original scale
summer.days.mean <- mean(covars$summer_aver_warm_days)
summer.days.sd <- sd(covars$summer_aver_warm_days)
pred.df.summer.days$summer_aver_warm_days_c <- 
  pred.df.summer.days$summer_aver_warm_days_z * summer.days.sd + summer.days.mean  

# Plot
summer.days.plot <- ggplot(pred.df.summer.days, aes(x = summer_aver_warm_days_c, 
                                                    y = estimate, 
                                                    color = Group,
                                                    fill = Group,
                                                    linetype = Group)) +
  geom_line(size = 0.3) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.1, color = NA) +
  labs(x = 'Average number of warm days (≥ 20 °C) in the summer grounds ',
       y = 'Estimated survival probability\n(95% CI)',
       color = 'Group',
       fill = 'Group',
       linetype = 'Group') +
  scale_x_continuous(labels = scales::number_format(accuracy = 1)) +
  scale_color_manual(values = c('Juvenile' = 'gray10', 
                                'Adult Female' = 'gray26', 
                                'Adult Male' = 'gray46')) +
  scale_fill_manual(values = c('Juvenile' = 'gray10', 
                               'Adult Female' = 'gray26', 
                               'Adult Male' = 'gray46')) +
  scale_linetype_manual(values = c('Juvenile' = 'solid',
                                   'Adult Female' = 'longdash',
                                   'Adult Male' = 'dotted')) +
  theme_classic()
summer.days.plot

# Save plot
ggsave(path = 'output/plots/New Plots Survival/',
       filename = 'summer warm days effect.png',
       plot = summer.days.plot,
       device = 'png',
       dpi = 300)

# 6) Effect of winter precip on probability of survival of all groups

# Build a prediction data frame
pred.df.winter.precip <- data.frame(
  Group = rep(c('Juvenile', 'Adult Female', 'Adult Male'), 
              each = length(winter.precip.range)),
  winter_precip_z = rep(winter.precip.range, times = 3))

# Add other covariates to predict data frame, holding them constant at 0 
# (standardized mean) 
pred.df.winter.precip$Intercept <- 1
pred.df.winter.precip$sexadult1 <- ifelse(pred.df.winter.precip$Group == 'Adult Female', 1, 0)
pred.df.winter.precip$sexadult2 <- ifelse(pred.df.winter.precip$Group == 'Adult Male', 1, 0)
pred.df.winter.precip$summer_aver_warm_days_z <- 0
pred.df.winter.precip$winter_aver_cold_days_z <- 0
pred.df.winter.precip$summer_min_temp_z <- 0
pred.df.winter.precip$`sexadult1:winter_aver_cold_days_z` <- 0 
pred.df.winter.precip$`sexadult2:winter_aver_cold_days_z` <- 0

# Calculate estimates of survival on the logit scale
estimate.winter.precip.logit <- as.matrix(pred.df.winter.precip[, names(betas)]) %*% 
  as.matrix(betas)

# Create design matrix
X.winter.precip <- as.matrix(pred.df.winter.precip[, names(betas)])

# Calculate bounds of confidence intervals on the logit scale
std.errors.winter.precip <- sqrt(diag(X.winter.precip %*%
                                        var.covar.matrix %*%
                                        t(X.winter.precip)))
lcl.winter.precip.logit <- estimate.winter.precip.logit - 1.96 * std.errors.winter.precip
ucl.winter.precip.logit <- estimate.winter.precip.logit + 1.96 * std.errors.winter.precip

# Convert to probability scale
pred.df.winter.precip$estimate <- plogis(estimate.winter.precip.logit) 
pred.df.winter.precip$lcl <- plogis(lcl.winter.precip.logit)
pred.df.winter.precip$ucl <- plogis(ucl.winter.precip.logit)

# Back transform covariate to original scale
winter.precip.mean <- mean(covars$winter_aver_precip)
winter.precip.sd <- sd(covars$winter_aver_precip)
pred.df.winter.precip$winter_aver_precip_c <- 
  pred.df.winter.precip$winter_precip_z * winter.precip.sd + winter.precip.mean  

# Plot
winter.precip.plot <- ggplot(pred.df.winter.precip, aes(x = winter_aver_precip_c,
                                                        y = estimate, 
                                                        color = Group,
                                                        fill = Group,
                                                        linetype = Group)) +
  geom_line(size = 0.3) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.1, color = NA) +
  labs(x = 'Winter precipitation (mm)',
       y = 'Estimated survival probability\n(95% CI)',
       color = 'Group',
       fill = 'Group',
       linetype = 'Group') +
  scale_color_manual(values = c('Juvenile' = 'gray10', 
                                'Adult Female' = 'gray26', 
                                'Adult Male' = 'gray46')) +
  scale_fill_manual(values = c('Juvenile' = 'gray10', 
                               'Adult Female' = 'gray26', 
                               'Adult Male' = 'gray46')) +
  scale_linetype_manual(values = c('Juvenile' = 'solid',
                                   'Adult Female' = 'longdash',
                                   'Adult Male' = 'dotted')) +
  theme_classic()
winter.precip.plot

# Save plot
ggsave(path = 'output/plots/New Plots Survival/',
       filename = 'winter precip effect.png',
       plot = winter.precip.plot,
       device = 'png',
       dpi = 300)
