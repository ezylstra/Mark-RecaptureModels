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
dat <- read.csv('output/cleaned-capture-data-RMNP.csv')

# Delete X column created in the csv file and remove data for year 2012 because
# we still don't have the recapture data for this year. I asked Fred for it
dat1 <- dat %>% 
  select(-X) %>% 
  filter(year != 2012) 

# Remove - from UBI_band 
dat1$band <- gsub('-', '', dat1$band)

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
           sex, location) %>% 
  unite(ch, c('2003','2004','2005','2006','2007','2008','2009','2010','2011'), sep = '')%>% 
  as.data.frame() %>% 
  select(-band) 

# Change sex from character to factor
ch_adults$sex <- as.factor(as.character((ch_adults$sex)))
ch_adults$location <- as.factor(as.character((ch_adults$location)))

# ------------------ Capture histories with age at first capture ------------- #
# ------------------------ includes juveniles and adults --------------------- #

# Create capture history 
ch_age <- dat1 %>%
  group_by(band, year, sex, age) %>%  
  summarize(n.observation = length(year))%>%
  mutate(observed = 1) %>% 
  pivot_wider(names_from = year, 
              values_from = observed, 
              id_cols = c(band, sex, age), 
              values_fill = 0) %>% 
  relocate(band, '2003','2004','2005','2006','2007','2008','2009','2010','2011', 
           sex, age) %>%
  unite(ch, c('2003','2004','2005','2006','2007','2008','2009','2010', '2011'), 
        sep = '') %>% 
  mutate(age = if_else(age == 'AHY', 'adult', 'young')) %>% 
  as.data.frame() %>% 
  select(-band) 

# Change sex from character to factor
ch_age$sex <- as.factor(as.character((ch_age$sex)))
ch_age$age <- as.factor(as.character((ch_age$age)))


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

# Add effort as a covariate 

# Load data
effort <- read.csv('output/banding-effort-all-sites-RMNP.csv')
# Total banding days per year
# Should we consider trapping hours? 

# Prepare effort data to add to ddl
effort1 <- effort %>% 
  filter(!site %in% c('CLP', 'BGMD'), # Sites not included in capture data for analysis
         year != 2012) %>%  
  group_by(year) %>% 
  summarize(total_days = sum(total_banding_days)) %>% 
  rename(time = year,
         effort = total_days) %>% 
  mutate_if(is.character, as.numeric) %>% 
  as.data.frame

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
  
  cml <- create.model.list("CJS") # Creates a dataframe of all combinations of parameter specifications for each parameter 
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
# over time (β = -0.1). Males have a lower probability of survival (β = -0.33) 
# than females. The top model used for inference shows a time effect 
# on the probability of recapture.    

# Extract real estimates, separate row names into useful columns and do a little 
# clean up
reals <- best$results$real %>%
  rownames_to_column("rowname") %>%
  select(-c(fixed, note)) %>%
  separate_wider_delim(rowname,  
                       delim = " ",
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
  xlab("") +
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
  xlab("") +
  scale_y_continuous(limits = c(0,1), breaks = seq(0, 1, 0.2))
phi_fig


# 2) AGE

# We want to know if the adult survival probability for birds marked as young
# differs from the adult survival probability for birds marked as adults

########### Not sure about this code. I don't think I understand how to icorporate
# age in the models

# Process the encounter history data frame for Mark analysis
# I'm leaving sex out for now
age_process <- process.data(ch_age,
                            model = 'CJS',
                            begin.time = 2003,
                            groups = 'age',
                            initial.ages = c(1,0)) # 1 for adults and 0 for young

# Create design data frame for Mark model specification based in PIM (parameter 
# index matrix)
age_ddl <- make.design.data(age_process)
age_ddl


# Not sure if I need this step or not...... 
# I skipped it
# Add a young/adult age field to the design data for Phi named ya
# It uses right = FALSE so that the intervals are 0 (young) and 1 to 9 (adult).
age_ddl <- add.design.data(age_process,
                           age_ddl,
                           'Phi',
                           'age',
                           bins = c(0,1,9),
                           right = FALSE,
                           name = 'ya')

# Not sure if I need this step or not...... 
# I skipped it
# Create a dummy field called marked.as.adult which is 0 for the group
# marked as young and 1 for the group marked as adults
age_ddl$Phi$marked.as.adult <- 0
age_ddl$Phi$marked.as.adult[age_ddl$Phi$group == 'adult'] <- 1

age_ddl$Phi

# Add effort to ddl 
age_ddl$p <- merge_design.covariates(age_ddl$p, effort1)

# Run models using a function

age_models <- function()
{
  Phi.dot <- list(formula = ~1) 
  Phi.Time <- list(formula = ~Time)
  Phi.time <- list(formula = ~time) 
  Phi.age <- list(formula = ~age)
  Phi.agePlusTime <- list(formula = ~age + Time)
  Phi.ageandTime <- list(formula = ~age * Time)

  p.dot <- list(formula = ~1)
  p.time <- list(formula = ~time)
  p.age <- list(formula = ~age)
  p.effort <- list(formula = ~effort)
  p.timePlusage <- list(forula = ~time + age)
  p.timePluseffort <- list(forula = ~time + effort)
  
  cml <- create.model.list("CJS") 
  results <- mark.wrapper(cml, 
                          data = age_process,
                          ddl = age_ddl,
                          adjust = FALSE)
  return(results)
}

# Store the results in a marklist
age_results <- age_models()
age_results

# One models with lowest Delta AIC of 0. 
# Phi(~age + Time)p(~time)

# Using model 26
best_age <- age_results[[12]]

# Look at beta-hats
best_age$results$beta
best_age$results$real
