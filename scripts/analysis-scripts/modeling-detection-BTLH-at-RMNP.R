# Modeling detection probability using a 'step-down' approach 
# with a fixed survival model
# Following Lebreton et al. (1992)

# Gaby Samaniego
# gabysamaniego@arizona.edu
# 2025-07-24

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
         month != 9) %>% 
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

# Edit effort data and standardize it
effort.z <- effort.raw %>% 
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

# ----------------------------------- RUN MODELS ----------------------------- #

# Model detection probability 

# Including time in function

# Create function 
ahy.p.1 <- function()
{
  Phi.sexTime <- list(formula = ~sex + time) 
  
  p.dot <- list(formula = ~ 1)
  p.sex <- list(formula = ~sex)
  p.time <- list(formula = ~ time)
  p.effort <- list(formula = ~effort_hours_z)
  p.sexEffortadd <- list(formula = ~sex + effort_hours_z)
  p.sexTime <- list(formula = ~ sex + time)
  p.timeEffort <- list(formula = ~time + effort_hours_z)
 
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
ahy.p.1.results <- ahy.p.1()
ahy.p.1.results

# Model with lowest Delta AIC
# Phi(~sex + time)p(~time) 0.0
# and
# Phi(~sex + time)p(~time + effort_hours_z) 0.0

# Look at estimates and standard errors of best model
results.1 <- ahy.p.1.results[[6]]
results.1$results$beta

# Look at estimates and standard errors of best model
results.2 <- ahy.p.1.results[[7]]
results.2$results$beta

# Including both time and effort causes estimation issues (SE = 0)

# I decided to exclude time from the next function as it is a covariate that does
# not explains much. It just tells us that detection probability differs by year.

# Remove mark files so they don't clog repo
invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# Excluding time in function

# Create function 
ahy.p.2 <- function()
{
  Phi.sexTime <- list(formula = ~sex + time) 
  
  p.dot <- list(formula = ~ 1)
  p.sex <- list(formula = ~sex)
  p.effort <- list(formula = ~effort_hours_z)
  p.sexEffortadd <- list(formula = ~sex + effort_hours_z)
  
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
ahy.p.2.results <- ahy.p.2()
ahy.p.2.results

# Model with lowest Delta AIC
# Phi(~sex + time)p(~effort_hours_z) 0.0
# Followed by 
# Phi(~sex + time)p(~1) 1.3
# Phi(~sex + time)p(~sex + effort_hours_z) 2.0

# Look at estimates and standard errors of best model
results.3 <- ahy.p.2.results[[2]]
results.3$results$beta

# The probability of detection increases with effort (+, barely not significant)

# Look at estimates and standard errors of third best model
results.4 <- ahy.p.1.results[[4]]
results.4$results$beta

# The probability of detection doesn't differ by sex (b = 0.02, not significant) 

# Including sex doesn't improve model fit.

# Remove mark files so they don't clog repo
invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))

