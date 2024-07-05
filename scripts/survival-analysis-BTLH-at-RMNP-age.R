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

# Load effort data
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
# Usually a good idea to standardize covariates (to avoid estimation problems
# and to help with interpretation). If effort is standardized to a mean of 0 and
# SD = 1, then the intercept will represent recapture probability at the mean
# effort level and the coefficient represents the expected change in recapture 
# probability for a 1-SD increase in effort.
effort1 <- effort1 %>%
  mutate(effort.z = (effort - mean(effort)) / sd(effort))
effort1

# ------------------ Capture histories with age at first capture ------------- #
# ------------------------ includes juveniles and adults --------------------- #

# Create capture histories
# (Not including location now -- can easily add in later)
ch_age <- dat1 %>%
  group_by(band, year, sex, age) %>%  
  summarize(n.observation = length(year), .groups = "keep") %>%
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
  as.data.frame()

# Make several variables factors (specifying levels for clarity)
ch_age$sex <- factor(ch_age$sex, levels = c("F", "M"))
ch_age$age <- factor(ch_age$age, levels = c("young", "adult"))
# Checks
head(ch_age)
str(ch_age)

# -------- Process data for CJS model and create design matrix --------------- #
age_process <- process.data(data = ch_age,
                            model = "CJS",
                            begin.time = 2003,
                            groups = c("sex", "age"),
                            age.var = 2,  # Provides the index for age variable in groups vector 
                            initial.ages = c(0, 1)) # Initial ages for each level of age variable
age_ddl <- make.design.data(age_process)

# This is the key step: creating age classes for survival: juvenile = age 0, adult = 1+
age_ddl <- add.design.data(data = age_process, 
                           ddl = age_ddl,
                           parameter = "Phi",
                           type = "age",
                           bins = c(0, 1, 9), 
                           name = "ageclass", 
                           right = FALSE) # Right truncation on bins
# At this point, not planning on including age or age classes in recapture model, 
# since all individuals are at least 1 year old (adults) when recaptured.

# Add effort to ddl 
age_ddl$p <- merge_design.covariates(age_ddl$p, effort1)

# Create a couple other variables to help with model construction
  # Creating 3 groups: Juveniles (0; both sexes combined); AdultF (1); AdultM (2)
  age_ddl$Phi$sexadult <- ifelse(age_ddl$Phi$ageclass == "[0,1)", 0,
                                 ifelse(age_ddl$Phi$sex == "F", 1, 2))
  age_ddl$Phi$sexadult <- factor(age_ddl$Phi$sexadult)
  # Indicator for adults
  age_ddl$Phi$adult <- ifelse(age_ddl$Phi$ageclass == "[0,1)", 0, 1)

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
  
  cml <- create.model.list("CJS") 
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

# Look at real estiamtes
age_sex_results[[model_ind]]$results$real

  # ---------------------------------------------------------------------------#
  # Code below organizes the real estimates into a new dataframe
      
      # Extract real estimates and add columns to identify grouping variables
      real_ests <- age_sex_results[[model_ind]]$results$real %>%
        rownames_to_column(var = "group") %>%
        mutate(param = ifelse(str_sub(group, 1, 3) == "Phi", "Phi", "p"),
               group = ifelse(param == "Phi", 
                              str_remove(group, "Phi "), 
                              str_remove(group, "p ")),
               year = as.numeric(str_sub(group, -4, -1)),
               a = str_sub(group, -7, -7),
               age = ifelse(a == "0", "J", "A"),
               sexMF = str_sub(group, 2, 2)) %>%
        select(param, year, age, sexMF, estimate, se, lcl, ucl)
      real_ests_Phi <- real_ests %>%
        filter(param == "Phi")
      real_ests_p <- real_ests %>%
        filter(param == "p") %>%
        select(-age)
      
      # Create dataframe to organize the real survival estimates
      Phi_ests <- data.frame(Time = rep(min(age_ddl$Phi$Time):max(age_ddl$Phi$Time), 4),
                             ageclass = rep(c(0, 0, 1, 1), each = 8),
                             sex = rep(c(0, 1, 0, 1), each = 8)) %>%
        mutate(year = Time + 2003,
               age = ifelse(ageclass == 0, "J", "A"),
               sexMF = ifelse(sex == 0, "F", "M"))
      Phi_ests <- Phi_ests %>%
        left_join(real_ests_Phi, by = c("year", "age", "sexMF")) %>%
        filter(!is.na(estimate)) %>%
        group_by(age, sexMF) %>%
        mutate(nyears = length(year)) %>%
        ungroup() %>%
        data.frame() %>%
        mutate(year = ifelse(nyears == 1, "All years", as.character(year))) %>%
        select(-c(Time, ageclass, sex, nyears)) %>%
        relocate(param)
      
      # Create a dataframe to organize the real recapture probability estimates
      p_ests <- data.frame(Time = rep(min(age_ddl$Phi$Time):max(age_ddl$Phi$Time), 2),
                           sex = rep(c(0, 1), each = 8)) %>%
        mutate(year = Time + 2003,
               sexMF = ifelse(sex == 0, "F", "M"))
      p_ests <- p_ests %>%
        left_join(real_ests_p, by = c("year", "sexMF")) %>%
        filter(!is.na(estimate)) %>%
        group_by(sexMF) %>%
        mutate(nyears = length(year)) %>%
        ungroup() %>%
        data.frame() %>%
        mutate(year = ifelse(nyears == 1, "All years", as.character(year))) %>%
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
