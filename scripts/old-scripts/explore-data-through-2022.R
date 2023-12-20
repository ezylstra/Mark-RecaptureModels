# Exploring Broad-tailed hummingbird data 
# 2023-06-25

library(tidyverse)
library(lubridate)
library(RMark)
library(ggplot2)

# Bring in raw data
dat <- read.csv("data/2002-2022_raw_data.csv", 
                strip.white = TRUE, na.strings = c("NA", ""))

#------------------------------------------------------------------------------#
# Basic clean up and formatting of the full dataset for all species
#------------------------------------------------------------------------------#

# Keep only the columns we need and a little cleanup...
dat2 <- dat %>% 
  select(c(Protocol, Initials.Bdr, Location, date, 
           Band.Status, Band.Number, BAND.SIZE, Species, Sex, 
           Age, Removed.Band.Number, CPBreed)) %>%
  # Capitalize character strings
  mutate_if(is.character, toupper) %>%
  # Format date and create date-related variables
  mutate(date = mdy(date),
         yr = year(date),
         mon = month(date),
         day = day(date),
         doy = yday(date)) %>%
  rename(protocol = Protocol,
         bander = Initials.Bdr, 
         loc = Location,
         band_status = Band.Status,
         band = Band.Number,
         band_size = BAND.SIZE,
         species = Species,
         sex = Sex,
         age = Age,
         band_removed = Removed.Band.Number,
         breed = CPBreed)

# Clean up band numbers
# If any band = "XXXXXX", replace it with NA and then make column numeric
if (any(unique(dat2$band) == "XXXXXX")) {
  message("Replacing band number 'XXXXXX' with NA")
  dat2 <- dat2 %>% 
    mutate(band = na_if(band, "XXXXXX")) %>%
    mutate(band = as.numeric(band))
}

# Merge data for banding locations that are close to each other or were moved 
# just a little ways away so they're basically the same site
  # MA - MA1 <- YES, Both sites close together MA1 has data  for 2002, MA has data from 2003 forward 
  # AL - AL1 <- NO, AL1 has 5 records but on different monitoring dates than AL, maybe it was a training site? Protocol is HMN for both
  # RA - RA1 <- NO, Both sites in Arizona, RA1 is a "training" site 
  # PA - PA2 <- NO, PA is a site in Arizona, PA2 is a site in British Columbia 
  # SWRS - SWRS1 <- YES, SWRS1 was the site we used while the station was closed due to covid, they are very close together
  # W1 - NO, training site 
  # T2 - NO, training site    
  # SH (2002-2003) - ML (2005-2022) <- YES, sites are very close together and 3 birds have good recapture data
  sort(unique(mdy(dat$date[dat$Location == "SH"])))

dat2$loc[dat2$loc == "MA1"] <- "MA"
dat2$loc[dat2$loc == "SWRS1"] <- "SWRS"
dat2$loc[dat2$loc == "SH"] <- "ML"

# Sort data by band, date, species, and sex
dat2 <- dat2 %>% 
  arrange(band, date, species, sex)

# Verify that first use of a band number corresponds to band status 1 (new),
# and following captures correspond to band status R (recapture) 

# Extract first captures (1) and recaptures (R) 
dat_1r <- filter(dat2, band_status %in% c("1","R"))

# Extract unique band numbers
new_bands <- unique(dat_1r$band)

# Create new "best_band_status" column
dat_1r$best_band_status <- NA

# Create a capture number column and add sequence of captures
dat_1r$cap_number <- sequence(from = 1, rle(dat_1r$band)$lengths)

# Fill in best_band_status with 1 and R  
dat_1r$best_band_status <- ifelse(dat_1r$cap_number == 1, "1", "R")
count(dat_1r, band_status, best_band_status)
# Looks like there were ~3200 instances with wrong status

# Subset other band status from data 
# 4 = band destroyed, 6 = band removed, 8 = band lost, F = foreign band
dat_other <- dat2 %>% filter(band_status %in% c("4", "6", "8", "F"))

# Merge so we have all band statuses (1, R, 4, 6, 8, F) and clean up
dat3 <- dat_1r %>%
  select(-cap_number) %>%
  bind_rows(dat_other) %>% 
  # Replace NA values for band status 4, 6, 8, and F in best_band_status
  mutate(best_band_status = ifelse(is.na(best_band_status), 
                                   band_status, 
                                   best_band_status)) %>%
  select(-band_status) %>%
  rename(band_status = best_band_status) %>%
  arrange(band, date)

#------------------------------------------------------------------------------#
# Check that each band number is associated with a single species and sex
#------------------------------------------------------------------------------#

check_band <- dat3 %>% 
  filter(!is.na(band)) %>%
  group_by(band) %>%
  summarize(n_spp = length(unique(species)),
            n_sex = length(unique(sex)))

# Extract band numbers associated with multiple species
error_spp <- check_band$band[check_band$n_spp > 1]

# Gaby's notes on these bands below:
  # Band Numbers with errors: database info matches field datasheets
  # 710018209: Asked BC bander to check this issue, waiting on response. 
  # 710027442: Banded 2018 as ANHU F age 2, recaptured as BCHU F age 1 in 2022 @ FH
  # 710027763: Recap 2020 as ANHU M @ HC, banded 2021 as RUHU M @ MPGF. Mistake in Rec
  # 710069805: Banded Aug/2020 as RUHU M @ FH, recap Mar/2020 as BBLH M @ HC. Mistake in Recapture
  # 810005126: Banded Jun/2020 as BCHU F @ ESC, recap Aug/2020 as HYHU @ ESC. Don't know mistake 
  # 810008256: Band applied twice Jul/14/21 as RUHU F @ WCAT and Jul/31/21 as BCHU F @ REDC. Don't know mistake
  # 810008431: Banded in 2021 @ REDC as BCHU F age 1, then band destroyed in 2022
  # 810008438: Banded in 2021 as RUHU F age 1, then recaptured in 2022 as BCHU F age 1 @ ESC

# Extract band numbers associated with multiple sexes
error_sex <- check_band$band[check_band$n_sex > 1]

dat3[dat3$band %in% error_sex,]
  # 310089627 was a VCHU, which isn't too surprising
  # 710018209 was a M CAHU captured in 2021 and a F RUHU captured in 2022 (LU)
  # 810008431 is OK: first capture labeled as F BCHU, then BADE with sex = NA
error_sex <- error_sex[error_sex != 810008431]

# For now, remove problematic bands from dataset
dat3 <- filter(dat3, !band %in% c(error_spp, error_sex))

#------------------------------------------------------------------------------#
# Check age data
#------------------------------------------------------------------------------#
# Juvenile: age 2
# Adult: age 1

# First, check that birds were assigned the same age for captures in same year
check_age1 <- dat3 %>% 
  filter(!is.na(band)) %>%
  group_by(band, species, loc, yr) %>%
  summarize(n_age = length(unique(age)),
            .groups = "keep") %>%
  data.frame()
check_age1[check_age1$n > 1,]
  # Two ANHU issues in 2020. Will remove these bands for now.
dat3 <- filter(dat3, !band %in% check_age1$band[check_age1$n > 1])

# Second, check that if birds were labeled as juveniles in one year, they were
# labeled as adults when recaptured
check_age2 <- dat3 %>%
  filter(!is.na(band)) %>%
  select(band, species, yr, age) %>%
  distinct() %>%
  arrange(band, species, yr) %>%
  group_by(band, species) %>%
  summarize(n_yrs = length(unique(yr)),
            ageF = age[1],
            age2 = ifelse(n_yrs > 1, age[2], NA),
            ageL = age[length(age)],
            .groups = "keep") %>%
  filter(n_yrs > 1) %>%
  data.frame()

count(check_age2, ageF, age2, ageL)
  # One band with second and last age = NA
  filter(dat3, band %in% check_age2$band[is.na(check_age2$age2)])
  # CAHU in 2020, 2022. No idea why age is NA

  # One bird with age2 = 2, ageL = 1
  filter(dat3, 
         !is.na(band) &
         band %in% check_age2$band[check_age2$age2 ==2 & check_age2$ageL == 1])
  # BBLH captured in 2008 and 2009 (both age 2), and 2011-2012 (age 1)
  
  # 8 birds with age2 = 2, ageL = 2
  filter(dat3, 
         !is.na(band) &
           band %in% check_age2$band[check_age2$age2 ==2 & check_age2$ageL == 2])
  # All birds just caught in 2 or 3 years, always as age 2.  
  
# For now, removing all these bands from the dataset
age_probs <- check_age2$band[is.na(check_age2$age2) | check_age2$age2 == 2] 
dat3 <- filter(dat3, !band %in% age_probs)

#------------------------------------------------------------------------------#
# Subset BTLH data
#------------------------------------------------------------------------------#

# Select BTLH data for capture-recapture analysis:
  # Only sites that follow HMN's protocol 
  # Only captures with a band number
  # Only individuals that were assigned sex (F/M; note that there were only 14 
    # birds with unknown sex and none of them were recaptured).

btlh <- dat3 %>%
  filter(species == "BTLH" & !is.na(band) & protocol == "HMN" & sex != "U")

btlh_sites <- btlh %>%
  group_by(loc) %>%
  summarize(n_yrs = length(unique(yr)),
            n_birds = length(unique(band)),
            first_yr = min(yr),
            last_yr = max(yr)) %>%
  arrange(desc(n_yrs), desc(n_birds)) %>%
  data.frame()

# Attach site information
sites <- read.csv("data/BTLH_sites.csv", header = TRUE, strip.white = TRUE,
                  col.names = c("loc", "lat", "long", "elev"))
btlh_sites <- left_join(btlh_sites, sites, by = "loc") %>%
  filter(n_yrs > 1 & n_birds > 10)

# Quick summary table of sites with elevation > 2000 m
site_summary <- btlh %>%
  filter(loc %in% btlh_sites$loc[btlh_sites$elev > 2000]) %>%
  group_by(loc) %>%
  summarize(n_caps = length(band),
            n_indiv = length(unique(band)),
            n_recaps = length(unique(band[band_status == "R"])),
            n_males = length(unique(band[sex == "M"])),
            n_females = length(unique(band[sex == "F"]))) %>%
  mutate(recap_rate = round(n_recaps / n_caps, 2)) %>%
  arrange(desc(n_caps)) %>%
  data.frame()
site_summary

# Looks like Gaby was interested in 4 sites with > 1000 captures (ML, WCAT, 
# PCBNM, and DGS).  We'll start with ML for now.   

#------------------------------------------------------------------------------#
# Explore Mount Lemmon data
#------------------------------------------------------------------------------#

# First, explore seasonal constraints, so we're focused on the breeding population
  # Using May 16th - July
  btlh_allmay <- btlh %>%
    filter(loc == "ML" & yr %in% 2002:2022) %>%
    filter(mon %in% 5:7)  
  # Using May - July
  btlh_midmay <- btlh %>% 
    filter(loc == "ML" & yr %in% 2002:2022) %>%
    filter((mon == 5 & day > 15) | mon %in% 6:7)
  # Compare number of birds, only using adult captures:
  length(unique(btlh_allmay$band[btlh_allmay$age == 1])) # 2292 birds
  length(unique(btlh_midmay$band[btlh_midmay$age == 1])) # 2279 birds
  # Compare number of birds, using all captures:
  length(unique(btlh_allmay$band)) # 2468 birds 
  length(unique(btlh_midmay$band)) # 2455 birds
  # Defining season as mid-May through July doesn't reduce overall sample size
  # much at all, so probably best to use May 16 as a start date.

# Explore juvenile vs adult captures (only captures between mid-May and July)
  btlh_ml_ja <- btlh %>% 
    filter(loc == "ML" & yr %in% 2002:2022) %>%
    filter((mon == 5 & day > 15) | mon %in% 6:7) %>%
    select(band, loc, species, sex, age, yr) %>%
    distinct() %>%
    arrange(band, loc, species, yr) %>%
    group_by(band, loc, species) %>%
    summarize(n_yrs = length(unique(yr)),
              ageF = age[1],
              ageL = ifelse(n_yrs > 1, age[2], NA),
              .groups = "keep") %>%
    data.frame()
  count(btlh_ml_ja, ageF, n_yrs)
  # 1846 of 2455 birds were captured in only one year
  # 176 birds were captured only once as a juvenile
  
  btlh_ml_ja_r <- btlh_ml_ja %>%
    filter(n_yrs > 1)
  count(btlh_ml_ja_r, ageF, ageL)
  # 50 birds were captured first as juveniles, then later as adults

# Subset adult data (only adult captures between mid-May and July) and 
# calculate the number of years each bird was captured
btlh_ml <- btlh %>% 
  filter(loc == "ML" & yr %in% 2002:2022) %>%
  filter((mon == 5 & day > 15) | mon %in% 6:7) %>%
  filter(age == 1) %>%
  arrange(band) %>% 
  select(band, sex, band_removed, yr, mon, day, doy, band_status) %>%
  group_by(band) %>%
  mutate(n_yrs = length(unique(yr))) %>%
  ungroup() %>%
  data.frame()

# How many captures in each year?
  capsperyr <- btlh_ml %>%
    count(., yr)
  arrange(capsperyr, n)
  # Lots of captures in 2002 (2nd most ever)
  # Way fewer captures in 2003 (n = 29); few in 2005 (102) and 2010 (113)
  # All other years > 131
  
  # Given these differences, may be worth exploring whether an analysis using
  # 2005-2022 data would be much different than an analysis using 2002-2022 data

# See how many adult birds were first captured in 2002 and subsequently captured 
# in 2003 and 2005-after
  cap02 <- btlh_ml %>%
    filter(yr == 2002) %>%
    select(band) %>%
    distinct() %>%
    unlist()
  recap02 <- btlh_ml %>%
    filter(band %in% cap02) %>%
    group_by(band, sex) %>%
    summarize(cap03 = ifelse(2003 %in% yr, 1, 0),
              cap05 = ifelse(2005 %in% yr, 1, 0),
              cap06a = ifelse(max(yr) > 2005, 1, 0),
              .groups = "keep") %>%
    data.frame()
  count(recap02, cap03, cap05, cap06a)
  # 8 birds recaptured in 2003, 1 in 2005, and 2 in 2006 (or after)

# Quantify annual survey effort (with the cap-recap data alone). 
# How many surveys and on what dates (under the assumption that at least one 
# adult BTLH was captured during each session).
ml_surveys <- btlh_ml %>%
  select(yr, mon, day, doy) %>%
  distinct() %>%
  arrange(doy) %>%
  group_by(yr) %>%
  summarize(n_surveys = length(doy),
            first = min(doy),
            last = max(doy),
            mn = round(mean(doy))) %>%
  data.frame()
ml_surveys  
# Clearly, 2003 is different than the rest:
  # Only 3 surveys (2 other years with 4 and all other years with 5-6)
  # Mean survey doy = 152 (all other years 167-178)

#------------------------------------------------------------------------------#
# Create capture histories for Mount Lemmon
#------------------------------------------------------------------------------#

# Identify years when surveys didn't occur 
nosurvey_yrs <- setdiff(min(btlh_ml$yr):max(btlh_ml$yr), 
                        sort(unique(btlh_ml$yr)))  
  
# Create histories for all captures of BTLH on Mount Lemmon
  ch_ml <- btlh_ml %>%
    group_by(band, sex, yr, n_yrs) %>%  
    summarize(n_obs = length(yr), .groups = "keep") %>%
    mutate(obs = 1) %>% 
    pivot_wider(names_from = yr, 
                values_from = obs, 
                id_cols = c(band, sex, n_yrs), 
                values_fill = 0)
  
    # Create columns with "." for years when no surveys occurred
    for (i in 1:length(nosurvey_yrs)) {
      ch_ml[ ,as.character(nosurvey_yrs)] <- "."
    }
  
  ch_ml <- ch_ml %>%
    relocate(band, sex, as.character(min(btlh_ml$yr):max(btlh_ml$yr)), n_yrs) %>%
    # Create CHs with dots for missing years
    unite(ch_dots, as.character(min(btlh_ml$yr):max(btlh_ml$yr)), sep = "") %>%
    # Create CHs without dots (will need to specify interval lengths in RMark)
    mutate(ch = str_remove_all(ch_dots, "[.]")) %>%
    data.frame()

  count(ch_ml, n_yrs)
  # 2279 individuals / capture histories
    # 1705 captured in 1 year
    # 338 in 2 years
    # 157 in 3 years
    # 43 in 4 years
    # 26 in 5 years
    # 10 in 6 years
  
  # Taking a quick look at individuals captured in 5-6 different years
  ch_ml %>% filter(n_yrs > 4) %>% arrange(desc(n_yrs), band)

#------------------------------------------------------------------------------#
# Create some very simple covariates?
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
# Running CJS capture-recapture models in RMark
#------------------------------------------------------------------------------#

# Make sex a factor
ch_ml$sex <- as.factor(ch_ml$sex)

# For now, use CHs without dots (so remove ch_dots from dataframe)
ch_ml <- select(ch_ml, -ch_dots)

# Calculate duration of survival intervals (in years)
survey_yrs <- sort(unique(btlh_ml$yr))
survey_intervals <- diff(survey_yrs)

# Create list of processed data
ml_process <- process.data(data = ch_ml,
                           begin.time = min(btlh_ml$yr),
                           time.intervals = survey_intervals,
                           model = "CJS",
                           groups = "sex")

# Make design data
  # To create the design data and maintain flexibility, they recommend creating 
  # the default design data and then adding other binned variables with the 
  # function add.design.data. 
ml_ddl <- make.design.data(data = ml_process)

# Create data frame with covariates for recapture probability (p)
p_covar <- ml_surveys %>%
  filter(yr > 2002) %>%
  select(yr, n_surveys, mn) %>%
  mutate(n_surveys_z = (n_surveys - mean(n_surveys)) / sd(n_surveys),
         survey_date_z = (mn - mean(mn)) / sd(mn)) %>%
  rename(time = yr)
#Adding yearly covariates into design data:
ml_ddl$p <- merge_design.covariates(ml_ddl$p, p_covar)

#Numerous ways of viewing/checking the data
  str(ml_ddl)
  ml_ddl$Phi[1:25,]
  ml_ddl$p[1:25,]
  summary(ml_ddl$Phi)
  summary(ml_ddl$p)
  
# Potential covariates:
  # sex (Phi or p)
  # year (Phi or p) = time
  # temporal trend (Phi) = Time
  # number of surveys each year (p) = n_surveys_z (standardized)
  # mean survey date each year (p) = survey_date_z (doy, standardized)
  # [eventually] ML weather during late spring/summer (Phi or p)
  # [eventually] weather in MX during winter

# Run a simple set of candidate models
m_simple <- function(){
  
  Phi.null <- list(formula = ~ 1)
  Phi.sex <- list(formula = ~ sex)
  Phi.yr <- list(formula = ~ time)
  Phi.trend <- list(formula = ~ Time)
  Phi.sextrend <- list(formula = ~ Time + sex)

  p.null <- list(formula = ~ 1)
  p.sex <- list(formula = ~ sex)
  p.yr <- list(formula = ~ time)
  p.nsurvs <- list(formula = ~ n_surveys_z)
  p.date <- list(formula = ~ survey_date_z)
  p.nsurvsdate <- list(formula = ~ n_surveys_z + survey_date_z)
  
  # For CJS models, create.model.list() creates a dataframe of all combinations 
  # of parameter specifications for Phi and p. This function scans the 
  # environment and collects all list objects that contain a formula and have 
  # names that contain "Phi." or "p."
  cml <- create.model.list("CJS")
  
  # mark.wrapper() uses a dataframe of parameter specifications created by 
  # create.model.list() to construct and run each model. Results are 
  # returned as a marklist with a model.table constructed by collect.models().
  results <- mark.wrapper(cml, data = ml_process, ddl = ml_ddl, output = FALSE)
  return(results)
}
results_simple <- m_simple()
results_simple

# For null model: Phi = 0.48, p = 0.54 
results_simple[[4]]$results$real
results_simple[[4]]$results$beta

# For sex model: PhiF = 0.53, PhiM = 0.41, pF = 0.57, pM = 0.49
results_simple[[11]]$results$real
results_simple[[11]]$results$beta

# Highest ranked model (by AICc) is Phi(Time + sex)p(time)
results_simple[[18]]$results$real
results_simple[[18]]$results$beta
# I'm guessing that p(time) rated higher than models with survey covariates 
# primarily because 2005 p-hat was very low (and covariate values are near mean)

# Generally survey date seems to have more explanatory power than n_surveys

# P(Time + sex)p(survey_date_z)
results_simple[[13]]$results$real
results_simple[[13]]$results$beta

# For Phi(sex)p(time), which is the 3rd highest ranked model:
results_simple[[6]]$results$real
results_simple[[6]]$results$beta
# Obviously, p estimates in 2004 and 2020 are nonsensical (no surveys)
# Yearly p values range from 0.4-0.8, except values < 0.07 in 2003, 2005 and 
# values in 2011 = 0.84.

# For Phi(Time + sex)p(time), which is the highest ranked model
results_simple[[12]]$results$real
results_simple[[12]]$results$beta
# Big increases in Phi for both males (0.33-0.51) and females (0.44-0.63)

# For Phi(Time * sex)p(time), which is the 2nd highest ranked model
results_simple[[9]]$results$beta
# This had problems estimating lots of p's (which is not surprising)
# Plus, looking at Phi(Time * sex)p(sex), there's little evidence that Phi
# trends differed between sexes.

# IMPORTANT: Get rid of all the mark files that will clog the repo:
rm(results_simple)
cleanup(ask = F)

# Run a second (slightly more refined) set of candidate models
m_simple2 <- function(){
  
  Phi.sex <- list(formula = ~ sex)
  Phi.trend <- list(formula = ~ Time)
  Phi.sextrend <- list(formula = ~ Time + sex)
  Phi.sexXtrend <- list(formula = ~ Time * sex)
  
  p.sex <- list(formula = ~ sex)
  p.yr <- list(formula = ~ time)
  p.date <- list(formula = ~ survey_date_z)
  p.sexdate <- list(formula = ~ sex + survey_date_z)
  p.sexyr <- list(formula = ~ sex + time)
  
  cml <- create.model.list("CJS")
  results <- mark.wrapper(cml, data = ml_process, ddl = ml_ddl, output = FALSE)
  return(results)
}
results_simple2 <- m_simple2()
results_simple2

# Four models clearly better than the rest (Delta < 2.5, all others > 13)
# Combinations of Phi(Time + sex) [slightly better] or Phi(Time * sex)
# and p(time) [slightly better] or p(sex + time)

# Most complicated: Phi(Time * sex)p(sex + time)
results_simple2[[14]]$results$beta
# No obvious estimation problems, so that's good
# Phi:interaction near 0
# p:sexM is negative but overlaps 0 by a lot

# Best model: Phi(Time + sex)p(time)
results_simple2[[10]]$results$beta
results_simple2[[10]]$results$real

# Visualize estimates from a Phi(Time + sex)p(time) model
  
  # Isolate the model you want to use for inferences (only necessary if you
  # ran a bunch of models with a function.  If not, you can use the original
  # model name instead of "best")
  best <- results_simple[[10]]
  
  # Look at beta-hats
  best$results$beta
  
  # Extract real estimates (Note that this is just for years when surveys were done)
  best$results$real
  # Separate rownames into useful columns and do a little clean up
  reals <- best$results$real %>%
    rownames_to_column("rowname") %>%
    separate_wider_delim(rowname, 
                         delim = " ",
                         names = c("param", "sex", NA, NA, "yr")) %>%
    mutate(sex = str_sub(sex, 2, 2),
           yr = as.numeric(str_sub(yr, 2, 5))) %>%
    select(-c(fixed, note)) %>%
    data.frame()
  reals
  
  # Visualize recapture probabilities -----------------------------------------#
  
  # Plot estimates of recapture probability (just for years when surveys done)
  p_reals <- reals %>%
    filter(param == "p")
  p_fig <- ggplot(p_reals, aes(x = yr, y = estimate)) +
    geom_point(size = 1.5) +
    geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0) +
    theme_classic() +
    ylab("Estimated recapture probability (95% CI)") + 
    xlab("")
  p_fig
  
  
  # Visualize survival probabilities ------------------------------------------#
  # Here, I'm not using values from the dataframe with real estimates (reals) 
  # because it doesn't include years when surveys weren't done, and we'd like 
  # estimates for every year and we'd like to create smooth curves over time for
  # trend models.  Instead, we'll have to use the beta hats to calculate the
  # real estimates.
  
  # Create values of Time to use for predicting Phi
  Time.values <- seq(0, max(ml_ddl$Phi$Time), by = 0.2)
  # Convert Time to year, for axis labels
  year.values <- Time.values + 2002 
  
  # Create dataframe to store predictions for each combination of sex and Time
    # (Note that we're calculating values for a fraction of a year in order to 
    # generate smooth curves. Below we'll just extract the years and plot each
    # separately)
  pred_df <- data.frame(Intercept = 1,
                        Time = rep(Time.values, 2),
                        sexM = rep(c(0, 1), each = length(Time.values)))
  # Look at prediction dataframe
  head(pred_df); tail(pred_df)
  
  # Extract beta-hats with lower/upper CIs for Phi parameter
    # Note: you could use the mean +/- SE instead if you'd like
  betas <- best$results$beta$estimate[1:3]
  betas_lcl <- best$results$beta$lcl[1:3]
  betas_ucl <- best$results$beta$ucl[1:3]
  # betas_lse <- betas - best$results$beta$se[1:3]
  # betas_use <- betas + best$results$beta$se[1:3]
  
  # Make predictions on the logit scale (mean, lower CI, upper CI)
    # %*% is used for matrix multiplication. Here it means we'll multiply each 
    # row of the pred_df dataframe by the vector of beta-hats. 
  estimate_logit <- as.matrix(pred_df) %*% as.matrix(betas)
  lcl_logit <- as.matrix(pred_df) %*% as.matrix(betas_lcl)
  ucl_logit <- as.matrix(pred_df) %*% as.matrix(betas_ucl)
  
  # Convert predictions to the probability (real) scale
  pred_df$estimate <- exp(estimate_logit) / (1 + exp(estimate_logit))
  pred_df$lcl <- exp(lcl_logit) / (1 + exp(lcl_logit))
  pred_df$ucl <- exp(ucl_logit) / (1 + exp(ucl_logit))
  
  # Add year to dataframe
  pred_df$yr <- year.values
  # Check that predictions for 2002 look the same as what's in model output
  head(pred_df); head(reals)
  
  # Plot smooth survival curves for males, females
  pred_df$Sex <- as.factor(ifelse(pred_df$sexM == 1, "Male", "Female"))
  phi_fig <- ggplot(pred_df, aes(x = yr, y = estimate, group = Sex)) +
    geom_line(size = 1.5, aes(color = Sex)) +
    geom_ribbon(aes(ymin = lcl, ymax = ucl, fill = Sex), alpha = 0.2) +
    theme_classic() +
    ylab("Estimated annual survival (95% CI)") + 
    xlab("")
  phi_fig
  
  # Plot annual values with error bars (= 95% CIs)
  phi_annual <- filter(pred_df, yr %in% min(yr):max(yr))
  phi_fig_ann <- ggplot(phi_annual, aes(x = yr, y = estimate, group = Sex)) +
    geom_point(size = 1.5, aes(color = Sex), 
               position = position_dodge(width = 0.6)) +
    geom_errorbar(aes(ymin = lcl, ymax = ucl, color = Sex), width = 0,
                  position = position_dodge(width = 0.6)) +
    theme_classic() +
    ylab("Estimated annual survival (95% CI)") + 
    xlab("")
  phi_fig_ann 

  
# IMPORTANT: Get rid of all the mark files that will clog the repo:
rm(results_simple2)
cleanup(ask = F)

#------------------------------------------------------------------------------#
# Create capture histories adult and juvenile captures on Mount Lemmon
#------------------------------------------------------------------------------#

# Subset adult and juvenile data by date (between mid-May and July) and 
# calculate the number of years each bird was captured
aj <- btlh %>% 
  filter(loc == "ML" & yr %in% 2002:2022) %>%
  filter((mon == 5 & day > 15) | mon %in% 6:7) %>%
  arrange(band, date) %>% 
  select(band, age, sex, yr) %>%
  distinct() %>%
  group_by(band) %>%
  mutate(cap_age = age[1],
         n_yrs = length(unique(yr))) %>%
  ungroup() %>%
  data.frame()

# Identify years when surveys didn't occur 
nosurvey_yrs <- setdiff(min(aj$yr):max(aj$yr), sort(unique(aj$yr))) 

# Create capture histories
ch_aj <- aj %>%
  group_by(band, sex, cap_age, yr, n_yrs) %>%  
  summarize(n_obs = length(yr), .groups = "keep") %>%
  mutate(obs = 1) %>% 
  pivot_wider(names_from = yr, 
              names_sort = TRUE,
              values_from = obs, 
              id_cols = c(band, sex, cap_age, n_yrs), 
              values_fill = 0) %>%
  unite(ch, as.character(min(aj$yr)):as.character(max(aj$yr)), sep = "") %>%
  data.frame()

count(ch_aj, cap_age, n_yrs)
  # 2455 individuals / capture histories
  # 1846 captured in 1 year (176 J, 1670 A)
  # 362 in 2 years (35 J, 327 A)
  # 165 in 3 years (11 J, 154 A)
  # 45 in 4 years (3 J, 42 A)
  # 27 in 5 years (1 J, 26 A)
  # 10 in 6 years (all A)

#------------------------------------------------------------------------------#
# Running CJS capture-recapture models in RMark
#------------------------------------------------------------------------------#

# Quantify annual survey effort (with the cap-recap data alone). 
# How many surveys and on what dates (under the assumption that at least one 
# adult BTLH was captured during each session).
ml_surveys <- btlh %>% 
  filter(loc == "ML" & yr %in% 2002:2022) %>%
  filter((mon == 5 & day > 15) | mon %in% 6:7) %>%
  select(yr, mon, day, doy) %>%
  distinct() %>%
  arrange(doy) %>%
  group_by(yr) %>%
  summarize(n_surveys = length(doy),
            first = min(doy),
            last = max(doy),
            mn = round(mean(doy))) %>%
  data.frame()
ml_surveys  
# Clearly, 2003 is different than the rest:
# Only 3 surveys (2 other years with 4 and all other years with 5-6)
# Mean survey doy = 152 (all other years 167-178)

# Make sex and age factors
ch_aj$sex <- factor(ch_aj$sex)
ch_aj$cap_age <- factor(ch_aj$cap_age, levels = c(2, 1))

# Calculate duration of survival intervals (in years)
survey_yrs <- sort(unique(aj$yr))
survey_intervals <- diff(survey_yrs)

# Create list of processed data for RMark
ml_process <- process.data(data = ch_aj,
                           model="CJS",
                           begin.time = min(aj$yr),
                           time.intervals = survey_intervals,
                           groups = c("sex", "cap_age"),
                           age.var = 2,
                           initial.ages=c(0, 1))
# Create default design matrix 
ml_ddl <- make.design.data(ml_process)

# Create ageclasses, juvenile as age=0, second year as age = 1, after second year = 2+
ml_ddl <- add.design.data(data = ml_process, 
                          ddl = ml_ddl, 
                          parameter = "Phi", 
                          type = "age",
                          bins = c(0, 1, 21), 
                          name = "ageclass",
                          right = FALSE) # bin intervals closed on the right
# Creating an ageclass variable that allows recapture probability to be 
# different for individuals that are 1 year old (were juveniles the previous
# summer) and all older individuals
ml_ddl <- add.design.data(data = ml_process, 
                          ddl = ml_ddl, 
                          parameter = "p", 
                          type = "age",
                          bins = c(1, 2, 21), 
                          name = "ageclass",
                          right = FALSE) # bin intervals closed on the right

# Create data frame with covariates for recapture probability (p)
p_covar <- ml_surveys %>%
  filter(yr > 2002) %>%
  select(yr, n_surveys, mn) %>%
  mutate(n_surveys_z = (n_surveys - mean(n_surveys)) / sd(n_surveys),
         survey_date_z = (mn - mean(mn)) / sd(mn)) %>%
  rename(time = yr)
#Adding yearly covariates into design data:
ml_ddl$p <- merge_design.covariates(ml_ddl$p, p_covar)

#Numerous ways of viewing/checking the data
str(ml_ddl)
ml_ddl$Phi[1:25,]
ml_ddl$p[1:25,]
summary(ml_ddl$Phi)
summary(ml_ddl$p)

# Run a simple set of candidate models
## Need to think about if/how ageclass and Time can be in the same Phi model
ajm_simple <- function(){
  
  Phi.null <- list(formula = ~ 1)
  Phi.sex <- list(formula = ~ sex)
  Phi.trend <- list(formula = ~ Time)
  Phi.sextrend <- list(formula = ~ Time + sex)
  Phi.age <- list(formula = ~ ageclass)
  Phi.agesex <- list(formula = ~ ageclass + sex)
  phi.ageXsex <- list(formula = ~ ageclass * sex)

  p.null <- list(formula = ~ 1)
  p.sex <- list(formula = ~ sex)
  p.yr <- list(formula = ~ time)
  p.nsurvs <- list(formula = ~ n_surveys_z)
  p.date <- list(formula = ~ survey_date_z)
  p.ageclass <- list(formula = ~ ageclass)
  p.ageclasssex <- list(formula = ~ ageclass + sex)
  p.fulladd <- list(formula = ~ ageclass + sex + time)
  
  # For CJS models, create.model.list() creates a dataframe of all combinations 
  # of parameter specifications for Phi and p. This function scans the 
  # environment and collects all list objects that contain a formula and have 
  # names that contain "Phi." or "p."
  cml <- create.model.list("CJS")
  
  # mark.wrapper() uses a dataframe of parameter specifications created by 
  # create.model.list() to construct and run each model. Results are 
  # returned as a marklist with a model.table constructed by collect.models().
  results <- mark.wrapper(cml, data = ml_process, ddl = ml_ddl, output = FALSE)
  return(results)
}
results_simple <- ajm_simple()
results_simple

# To look at real or beta estimates for model number 1:
results_simple[[1]]$results$real
results_simple[[1]]$results$beta

# Best model is Phi(~Time + sex)p(~time), with weight = 0.77

# 2nd best is Phi(~Time + sex)p(~ageclass + sex + time), with weight = 0.23
# This model suggests that recapture rates for 1 yr olds is slightly lower than
# older individuals

# All other models have virtually no weight.

# For null model: Phi = 0.47, p = 0.54 

# For sex model: PhiF = 0.53, PhiM = 0.40, pF = 0.57, pM = 0.50

# Phi(~ageclass + sex)p(time) suggests 1st year survival is lower

# IMPORTANT: Get rid of all the mark files that will clog the repo:
rm(results_simple)
cleanup(ask = F)


