# Seeing if/how we can extract location (and date?) of recaptures
# 20 Feb 2024

# Load packages
library(tidyverse)
library(data.table)
library(janitor)

rm(list = ls())

# Run clean-RMNP-recaptured-data.R through line 561, then the following
# (just needed to do this once)

  # recap <- recaptured.dat %>% 
  #   filter(species == 'BTLH') %>%   
  #   select(UBI_band, band_status, recapture_year, fixed_sex, original_site, 
  #          paste0("recapture_", 1:17), comments) %>%
  #   rename(band = UBI_band, 
  #          recap_yr = recapture_year,
  #          site_orig = original_site,
  #          sex = fixed_sex)
  # colnames(recap)[6:22] <- paste0("r", 1:17)
  # 
  # # Export csv of new data frame
  # write.csv(recap, 
  #           "output/cleaned-recaptured-data-RMNP-full.csv",
  #           row.names = FALSE) 

# Load cleaned, full recap file:
recap <- read.csv("output/cleaned-recaptured-data-RMNP-full.csv",
                  na.strings = c(NA, ""))

# Load cleaned banded file:
banded <- read.csv("output/cleaned-banded-data-RMNP.csv") %>%
  select(-c(X, species)) %>%
  rename(sex = fixed_sex,
         band = UBI_band)

# Load information about sites
sites_df <- read.csv("data/RMNP-sites-data.csv")
sites <- sort(unique(sites_df$site))

# Load letter codes for reference
letters <- read.csv("data/BBL_letter_codes.csv")

# Is there an entry in the first recapture column for every bird/yr?  
sum(is.na(recap$r1)) # No, 7 NAs
filter(recap, is.na(r1))
# There are 7 rows with no entry in recapture columns
# (foreign, rebands, found dead)
  # 2 dead, will remove
  # 4 with NEW BAND and former number in comments, but this recapture info is in
  # banded, so can remove here too.
  # One with no comments (9000-12585). Maybe didn't enter info, but ok?
recap <- filter(recap, !(is.na(r1) & !is.na(comments)))
# Going to enter fake date into r1 column for remaining row (9000-12585 in 2010) 
# so that the recap info doesn't disappear if I convert to long form
recap$r1[is.na(recap$r1)] <- "7/15"

# Move information about rebanding events from banded to recap ----------------#

banded$band_status == "R" # n = 70


# Convert to long form --------------------------------------------------------#

# First, are there duplicates (band * recap_yr)?
repeats <- recap %>%
  group_by(band, recap_yr) %>%
  summarize(nrows = length(band)) %>%
  data.frame() %>%
  filter(nrows > 1) # Yup, 5 of them

for (i in 1:nrow(repeats)) {
  print(filter(recap, band == repeats$band[i], recap_yr == repeats$recap_yr[i]))
}

recapl <- recap %>%
  pivot_longer(cols = r1:r17, 
               names_to = "r_num",
               values_to = "entry") %>%
  data.frame() %>%
  filter(!is.na(entry)) %>%
  distinct()

# Merge this info with that in banded dataset (just original banding events)

banded <- banded %>%
  rename(band_yr = year,
         band_age = age, 
         band_site = site)
recapl <- left_join(recapl, banded, by = "band")

# Check that band_site (banded) == site_orig (recapl)

# Sites -----------------------------------------------------------------------#

sites_search <- paste0(c(sites, tolower(sites)), collapse = "|")
recapl <- recapl %>%
  mutate(site = toupper(str_extract(entry, pattern = sites_search))) %>%
  mutate(at = if_else(str_detect(entry, "at"), 1, 0))

filter(recapl, at == 1 & is.na(site))
# Other locs? YMCA (n = 6); HPK3 (6); Peaceful Valley, CO (1); ARIZONA (1)
# YMCA is on/near park boundary, so may be close to other sites
# Will remove Peaceful Valley and ARIZONA for now since they're either outside 
# park or unknown

# Adding a few sites (that may or may not have been regular sampling locations)
sites_search <- paste0(sites_search, "YMCA", "HPK3")

recapl <- recapl %>%
  filter(!str_detect(entry, "Peaceful Valley|ARIZONA")) %>%
  mutate(site = toupper(str_extract(entry, pattern = sites_search))) %>%
  select(-at) %>%
  mutate(recap_site = if_else(is.na(site), site_orig, site))

# Look at individuals that moved between sites
moved <- recapl %>%
  filter(site_orig != recap_site)

length(unique(moved$band)) # 347 birds 

# See how often these movements occurred within/between seasons

# Create dataframe with site pairs
  # Number of "movements"
  # Distance between sites
  # Same site "cluster"?  (need to add this information to sites...)




# Dates -----------------------------------------------------------------------#

# Looks like many dates have m/dd format or dd-MON format
# (plus a few June/July/August dates entered as 6.X or 7.X)
mmdd <- "[0-9]{2}/[0-9]{2}"
mdd <- "[0-9]/[0-9]{2}"
mmd <- "[0-9]{2}/[0-9]"
md <- "[0-9]/[0-9]"
ddMON <- "[0-9]{2}-[A-Za-z]{3}"
dMON <- "[0-9]-[A-Za-z]{3}"
m.dd <- "[6-8][.][0-9]{2}"
m.d <- "[6-8][.][0-9]"
mdd2 <- "[0-9]-[0-9]{2}"
md2 <- "[0-9]-[0-9]"
dates <- paste0(c(mmdd, mdd, mmd, md, ddMON, dMON, m.dd, m.d, mdd2, md2), 
                collapse = "|")

# Extract date (in any format) from r1 column
datesd <- str_extract(recap$r1, dates)
sum(is.na(datesd)) # 2 entries where a date wasn't extracted
recap[is.na(datesd), c("band", "recap_yr", "r1", "comments")] # Yup, no date

sort(unique(datesd)) # Most look ok (but need to check 4/16, 61/7, 7/63, 9/22)

# Check all other recapture columns
r217 <- as.matrix(recap[,paste0("r", 2:17)])
# Create a vector (reads down r2 column, then r3 column, etc)
recap217 <- as.vector(ifelse(is.na(r217), 0, 1))
# Extract dates
dates217 <- str_extract(r217, dates)
sort(unique(dates217)) # Most look ok (but need to check 3/16, 6/64, 7/63, 8/83)

recap217_check <- which(recap217 == 1 & is.na(dates217))
# Find rows with missing date entries
rows_missingdates <- arrayInd(recap217_check, .dim = dim(r217))[,1]
recap[rows_missingdates,]










