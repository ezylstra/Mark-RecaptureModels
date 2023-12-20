# Extracting dates from recapture_1:recapture_17 columns

# First running clean-RMNP-recaptured-data.R
dat <- recaptured.dat

# Should probably first check that all bands appearing in the rebanded dataset
# are in the banded dataset. Assuming that's been done...

# Is there an entry in the first recapture column for every bird/yr?  
sum(is.na(dat$recapture_1)) # No NAs
filter(dat, recapture_1 == "")
# There are 13 rows with no entry in recapture columns
# (foreign, rebands, found dead, found in distant location)
# Removing these for now....
dat <- filter(dat, recapture_1 != "")

# Removing any species other than BTLH
dat <- filter(dat, species == "BTLH")

# Looks like many dates have m/dd format or dd-MON format
mmdd <- "[0-9]{2}/[0-9]{2}"
mdd <- "[0-9]/[0-9]{2}"
mmd <- "[0-9]{2}/[0-9]"
md <- "[0-9]/[0-9]"
ddMON <- "[0-9]{2}-[A-Za-z]{3}"
dMON <- "[0-9]-[A-Za-z]{3}"
dates <- paste0(c(mmdd, mdd, mmd, md, ddMON, dMON), collapse = "|")

# Extract date (in any format) from recapture_1 column
datesd <- str_extract(dat$recapture_1, dates)
sort(unique(datesd)) # Most look ok (but need to check 4/16, 61/7, 7/63,)

sum(is.na(datesd)) # 13 entries where a date wasn't extracted
dat[is.na(datesd), c("band_number", "original_date", "recapture_year", "recapture_1", "comments")]
# 4000-88228: looks like recapture_1 contains info about when it was banded in previous year (by others)
# 5 entries with foreign bands (I think entry represents first time captured by Fred)
# 7 entries with no recapture date (but some in different year than banding, so would like to keep in there)

# Wondering whether it's worth extracting recapture dates.
  # Almost all trapping sessions occurred from May - early Sept (based on recap dates)

  # IF birds encountered in RMNP are unlikely to be migrating individuals that
  # are breeding further north, then date doesn't really matter. If otherwise, 
  # then we'll want to restrict the sampling period. 



