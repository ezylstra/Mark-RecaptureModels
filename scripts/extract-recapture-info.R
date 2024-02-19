# Seeing if/how we can extract location (and date?) of recaptures
# 19 Feb 2024

library(dplyr)
library(stringr)
library(lubridate)

rm(list = ls())

# Load information about sites
sites_df <- read.csv("data/RMNP-sites-data.csv")
sites <- sort(unique(sites_df$site))

# Load recapture files
recap_files <- list.files(path = "data/RMNP-recaptured-by-year/", 
                          pattern = "*.csv", 
                          full.names = TRUE)
recap_list <- lapply(recap_files, read.csv)
recap <- bind_rows(recap_list)
count(recap, Recapture.Year) # Ranges from 131-782

# Rename columns, remove species other than BTLH
recap <- recap %>%
  filter(Species == "BTLH") %>%
  select(-Species) %>%
  rename(band = Band.Number,
         date_orig = Original.Date,
         site_orig = Original.Site,
         sex_orig = Original.Sex,
         age_orig = Original.Age,
         yr_recap = Recapture.Year,
         comments = Comments)
colnames(recap)[7:23] <- paste0("r", 1:17)

# Forgetting about band number issues for now....

# Is there an entry in the first recapture column for every bird/yr?  
sum(is.na(recap$r1)) # No NAs
filter(recap, r1 == "")
# There are 8 rows with no entry in recapture columns
# (foreign, rebands, found dead)

# Remove (and set aside) birds that got a new band because I'm assuming they 
# appear in banding dataset (will need to check this)
newbands <- filter(recap, r1 == "" & str_detect(comments, "NEW"))
# Remove birds found dead, birds with comments = "Foreign" or comments = NA
recap <- filter(recap, r1 != "")

# Dates -----------------------------------------------------------------------#
# Might be easier to start with since they seem to appear most

# Looks like many dates have m/dd format or dd-MON format
mmdd <- "[0-9]{2}/[0-9]{2}"
mdd <- "[0-9]/[0-9]{2}"
mmd <- "[0-9]{2}/[0-9]"
md <- "[0-9]/[0-9]"
ddMON <- "[0-9]{2}-[A-Za-z]{3}"
dMON <- "[0-9]-[A-Za-z]{3}"
dates <- paste0(c(mmdd, mdd, mmd, md, ddMON, dMON), collapse = "|")

# Extract date (in any format) from r1 column
datesd <- str_extract(recap$r1, dates)
sort(unique(datesd)) # Most look ok (but need to check 4/16, 61/7, 7/63, 9/22)

sum(is.na(datesd)) # 14 entries where a date wasn't extracted
recap[is.na(datesd), c("band", "date_orig", "yr_recap", "r1", "comments")]
# 6 entries with foreign bands (I think date_orig represents first time captured by Fred)
# 8 entries with no recapture date (but some in different year than banding, so would like to keep in there)




