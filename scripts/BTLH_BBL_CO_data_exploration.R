# Exploring BTLH banding data for CO
# Data source: Bird Banding Laboratory
# Records from 1960 to 2023
# Gaby Samaniego
# gabysamaniego@arizona.edu
# 2023-08-24

library(tidyverse)
library(waldo) # compares two data sets

# Read data
# I downloaded two data sets:

# Request type: Banding data with related encounters (if any) request code 1180
band.dat.with.enco <- read.csv("data/BBL2023_Req_1180.csv",
                          na.strings = c("",NA),
                          strip.white = TRUE)

# Request type: Encounters data (related banding data are included) request code 1179
enco.dat <- read.csv("data/BBL2023_Req_1179.csv",
                          na.strings = c("",NA),
                          strip.white = TRUE)

# Compare the two data sets
compare(enco.dat, band.dat.with.enco)
# Old: enco.dat
# New: band.dat.with.econ

# The output of this comparison is not straight forward to understand. But, it uses
# colors to show the differences in the two data sets. In this case, it is showing in
# black the 1186 rows that are the same and in orange the rows that are different.
# This means all the data in enco.dat is included in the 33363 observations of 
# band.dat.with.enco 

# Explore banding data with related encounters (if any) 
colnames(band.dat.with.enco)

# band, original_band, others_bands are all obfuscated 

count(band.dat.with.enco, event_type)
# 32728 records are B for banding
# 635 records are E for encounter 

unique(band.dat.with.enco$iso_subdivision)
# There are six records that are not from CO, two for AZ, TX, and ID respectevely.
# They are encountered records (recaptures out of state)

count(band.dat.with.enco, coordinates_precision_code)
# coordinates_precision_code     n
#                          0 14981   Exact location
#                          1  7768   Centroid of 1 minute block
#                         10 10610   Centroid of 10 minute block
#                         11     4   approximate location based on centroid of town/place/area

count(band.dat.with.enco, band_type_code)
# band_type_code     n
#             11 33361   Aluminum/butt end
#             98     2   Banding Mortality (No band manufactured)

count(band.dat.with.enco, bird_status)
#  bird_status     n
#           3 33154   Normal wild bird
#           5     7   Sick, Exhausted, Over-stressed, Injured, or Physical Deformity
#           7     2   Rehabilitated and held
#           8    10   Held for longer than 24 hours for experimental or other purposes
#          NA   190   

count(band.dat.with.enco, extra_info_code)
#   extra_info_code     n
#               0   29946  Federal numbered metal band only
#               8    2527  Temporary markers: Paint or dye; other temporary markers on feathers (imping, tape on tail)
#               9      12  All flight feathers on one or both wings clipped or pulled upon release
#              18     163  Blood sample taken
#              25     100  Two or more types of auxiliary markers (neck collar and color leg band or wing tag and radio transmitter)
#              75     421  PIT tag
#              85       4  Miscellaneous (combination or situation not covered by other ai codes)
#              NA     190

count(band.dat.with.enco, age_code)
#   age_code     n
#        0     272  Unknown
#        1   26467  AHY
#        2    6409  HY
#        4       9  Local: A nestling or young bird incapable of sustained flight
#        5      12  Second year
#        6       4  After second year
#       NA     190

count(band.dat.with.enco, sex_code)
# sex_code     n  
#        0  1512  Unknown
#        4 12914  Male
#        5 18744  Female
#        7     3  Female, sex upon recapture
#       NA   190

count(band.dat.with.enco, permit)
#    permit     n
#  P0710540     1
#  P1713263    10
#  P1716506  2283
#  P1719717  2416
#  P2722447 12344  *
#  P2723703     1
#  P2724692  3154
#  P3723353  5701  *
#  P3723383     1
#  P3733333   493
#  P3735013  2326
#  P3737353   246
#  P4749687   810
#  P6956009     1
#  P7711636   125
#  P7715891   162
#  P7725945   192
#  P8980140    19
#  P9987129   305
#  P9988778   220
#  P9993769  2372
#      <NA>   181
# Prmits are also obfuscated

count(band.dat.with.enco, band_status_code)
# band_status_code     n
#                0 32630  Original band
#                1    96  Replacement band
#                2     1  Additional band
#                X     1  Banding mortality
#             <NA>   635

count(band.dat.with.enco, how_obtained_code)
#  how_obtained_code     n
#                  0     7  Found dead bird
#                 10     3  Banding Mortality: due to trap, holding device, or handling
#                 13     1  Caught due to striking: stationary object other than wires or towers
#                 21     5  Bird caught or found dead in building or enclosure.
#                 52     1  Saw or photographed federal band while bird was free
#                 53     2  Captured for Scientific Purposes (not collected)
#                 59     1  Caught uninjured bird by hand
#                 66   534  Previously banded bird trapped and released during banding operations
#                 97    80  Miscellaneous. Method of recovery not covered by other codes
#                 98     1  Found band or band number only (including metal detecting)
#                 NA 32728
 
count(band.dat.with.enco, record_source)
# record_source     n
#             B 32728  Banding
#             E   190  Encounter
#             R   445  Recapture

# Map

dat <- band.dat.with.enco %>% 
  group_by(permit) %>% 
  count(lat_dd) 








