# Download MODIS-provided NDVI data
# Jeff Oliver
# jcoliver@arizona.edu
# 2023-06-14

library(MODISTools) # Retrieval of NDVI data
library(dplyr)      # General data wrangling

# Note: Dang but retrieval with MODISTools is slow. Likely a server issue; 
# early attempts at directly using the web service were relatively 
# unsuccessful, due in large part to limitation to number of tiles per query.
# Should there be interest, there may still be ways to use the web service 
# directly (like with httr or RCurl). See examples at 
# https://github.com/ornldaac/modis_restservice_qc_filter_R/blob/master/modis_restservice_qc_filter_R.ipynb

# For hummingbird monitoring network sites
sites <- read.csv(file = "data/sites-for-NDVI.csv")

# Product and band for NDVI
ndvi_product <- "MOD13Q1"
ndvi_band <- "250m_16_days_NDVI"

# MODISTools needs lat and lon
# We also bound the search by the beginning of the first active year (Jan 1) 
# and the end of the last active year (Dec 31). We leave it as an exercise to 
# the reader to filter out dates that aren't needed (i.e. those skipped years)
modis_sites <- sites %>%
  select(Location_code, LatDD, LongDD, First.active.year, Last.active.year) %>%
  rename(site_name = Location_code,
         lat = LatDD,
         lon = LongDD) %>%
  mutate(sdate = paste0(First.active.year, "-01-01"),
         edate = paste0(Last.active.year, "-12-31")) %>%
  select(-c(First.active.year, Last.active.year))

# Loop to run query for each row in sites data frame. Yes, we hate loops, but 
# they aren't the time suck here. Querying via MODISTools is glacial.

# For development, just use the first two rows of data
# query_sites <- modis_sites[1:2, ]

# What happens with Mexico & Canada? Apparently they work fine!
# query_sites <- modis_sites[c(8, 19), ]

# The full data set
query_sites <- modis_sites
ndvi_list <- vector(mode = "list", length = nrow(query_sites))
for (site_i in 1:nrow(query_sites)) {
  site_name <- query_sites$site_name[site_i]
  message("Running query for site ", site_name, 
          " (", site_i, " of ", nrow(query_sites), ")")
  # Run the query
  ndvi_query <- mt_subset(product = ndvi_product,
                          band = ndvi_band,
                          lat = query_sites$lat[site_i],
                          lon = query_sites$lon[site_i],
                          start = query_sites$sdate[site_i],
                          end = query_sites$edate[site_i])
  # Extract date and re-scale NDVI scores
  # scale column comes back as numeric, so wrap in as.numeric
  result <- ndvi_query %>%
    mutate(ndvi = as.numeric(scale) * value) %>%
    mutate(site_name = site_name) %>%
    select(site_name, calendar_date, ndvi)
  rownames(result) <- NULL
  ndvi_list[[site_i]] <- result
  # Should be unnecessary, but for some reason paranoid about memory today?
  rm(ndvi_query, result)
  gc()
}

# Bundle all the results back together
all_ndvi <- ndvi_list %>%
  bind_rows()
rownames(all_ndvi) <- NULL

# Write the results to disk
write.csv(file = "data/NDVI-data.csv",
          x = all_ndvi,
          row.names = FALSE)

################################################################################
# Initial testing below here

# Some investigation of the data products available via MODISTools
# products <- MODISTools::mt_products()
# Find the rows with "NDVI" in the description
# ndvi <- grep(pattern = "NDVI", ignore.case = TRUE, x = products$description)
# Print those NDVI-containing rows
# products[ndvi, ]
# MOD13Q1: MODIS/Terra Vegetation Indices (NDVI/EVI) 16-Day L3 Global 250m SIN Grid
ndvi_product <- "MOD13Q1"
# Look at the available bands for the product of interest
# mt_bands(product = ndvi_product)
ndvi_band <- "250m_16_days_NDVI"

# Test with one location, roughly three years of data
pallisades <- c(lat = 32.410253, lon = -110.714690)
pall_ndvi <- mt_subset(product = ndvi_product,
                       lat = pallisades["lat"],
                       lon = pallisades["lon"],
                       band = ndvi_band,
                       start = "2019-06-01",
                       end = "2020-06-04")
# Things get stored as integers, so need to multiply by value in scale column 
# to get actual (unscaled) NDVI value
pall_ndvi$ndvi <- as.numeric(pall_ndvi$scale) * pall_ndvi$value

