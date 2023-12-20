# Test for combining band, reband data
# (first running most of clean-RMNP-banded-data.R, clean-RMNP-recaptured-data.R)

head(banded.dat, 2)
head(recaptured.dat, 2)

# Simplify datasets
datb <- banded.dat %>% 
  select(band_number, date, site, species, sex, age, year, day, month) %>%
  mutate(band_status = 1) %>% # 1 for all new bands applied
  filter(species == 'BTLH') %>%
  filter(year < 2012) # Since we don't have 2012 recapture data
datr <- recaptured.dat %>%
  select(band_number, original_site, species, original_sex, recapture_year) %>%
  filter(species == "BTLH") %>%
  distinct(band_number, recapture_year, .keep_all = TRUE) # Make sure there are no duplicates

# Check that bands in recapture dataset appear in banding dataset, and for now
# remove any bands that don't appear in recapture dataset. NOTE: may need
# to also explore whether birds moved between sites if the sites are not in
# close proximity and check that sex/age assignments were consistent.
bandr <- sort(unique(datr$band_number))
bandr[!bandr %in% datb$band_number]
datr <- filter(datr, band_number %in% datb$band_number)

# Combine band_number and year from both datasets
datb <- datb %>%
  select(band_number, date, site, sex, age, year, band_status)
datr <- datr %>%
  select(-species) %>%
  rename(site = original_site,
         sex = original_sex,
         year = recapture_year) %>%
  mutate(date = NA, .after = "band_number") %>%
  mutate(age = NA, .after = "sex") %>%
  mutate(band_status = 2)
dat <- rbind(datb, datr) %>%
  arrange(band_number, year)

# Few checks:
bandyr <- dat %>%
  group_by(band_number, year) %>%
  summarize(ncaps = length(year), .groups = "keep") %>%
  data.frame()
head(filter(bandyr, ncaps > 1))
filter(dat, band_number == "3100-41414")
# Lots of bands have an entry in the recapture file for the year that were first
# captured. Need to remove them.

bands <- dat %>%
  group_by(band_number) %>%
  summarize(cap_year = min(year)) %>%
  data.frame()

dat <- left_join(dat, bands, by = "band_number") %>%
  filter(!(year == cap_year & band_status == 2))

bands <- dat %>%
  group_by(band_number) %>%
  summarize(cap_year = cap_year[1],
            n_status_1 = sum(band_status == 1),
            n_yrs = length(year),
            n_recaps = sum(band_status == 2)) %>%
  data.frame()
count(bands, n_yrs, n_recaps)

datw <- pivot_wider(data = select(dat, -c(date, site, sex, age)),
                    names_from = year,
                    names_prefix = "y",
                    names_sort = TRUE,
                    values_from = band_status,
                    values_fill = 0) %>%
  mutate(across(y2003:y2011, ~ replace(., . == 2, 1))) %>%
  unite("ch", y2003:y2011, sep = "", remove = FALSE) %>%
  data.frame()
# Note: would be good to keep site, sex, age in there, but need values for each
# bird to be consistent over years. For now will just exclude those columns.
