# Run clean-RMNP-banded-data.R through line 456
# Then save a banded dataset that includes date (and not just year)

# Create a new data frame with selected columns we'll need for analysis
bandeddate <- banded.dat %>% 
  select(UBI_band, band_status, year, date, species, age, fixed_sex, site) %>% 
  filter(species == 'BTLH') %>% 
  select(-species)

# Export csv of new data frame
write.csv(bandeddate, 'output/cleaned-banded-data-RMNP-date.csv',
          row.names = FALSE)
