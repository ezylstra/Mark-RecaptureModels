# Plot RMNP banding sites
# Gaby Samaniego
# gabysamaniego@arizona.edu
# 2024-01-04

# Load packages
library(terra)
library(tidyterra)
library(ggplot2)
library(ggspatial) # To add scale bar on map
library(sf)

# Clear environment
rm(list = ls())

# Import breeding range of BTLH from eBird
range <- vect("data/BTLH-range-map/brthum_range_2021.gpkg")
data.frame(range)
breeding <- subset(range, range$season == "breeding")

# Quick look at breeding range
plot(breeding, col = "gray")

# Import sampling locations
sites <- read.csv('data/RMNP-sites-data.csv')
locs <- vect(sites, geom = c('longitude', 'latitude'), crs = crs(breeding))

# Code to fix location of the label on map, from Erin on a different script 
sites <- sites %>%
  mutate(nudge_lat = latitude + 0.01,
         nudge_log = longitude + 0.02) 

ggplot() + 
  geom_spatvector(data = breeding, fill = "lightsalmon", color = NA) +
  borders("world", colour = 'black') +
  borders("state", colour = 'black') +
  geom_spatvector(data = locs) +
  coord_sf(expand = FALSE, xlim = c(-105.4, -106),  ylim = c(40.1, 40.6)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 9)) +
  labs(title = 'Broad-tailed hummingbird breeding distribution range',
       subtitle = 'including RMNP banding sites') +
  geom_text(data = sites,
            aes(x = nudge_log, 
                y = nudge_lat, # Moves the label, so it is not on top of the point 
                group = NULL,
                fill = NULL,
                label = site),
            color = "black",
            size = 3) +
  theme(plot.title = element_text(color = "black", size = 12, hjust = 0.5),
        plot.subtitle = element_text(color = "black", size = 12, hjust = 0.5)) +
  annotation_scale()

# Calculate distance between points 


