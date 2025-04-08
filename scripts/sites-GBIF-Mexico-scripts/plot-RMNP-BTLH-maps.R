# Plot RMNP banding sites and BTLH wintering sightings in Mexico
# Gaby Samaniego
# gabysamaniego@arizona.edu
# 2025-03-23

# Load packages
library(tidyverse)
library(sf)

# Clear environment
rm(list = ls())

# ------------------------------- LOAD DATA ---------------------------------- #

# Load BTLH range map (source eBird) 
range <- st_read('data/sites-BTLH-range-map/brthum_range_2021.gpkg')

# Subset breeding and non breeding ranges 
breeding <- range %>% 
  filter(season == 'breeding')

nonbreeding <- range %>% 
  filter(season == 'nonbreeding')

# Read RMNP banding sites and exclude sites that won't be part of analysis  
summer.sites <- read.csv('data/sites-BTLH-range-map/thinned-summer-sites.csv') %>% 
  filter(!site %in% c('WB2','WB1', 'WPK1', 'NFPC', 'POLC', 'SHIP'))

# Convert data frame to sf object and add column useful to plot the data
summer.sf <- st_as_sf(summer.sites, 
                      coords = c('longitude', 'latitude'), 
                      crs = st_crs(breeding)) %>%
  mutate(point_type = 'BTLH banding sites')

# Read Mexico's sites, convert to sf and add column useful to plot the data
winter.sites <- read.csv('data/sites-BTLH-range-map/thinned-winter-sites.csv')
winter.sf <- st_as_sf(winter.sites, 
                      coords = c('longitude', 'latitude'), 
                      crs = st_crs(nonbreeding)) %>%
  mutate(point_type = 'BTLH sightings')

# Combine summer and winter sites
points.data <- bind_rows(summer.sf, winter.sf)

# ---------------------- PLOT SUMMER AND WINTERING GROUNDS ------------------- # 
# ---------------- INCLUDING BANDING SITES AND MEXICO SIGHTINGS -------------- #

# Create dummy data to force fill legend in the map
legend.fill <- data.frame(x = c(0, 0),
                          y = c(0, 0),
                          range_type = c('Breeding', 'Nonbreeding'))

# Create map
main.map <- ggplot() + 

  # Plot breeding and non breeding range 
  geom_sf(data = breeding, 
          fill = 'salmon', 
          color = NA) +
  geom_sf(data = nonbreeding, 
          fill = 'cadetblue', 
          color = NA) +
  
  # Plot borders
  borders('world', colour = 'gray20') +
  borders('state', colour = 'gray20') +
  
  # Plot point data
  geom_sf(data = points.data, 
          aes(shape = point_type), 
          size = 2, 
          color = 'gray18') +
  
  # Create invisible dummy tiles to create fill legend
  geom_tile(data = legend.fill, 
            aes(x = x, y = y, fill = range_type),
            alpha = 0) +
  
  # Manual fill colors for breeding and non breeding
  scale_fill_manual(values = c('Breeding' = 'salmon', 
                               'Nonbreeding' = 'cadetblue'),
                    name = 'Range',
                    labels = c('Breeding' = 'Summer Grounds', 
                               'Nonbreeding' = 'Wintering Grounds')) +
  
  # Define point's shapes
  scale_shape_manual(values = c('BTLH banding sites' = 16,  # Circle
                                'BTLH sightings' = 17),     # Triangle
                     name = 'BTLH Data') +
  
  # Define map extent
  coord_sf(expand = FALSE, xlim = c(-118, -89),  ylim = c(13, 42.5)) +
  
  # Edit map's theme
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 9),
        legend.key.size = unit(0.4, 'cm'),
        legend.background = element_rect(fill = 'white', color = 'white'),
        legend.box = "vertical",        # Stack legends vertically
        legend.position = c(0.2, 0.2)) +  # Adjust location inside plot
  
  # Customize legend appearance and order
  guides(fill = guide_legend(order = 1, 
                             override.aes = list(alpha = 1, size = 4)),
         shape = guide_legend(order = 2))

# Export map
ggsave('output/GBIF-Mexico-data/study-sites-RMNP-Mexico.pdf', 
       plot = main.map, width = 8, height = 6)

# --------------------------- PLOT RMNP BANDING SITES ------------------------ # 
# ------------ INCLUDING PARK'S BOUNDARY AND CONTINENTAL DIVIDE -------------- #

# Add coordinates to sf object and nudge y and x to adjust site's labels in plot
summer.sf <- summer.sf %>%
  mutate(x = st_coordinates(summer.sf)[, 1], # retrieve coordinates from geometry
         y = st_coordinates(summer.sf)[, 2],
         nudge_y = y + 0.01,
         nudge_x = x + 0.02)

# Read shape files needed for the map:
# RMNP boundary
rmnp.sf <- st_read('data/sites-BTLH-range-map/RMNP-boundary/Boundary_(Line).shp')
# Continental divide
con.divide <- st_read('data/sites-BTLH-range-map/continental-divide/condivl020.shp')

RMNP.map <- ggplot() + 
  
  # Plot banding siteS
  geom_sf(data = summer.sf, 
          size = 2, 
          color = 'gray18') +
  
  # Add site's names labels
  geom_text(data = summer.sf,
            aes(x = nudge_x, y = nudge_y, label = site),
            color = 'black',
            size = 3) +
  
  # Plot RMNP boundary
  geom_sf(data = rmnp.sf, 
          aes(color = 'Rocky Mountain National Park'), 
          fill = NA, 
          size = 1, 
          linetype = 'solid') +
  
  # Plot continental divide
  geom_sf(data = con.divide, 
          aes(color = 'Continental Divide'), 
          fill = NA, 
          size = 1, 
          linetype = 'dashed') +
  
  # Define map extent
  coord_sf(expand = FALSE, xlim = c(-106, -105.4), ylim = c(40.1, 40.6)) +
  
  # Customize color legend for lines
  scale_color_manual(name = "", 
                     values = c('Rocky Mountain National Park' = 'grey18',
                                'Continental Divide' = 'darkorange')) +
  
  # Edit map's theme
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 9),
        legend.key.size = unit(0.4, 'cm'),
        legend.background = element_rect(fill = 'white', color = 'white'),
        legend.box = "vertical",        # Stack legends vertically
        legend.position = c(0.2, 0.08)) 

# Export map
ggsave('output/GBIF-Mexico-data/banding-sites-RMNP.pdf', 
       plot = RMNP.map, width = 8, height = 6.5)
