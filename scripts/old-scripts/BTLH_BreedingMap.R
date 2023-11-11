# Map for Western Hummingbird Partnership grant proposal
# Original script by Erin Zylstra
# Updated by Gaby Samaniego
# 2023-25-06

library(terra)
library(tidyterra)
library(ggplot2)

# Import breeding range of BTLH from eBird
range <- vect("data/Broad-tailed range map/brthum_range_2021.gpkg")
data.frame(range)
breeding <- subset(range, range$season == "breeding")

# Quick look at breeding range
plot(breeding, col = "gray")

# Import sampling locations
sites <- read.csv('data/BTLH_sites.csv')
sites4 <- filter(sites, Location %in% c('ML', 'PCBNM', 'DGS', 'WCAT'))
locs <- vect(sites4, geom = c('Longitude', 'Latitude'), crs = crs(breeding))

# Code to fix location of the label on map, from Erin on a different script 
sites4 <- sites4 %>%
  mutate(nudge_lat = Latitude + 0.5,
         nudge_log = Longitude + 0.3) 

ggplot() + 
  geom_spatvector(data = breeding, fill = "lightsalmon", color = NA) +
  borders("world", colour = 'black') +
  borders("state", colour = 'black') +
  geom_spatvector(data = locs) +
  coord_sf(expand = FALSE, xlim = c(-100, -120),  ylim = c(30, 42.5)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 9)) +
  labs(title = 'Broad-tailed hummingbird breeding distribution range',
       subtitle = 'including HMN banding sites') +
  geom_text(data = sites4,
            aes(x = nudge_log, 
                y = nudge_lat, # Moves the label, so it is not on top of the point 
                group = NULL,
                fill = NULL,
                label = Location),
            color = "black",
            size = 3) +
  theme(plot.title = element_text(color = "black", size = 12, hjust = 0.5),
        plot.subtitle = element_text(color = "black", size = 12, hjust = 0.5))

