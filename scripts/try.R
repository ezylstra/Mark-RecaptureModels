library(sf)
library(tmap)
library(dplyr)

data(World) # free dataset that will load
countries = World %>%
  filter(name == "Afghanistan" | 
           name == "Turkmenistan" |
           name == "Iran") #Picking three countries in the world

pnts = data.frame(name=c("Point1","Point2","Point3","Point4"),latitude = c(34,35,36,37),longitude=c(63,65,62.5,66.5)) #Picking random points

sp_points = st_as_sf(pnts,coords = c('longitude',"latitude"))#make points spatial
st_crs(sp_points)= 4326 # Give the points a coordinate reference system (CRS)
sp_points=st_transform(sp_points,crs = st_crs(countries)) # Match the point and polygon CRS

tm_shape(countries)+
  tm_borders() +
  tm_text('name')+
  tm_shape(sp_points)+
  tm_dots(col='red',size=1.2)+
  tm_text('name',ymod = 1) #Creates the map below