# Download NDVI data from MODIS
# Gaby Samaniego gabysamaniego@arizona.edu
# 2023-02-20

# Code from tutorial https://rspatial.org/modis/2-download.html
# Edited to download NDVI for one site on BTLH's wintering grounds 

# Libraries and packages needed to work with MODIS data
library(terra) # To explore and visualize MODIS data
library(luna)  # To download MODIS data

# Code to install 'luna' from: https://github.com/rspatial/luna/
# install.packages('luna', repos='https://rspatial.r-universe.dev')

# Libraries needed to work with maps ##### for later
library(rgdal)
library(broom)
library(maps)
library(mapproj) # Needed for coord_map function in Erin's code

# ------------------ SKIPT THIS PART ---------------------#
### Code used to learn about MODIS products ###

# Lists all products that are currently searchable
prod <- getProducts()
head(prod)

# Output
#provider             concept_id                          short_name version
#1        GHRC             C1000-GHRC                            dc8capac       1
#5       CDDIS      C1000000000-CDDIS              CDDIS_DORIS_data_cycle       1
#18 LANCEAMSR2 C1000000000-LANCEAMSR2                      A2_RainOcn_NRT       0
#19  NSIDC_ECS  C1000000000-NSIDC_ECS                            NmHRIR3H       1
#23  ORNL_DAAC  C1000000000-ORNL_DAAC GLOBAL_MICROBIAL_BIOMASS_C_N_P_1264       1
#25      SEDAC      C1000000000-SEDAC               CIESIN_SEDAC_EPI_2012 2012.00

# To find the MODIS products
modis <- getProducts("^MOD|^MYD|^MCD")
modis

# Get product
# Product name from https://modis.gsfc.nasa.gov/data/dataprod/mod13.php
product.NDVI <- "MOD13Q1"

# Launch website to learn about the product
productInfo(product.NDVI)

# ---------------- RUN THIS PART ------------------- #

# Get product
product.NDVI <- "MOD13Q1"

# Define start and end dates 
# Start small! One week 
start <- "2022-01-01"
end <- "2022-01-07"

# Define area of interest
site <- c(-103.68,-103.66,19.60,19.63)

# Available data at MODIS for the area of interest
available <- luna::getModis(product.NDVI, 
                            start, 
                            end, 
                            aoi = site, 
                            download = FALSE)
available 

# Download data
test.data <- luna::getModis(product.NDVI, 
                            start, 
                            end, 
                            aoi = site, 
                            download = TRUE,
                            path = ("C:/Users/gabym/Documents/R/HummingBird/data/modis/colima"),
                            username = 'gaby.samaniego', 
                            password = 'Modis2022')
test.data

# Read the data
datadir <- file.path("C:/Users/gabym/Documents/R/HummingBird/data/modis")
test <- file.path(datadir, "MOD13Q1.A2021353.h08v07.006.2022004234729.hdf")

# Create a SpatRaster object from the file downloaded
r <- rast(test[1])  # why [1]?
r

# Output
# class       : SpatRaster 
# dimensions  : 4800, 4800, 12  (nrow, ncol, nlyr)
# resolution  : 231.6564, 231.6564  (x, y)
# extent      : -11119505, -10007555, 1111951, 2223901  (xmin, xmax, ymin, ymax)
# coord. ref. : +proj=sinu +lon_0=0 +x_0=0 +y_0=0 +R=6371007.181 +units=m +no_defs 
# sources     : MOD13Q1.A2021353.h08v07.006.2022004234729.hdf:MODIS_Grid_16DAY_250m_500m_VI:250m 16 days NDVI  
# MOD13Q1.A2021353.h08v07.006.2022004234729.hdf:MODIS_Grid_16DAY_250m_500m_VI:250m 16 days EVI  
# MOD13Q1.A2021353.h08v07.006.2022004234729.hdf:MODIS_Grid_16DAY_250m_500m_VI:250m 16 days VI Quality  
# ... and 9 more source(s)
# varnames    : MOD13Q1.A2021353.h08v07.006.2022004234729 
# MOD13Q1.A2021353.h08v07.006.2022004234729 
# MOD13Q1.A2021353.h08v07.006.2022004234729 
# ...
# names       : "250m~NDVI", "250m~ EVI", "250m~lity", "250m~ance", "250m~ance", "250m~ance", ... 

# Explore image properties 
crs(r)  # Get coordinate reference system CRS
dim(r)
nrow(r)
ncol(r)
nlyr(r)  # Number of layers
ncell(r)
res(r) # Resolution
names(r) # What bands we have

# Plot the data

# Create an image RGB composite plot
# r = x, SpatRaster 
plotRGB(r,  # plotRGB: makes a Red-Green-Blue plot based on three layers in a SpatRaster
        r = 1,  # integer. Index of the Red channel, between 1 and nlyr(r) 
        g = 4,  # integer. Index of the Green channel, between 1 and nlyr(r) 
        b = 3,  # integer. Index of the Blue channel, between 1 and nlyr(r)
        stretch = "lin") # character. Option to stretch the values to increase contrast: "lin" (linear) or "hist" (histogram)

# Specify a matrix (“qabits”) with the start and end of the quality assessment (QA) 
# bits considered, and specify a list (“reject”) with the values to be rejected
# Use table with bits values 

from <- c(1,3,11,14) # Defines the bits form table 
# From: 1 cloud state, 3 cloud shadow, 11 cloud flag, 14 adjacent to cloud   
to   <- c(2,3,11,14)
reject <- c("01,10", "1", "1", "1")
# reject: "01,10" is cloudy and mixed for bit 1-2, "1" is yes for bits 3, 11, 14 
qa_bits <- cbind(from, to, reject)
qa_bits # matrix containing QA information 

# Generate the quality mask
# Use band 12 sur_refl_state_500m that has the quality data
library(terra)
qc <- r[[12]]
plot(qc, main = "Quality")

# The luna package has a modis_mask method to create a mask from the quality band 
# and the parameters defined above (matrix) 
library(luna)
quality_mask <- modis_mask(qc,  # SpatRaster
                           16,  # single interger: bits, 16 or 32. Is 16 the # days?
                           qa_bits) # qmart: the tree column matrix with start, end and reject bits
plot(quality_mask, main="Quality mask")

# The plot shows the pixels we want to retain. Now that we have the quality mask, 
# we can apply it to all the bands
# mask: if x is a SpatRaster (which it is), it creates a new SpatRaster with the same values
# as x, except for the cells that are NA in another SpatRaster (the 'mask', here: quality_mask)
# or the cells that are not covered by the SpatVector. These cells become NA
rmask <- mask(r, quality_mask)

# Plot the results, here as a “false color composite” (NIR:Red:Green)
plotRGB(rmask, 
        r = 2, # Why the number?
        g = 1, # Same
        b = 4, # Same
        main = "False color composite", 
        stretch ="lin")

# Next step, I could get NDVI for the specific polygon of interest, for now I will
# get it for the entire tile

# Use the processed image to compute an index measure
i <- clamp(rmask, 0, 1) # Clamp (fijar) values to a minimum and maximum value

# Compute NDVI with formula
ndvi <- (i[[2]] - i[[1]]) /(i[[2]] + i[[1]])
plot(ndvi, main="NDVI")




