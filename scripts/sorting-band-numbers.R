# Sorting band numbers
# Gaby Samaniego

install.packages("tidyverse")
library(tidyverse)

# Set working directory
getwd()
setwd("C:/Users/gabym/Documents/R/HummingBird/data")

# Bring in data
data <- read.csv("2021-fh-alldata.csv")
head(data)

# Sort band numbers alphabetically 
data[order(data$Band.Number),] 

# Unique values should be band status 1, duplicate values should be band status R
duplicated(data$Band.Number)




