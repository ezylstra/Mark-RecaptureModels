# Vetting HMN's 2021 banding data 
# Gaby Samaniego gabysamaniego@email.arizona.edu 
# 2021-11-15 

library(tidyverse)

# Set working directory
setwd("C:/Users/My Elite/Documents/R/HummingBird/data")

# Bring in data
data <- read.csv("HC_SWRS1_HMNBandingData_2021_GS.csv")
head(data)

example <- read.csv("example.csv")
head(example)

# Change all characters to uppercase
uppercase_data <- data.frame(lapply(data, function(variables) {
                                if (is.character(variables)) {
                                  return(toupper(variables))
                                } else {
                                  return(variables)}
                              }),
                       stringsAsFactors = FALSE) 
head(uppercase_data)


uppercase_data_2 <-mutate_all(example, .funs=toupper) # all allows you to perform 
                                            # an operation on all variables at once 
head(uppercase_data_2)
