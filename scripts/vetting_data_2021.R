# Vetting HMN's banding data 
# Gaby Samaniego gabysamaniego@email.arizona.edu 
# 2021-11-15 

library(tidyverse)

# Set working directory
setwd("C:/Users/gabym/Documents/R/HummingBird/data")

# Bring in data
data <- read.csv("HC_SWRS1_HMNBandingData_2021_GS.csv")
head(data)

example <- read.csv("example.csv") # It has lower case in some columns 
head(example)

# Change all characters in the database to uppercase
uppercase_data <- data.frame(lapply(data, function(variables) { # from the internet... 
                                if (is.character(variables)) {
                                  return(toupper(variables))
                                } else {
                                  return(variables)}
                              }),
                       stringsAsFactors = FALSE) 
head(uppercase_data) # it worked! 


uppercase_data_2 <-mutate_all(example, .funs=toupper) # all allows you to perform 
                                            # an operation on all variables at once 
                                            # from Kira's script, also worked  
head(uppercase_data_2)

# Find duplicates in Band_number 





