# Vetting HMN's banding data 
# Gaby Samaniego gabysamaniego@email.arizona.edu 
# 2021-11-15 

install.packages("qdap")

library(tidyverse) # for everything
library(qdap)      # to remove extra spaces 

# Set working directory
setwd("C:/Users/gabym/Documents/R/HummingBird/data")

# Bring in data
example <- read.csv("example.csv") # It has lower case in some columns and other
head(example)                      # errors to try the code

# Change all characters in the database to uppercase

uppercase_data <- data.frame(lapply(example, function(variables) { # from the internet 
                                if (is.character(variables)) {     # notes in notebook
                                  return(toupper(variables))  
                                } else {
                                  return(variables)}
                              }),
                       stringsAsFactors = FALSE) 

head(uppercase_data) # it worked! 


head(example)
uppercase_data_2 <-mutate_all(example, .funs=toupper) # from Kira's script  
head(uppercase_data_2) # also worked!                 # Notes in notebook

# Remove multiple spaces in rows 
trimws(example,"both")
head(example)


# Check each column to find mistakes



# Check the sequence of the band numbers 

# Create csv with clean data for each banding session 


# Find duplicates in Band_number 


# Separate combined columns 





