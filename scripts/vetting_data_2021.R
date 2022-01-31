# Vetting HMN's banding data 
# Gaby Samaniego gabysamaniego@email.arizona.edu 
# 2021-11-15 

install.packages("qdap")

library(tidyverse) # to do everything
library(qdap)      # to remove extra spaces 
library(stringr)   # to work with character vector

# Set working directory
setwd("C:/Users/gabym/Documents/R/HummingBird/data")

# Bring in data
example <- read.csv("example.csv") # It has lower case in some columns and other
head(example)                      # errors to try the code

# Change all characters in the database to uppercase

# First try:
uppercase_data <- data.frame(lapply(example, function(variables) { # from the internet 
                                if (is.character(variables)) {     # notes in notebook
                                  return(toupper(variables))  
                                } else {
                                  return(variables)}
                              }),
                       stringsAsFactors = FALSE) 

head(uppercase_data) # it worked! 

# Second try:
uppercase_data_2 <- mutate_all(example, .funs=toupper) # from Kira's script  
head(uppercase_data_2) # also worked!                 # Notes in notebook

# Remove all leading and trailing white spaces
banding$Initials.Bdr <- str_trim(banding$Initials.Bdr, side=c("both")) # from Kira's script

remove_spaces <- mutate_all(example, .funs=str_trim(example, side = "both"))

remove_spaces_2 <- data.frame(lapply(example, function(varaibles){
                              if (str_trim(example, side = "both")) {
                              } else {
                                return(varaibles)}
                              }),
                              stringsAsFactors = FALSE)


   # can I use mutate_all for this purpose? I tried and failed :( 

# Check each column to find mistakes  
unique(example$Initials.Bdr)
unique(example$Location)
unique(example$year)
unique(example$mo)
unique(example$day)
unique(example$Band.Status)

   # This is going to take forever... is there a better way to do it??? 


# Check the sequence of the band numbers 

# Create csv with clean data for each banding session 


# Find duplicates in Band_number 


# Separate combined columns 





