# Vetting HMN's banding data 
# Gaby Samaniego gabysamaniego@email.arizona.edu 
# 2021-11-15 

install.packages("qdap")
install.packages("pointblank")

library(tidyverse) # to do everything
library(qdap)      # to remove extra spaces 
library(stringr)   # to work with character vector
library(pointblank) # for data validation 

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

# Check each column to find mistakes  
unique(example$Initials.Bdr)
unique(example$Location)
unique(example$year)
unique(example$mo)
unique(example$day)
unique(example$Band.Status)

# Remove all leading and trailing white spaces
banding$Initials.Bdr <- str_trim(banding$Initials.Bdr, side=c("both")) # from Kira's script

remove_spaces <- mutate_all(example, .funs=str_trim, side = "both")
head(remove_spaces)
unique(remove_spaces$Location)

remove_spaces_2 <- data.frame(lapply(example, function(varaibles){
                              if (str_trim(example, side = "both")) {
                              } else {
                                return(varaibles)}
                              }),
                              stringsAsFactors = FALSE)

# Try the package pointblank for data validation. It works and it is AWESOME!!!

agent <- 
  create_agent(
    tbl = example,
    tbl_name = "example",
    label = "VALID-I Example No. 1") %>%
col_vals_in_set(vars(Location), set = c("hc","swrs1")) %>%
col_vals_in_set(vars(Initials.Bdr), set =c("smw","ml","gs"))


interrogate(agent)


# Mount Lemmon site (ML)

ML_data <- read.csv("ML_data.csv")
head(ML_data)
colnames(ML_data)

ML <- 
  create_agent(
    tbl = ML_data,
    tbl_name = "ML_data",
    label = "ML Data Validation") %>% 
  col_vals_equal(vars(Location), value = "ML") %>% 
  col_vals_equal(vars(Initials.Bdr), value = "GS") %>% 
  col_vals_in_set(vars(Band.Status), set = c("1","R","F","4","5","6","8")) %>% 
  col_vals_in_set(vars(Band.Size), set = c("B","C","D","E","F","G","H","I","J",
                                           "K","L","M","N","O")) %>% 
  col_vals_in_set(vars(Species), set = c("ANHU","ALHU","BBLH","BCHU","BADE",
                                         "BEHU","BTMG","BTLH","BUFH","BALO",
                                         "CAHU","COHU","LUHU","RIHU","HYHU",
                                         "RTHU","RUHU","VCHU","WEHU","UNHU")) %>% 
  col_vals_in_set(vars(Sex), set = c("M","F","U")) %>% 
  col_vals_in_set(vars(Age), set = c("0","1","2","5")) %>% 
  col_vals_in_set(vars(GorColor), set = c("O","R","V","P","B","G","GP","NS",
                                          "LS","MS","HS")) %>% 
  col_vals_between(vars(GorCount....),1, 99, na_pass = TRUE) %>% 

  

interrogate(ML)

# Check the sequence of the band numbers 

# Create csv with clean data for each banding session 


# Find duplicates in Band_number 


# Separate combined columns 





