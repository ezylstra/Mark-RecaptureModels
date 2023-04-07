# Try RMark with dipper data
# Gaby Samaniego

library(RMark)

# Get dipper data included in package RMark
data(dipper)
summary(dipper)
head(dipper)

# Run first 'CJS' model Phi(.)p(.) in MARK notation and Phi(∼1)p(∼1) in R notation
# using default values for all arguments in function mark 
myexample <- mark(dipper, begin.time = 1990)

# The output
# The file mark001.inp is the file that would be equivalent to what you would see 
# if you used “Save Structure” rather than directly running the model in the MARK interface
# mark001.out is the text output file fromMARK with all the results
# mark001.res is the file of residuals (not currently used by RMark)
# mark001.vcv is a binary file containing the variance-covariance matrices and parameter
# estimates

myexample # Will show the output in same way that MARKc shows it

# An all-different PIM is the default PIM (parameter index matrix) type used in RMark
# See the PIM structure by using the PIMS function for Phi and p
PIMS(myexample, # mark model object
     'Phi', # parameter 
     simplified = FALSE) # shows full PIM structure, parameters from 1 to 42

PIMS(myexample, 'p', simplified = FALSE)

# Each of the 21 real parameters in Phi and another 21 real parameters in p are 
# given their own unique index, thus the term ‘all-different’

# For a CJS model without groups, the "design data” are occasion (time), age and 
# cohort-specific data.
# Separate design data are defined for each parameter to allow flexibility and
# differences in the way design data are handled for each parameter.
# Also, for labeling it is better to keep them separate since some parameters like 
# Phi represent an interval and others like p are for an occasion. 

# There are many different kinds of design data that can be created for any particular 
# example, but there are always several kinds of data that can be created automatically 
# by default. For this example, they are cohort, time and age

# To see the simplified and recoded PIMS for a model, use the PIMS function but 
# this time using the default value of simplified = TRUE. In myexample see that the
# 42 parameters have been recoded to the 2 unique parameters

PIMS(myexample,
     'Phi', 
     simplified = TRUE) # shows PIM structure, parameters recoded 1 

PIMS(myexample, 'p', simplified = TRUE) # parameter recoded 2

#### The function mark

# FIRST
# Use process.data

# Use the dipper data and the field sex to create 2 groups in the data and define
# fictitious beginning time and time intervals for the data
dipper.process <- process.data(dipper,   # use ?process.data 
                               model = 'CJS',
                               begin.time = 1980,
                               groups = 'sex')
# The resulting object (dipper.process) is a list containing the data and its attributes

# SECOND
# Use make.design.data

# Create the design data and PIM structure which depends on the selected type of
# analysis model (CJS), number of occasions, grouping variables and other attributes of
# the data that were defined in the process.data function 

# Naming convention is to use ddl (design data list) as the suffix and the data 
# name as the prefix
dipper.ddl <- make.design.data(dipper.process)

# Run a model with the processed data and the ddl data
myexample2 <- mark(dipper.process,dipper.ddl)

# Look at the non-simplified PIMS for ! and compare them to the design data that were
# created
PIMS(myexample2,
     'Phi',
     simplified = FALSE)

# Examine the design data for Phi
dipper.ddl$Phi







