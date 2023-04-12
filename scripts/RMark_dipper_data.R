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

# Specify parameters for simple models created
Phi.dot <- list(formula = ~1)
p.dot <- list(formula = ~1)

# Create some more parameter specifications solely for demonstration purposes as some of
# these models may not make sense for the dipper data

Phi.time <- list(formula = ~time)
Phi.sex <- list(formula = ~sex)
Phi.sexplusage <- list(formula = ~sex + age)
p.time <- list(formula = ~time)
p.Time <- list(formula = ~Time)
p.Timeplussex <- list(formula = ~Time + sex)

# Run three last models. Examples of how to keep up with the names
dipper.phi.dot.p.dot <- mark(dipper.process,
                             dipper.ddl,
                             model.parameters = list(Phi = Phi.dot,
                                                     p = p.dot))

dipper.phi.time.p.dot <- mark(dipper.process,
                              dipper.ddl,
                              model.parameters = list(Phi = Phi.time,
                                                      p = p.dot))

dipper.phi.sex.p.dot <- mark(dipper.process,
                             dipper.ddl,
                             model.parameters = list(Phi = Phi.sex,
                                                     p = p.dot))

dipper.phi.sex.p.Timeplussex <- mark(dipper.process,
                                     dipper.ddl,
                                     model.parameters = list(Phi = Phi.sex,
                                                             p = p.Timeplussex))

dipper.phi.time.p.time <- mark(dipper.process,
                               dipper.ddl,
                               model.parameters = list(Phi = Phi.time,
                                                       p = p.time))

dipper.phi.sexplusage.p.dot <- mark(dipper.process,
                                    dipper.ddl,
                                    model.parameters = list(Phi = Phi.sexplusage,
                                                            p = p.dot))
# Look at the output of one of the models
summary(dipper.phi.sex.p.Timeplussex)

# look at the mark result object some more so you can see how to extract various 
# parts of the results. We see that the names of the elements are:
names(dipper.phi.sex.p.Timeplussex)

# model.parameters is simply the value of the mark argument with the same name
# parameters field is for internal use with various attributes set for each parameter

# The field output is the link to the input and output files
dipper.phi.sex.p.Timeplussex$output

# The design matrix for the simplified model structure is also contained in the 
# result as a matrix
dipper.phi.sex.p.Timeplussex$design.matrix

# The list element of most interest is results, a list containing extracted values 
# from the MARK output files:
names(dipper.phi.sex.p.Timeplussex$results)

# The definitions of the elements are as follows:
# lnl: −2 logL Likelihood value
# deviance: difference between null deviance and model deviance
# npar: Number of parameters (always the number of columns in design matrix)
# n: effective sample size
# AICc: Small sample corrected AIC using npar
# beta: data frame of beta parameters with estimate, standard error (se), lower confidence
# limit (lcl), and upper confidence limit (ucl)
# real: data frame of unique (simplified) real parameters with estimate, standard error
# (se), lower confidence limit (lcl), and upper confidence limit (ucl), and notation
# for fixed parameters
# beta.vcv: variance-covariance matrix for beta
# derived: data frame of derived parameters if any
# derived.vcv: variance-covariance matrix for derived parameters if any
# covariate.values: dataframe with fields Variable and Value which are the covariate
# names and value used for real parameter estimates in the MARK output
# singular: indices of beta parameters that are non-estimable or at a boundary

# The individual elements can be extracted using list notation. For example, the 
# data frame of the beta parameters:
dipper.phi.sex.p.Timeplussex$results$beta

# To view all of the real parameters with standard errors, use summary as follows 
summary(dipper.phi.sex.p.Timeplussex, se = T)

### Design covariates in RMArk

# Define a Flood variable that is 1 for flood periods and 0 otherwise

# Between sampling occasions 1981-1982 and 1982-1983 there were severe floods that 
# could have reduced survival in those periods and capture probability may have differed in 1982

# Remember that there are different design data for each parameter, so a Flood 
# field has to be defined for each parameter that will use the field in the model. 
# Because the timing of the effect varies for phi and p, the definitions of those 
# variables are different.
dipper.ddl$Phi$Flood <- 0
dipper.ddl$Phi$Flood[dipper.ddl$Phi$time == 1981 | dipper.ddl$Phi$time == 1982] <- 1
dipper.ddl$p$Flood <- 0
dipper.ddl$p$Flood[dipper.ddl$p$time == 1982] <- 1

# Once the data have been created they can be used in models as shown below
Phi.Flood <- list(formula = ~ Flood)
p.Flood <- list(formula = ~ Flood)

dipper.phi.flood.p.dot <- mark(dipper.process,
                               dipper.ddl,
                               model.parameters = list(Phi = Phi.Flood,
                                                       p = p.dot))
dipper.phi.flood.p.flood <- mark(dipper.process,
                                 dipper.ddl,
                                 model.parameters = list(Phi = Phi.Flood,
                                                         p = p.Flood))

# The function add.design.data. For example, if we want to create age intervals 
# for survival (young, sub-adult, and adult), we can do it as follows:
dipper.ddl <- add.design.data(dipper.process, 
                              dipper.ddl,
                              parameter = "Phi", 
                              type = "age", # either "age", "time" or "cohort"
                              bins = c(0,1,3,6), # bins for grouping
                              name = "ageclass") # name assigned to variable in design data

# We can see the assigned values in Phi
summary(dipper.ddl$Phi)

# In the definition of ageclass, a “(” means the interval is open on the left which 
# means that value is not included in the interval.Whereas a square bracket 
# (“[” or “]”) is for a closed interval which means the interval end point is included

# If we decided that the intervals should be shifted to the left, the easiest 
# way is as follows:
dipper.ddl <- add.design.data(dipper.process, 
                              dipper.ddl,
                              parameter = "Phi", 
                              type = "age",
                              bins = c(0,1,3,6),
                              name = "ageclass",
                              right = FALSE, # If TRUE, bin intervals are closed on the right
                              replace = TRUE) # if TRUE, replace any variable with same name as name

# Now ageclass defines the intervals 0,1 to 2, and 3+ for modeling age effects in Phi
summary(dipper.ddl$Phi$ageclass)

# In many situations the additional design data are simply covariates to be used 
# in place of occasion/-time effects. Examples are effort, weather, or observers 
# which vary for occasions and may be useful to simplify modeling of capture 
# probability rather than time-varying parameters. For this situation, the function 
# merge_design.covariates was created. 
# The following is an example in which fictitious effort data were created for 
# the dipper data:
df <- data.frame(time = c(1980:1986),
                 effort = c(10,5,2,8,1,2,3))

dipper.ddl$p <- merge_design.covariates(dipper.ddl$p, df)
summary(dipper.ddl$p$effort)

#So why is the maximum value for effort only 8 and not 10? For the CJS model there 
# is no capture probability for 1980, so the value is ignored. 

# The function is less forgiving if you forget to include data for one of the times
df2 <- data.frame(time = c(1980:1985),
                 effort = c(10,5,2,8,1,2))

dipper.ddl$p <- merge_design.covariates(dipper.ddl$p,df2)

# We get an error. Make sure to include all data

### Create another model that uses those new design data
Phi.ageclass.plus.sex <- list(formula = ~ ageclass + sex)
p.effort.plus.sex <- list(formula = ~ effort + sex)

dipper.phi.ageclassplussex.p.effortplussex <- mark(dipper.process,
                                                   dipper.ddl,
                                                   model.parameters = list(Phi = Phi.ageclass.plus.sex,
                                                                           p = p.effort.plus.sex))
### Comparing results from multiple models

# Use the function collect.models
dipper.cjs.results <- collect.models()

# It looked through all of the objects in the workspace and collected any object 
# that had a class of “mark'

dipper.cjs.results
names(dipper.cjs.results)

# Remove first models my example
dipper.cjs.results <- remove.mark(dipper.cjs.results, c(8,9))
dipper.cjs.results








