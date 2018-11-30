##################################################################
##  Master file: This is the masterfile outlining the steps   ####
##  in the RCI analysis                                       ####
##  Start: 3/2/2017                                           ####
##################################################################

##  The R packages required and user functions are in this file
source('RCI functions.R')


# Step 1: Creating a derived data for linkage using LSOA01 ----------------


#############
##  Step two: Bayesian inference.---- 
##  We need to get the predicted outcomes from the Bayesian models for the 3 DWP stats and save them. This is because they are used for all calculation of RCI so we are better off saving time this way
source('Bayesian models/Running and saving bayes model LA.R') #Runs the models for the LA
source('Bayesian models/Running and saving bayes model TTWA.R') #Runs the models for the TTWAs (excl. greater london area)
source('Bayesian models/Running and saving bayes model London.R') #Runs models for the greater London area

##  Step three: Calculating the RCI. We do this by boundaries--LAs and TTWA.
##  Despite it's name the RCI files also contain the DI calculations
##  RCI
source('Analysis code/RCI analysis LA.R') #This is self explanatory; cover standard RCI and by accessibility
source('Analysis code/RCI analysis TTWA.R') #by normal rci, general and access
source('Analysis code/RCI analysis London.R') #by normal and access

##  ACI (optional addition in paper)
source('Analysis code/ACI analysis LA.R') 
source('Analysis code/ACI analysis TTWA.R') 
source('Analysis code/ACI analysis London.R') 

##  Step four: Sensivity analysis
##  Rboustness test one: looking at the results using only the unchanged zones
source('Sensitivity analysis/Robustness test 1 omitting changed LSOAs LA.R')
source('Sensitivity analysis/Robustness test 1 omitting changed LSOAs TTWA.R')
source('Sensitivity analysis/Robustness test 1 omitting changed LSOAs London.R')

##  Robustness test two: the results by expanding and contracting the LA borders


##  Step five: Graphing and tabling function (subject to change)
source('Graphs and tables/Tabling function.R') # This contains a ready made function for formatting tables 
source('Graphs and tables/Paper tables.R') #as described
source('Graphs and tables/Paper tables RCI.R') #another possible (duplicated) code
source('Graphs and tables/Paper tables DI.R')
source('Graphs and tables/Graphing ACI change.R')

### These are the analysis files: