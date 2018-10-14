###########################################################
## - IMputation - ##
####################
#
# This script contains the issing values imputation procedure on the simulated dataset
# and the Final Imputation on the initial dataset on SuperComputer  'Tritanium'.
#
# 
#
# _______ATTENTION!!!___________________________________________ 
# DO NOT RUN THIS SCRIPT IF YOU HAVE NOT RUN THE 'Explore Data.R'
# IMPUTATION TAKES MUCH TIME TO RUN. DON'T RUN IT IN LAPTOPS
# 
# YOU CAN AVOID THIS SCRIPT, YOU CAN FIND READY THE IMPUTED FILE  /data/Exported_DAta/imputedola.csv
# IF YOU SKIP THIS SCRIPT YOU WILL USE imputedola.csv FOR THE NEXT SCRIPTS automatically.
################################################################


## START the imputation with MICE package
#
# m: num_imputation_sets
# maxit: max iterations
# method: model used for imputations eg 'pmm'
# Check MICE documentation for further information
#
# set working directory (change it before running in different machines)
setwd(dir = "~/Documents/DBDM/Assignment-3/census/Project/Final/")


 
start_time = Sys.time()

# 
packs <- c("mice" , "VIM" , "ggplot2" , "corrgram" , "RCurl" , "XML" , "xtable" , "corrgram",
           "prettyR" , "ggplot2" , "gridExtra", "missForest" , "nnet" ,"arules","arulesViz" ) 
# install.packages(packs , dependencies = T)
# Load packages
library(nnet)
library(mice)      ## package for general multiple imputation
library(missForest)## package for factorial imputation
library(VIM)
library(RCurl)     ## package for downloading URLs 
library(XML)       ## package for reading HTML files
library(xtable)    ## package for printing HTML tables
library(corrgram)  ## package for plotting correlations
library(prettyR)   ## package for cross-tabulation
library(ggplot2)   ## package for producing graphical plots
library(gridExtra)
library(arules)
library(arulesViz)
###############



df0.miss = read.csv("data/Exported_Data/data_sim.csv" , header=T , stringsAsFactors=T)
df0 = read.csv("data/Exported_Data/data_sim_truth.csv" , header=T , stringsAsFactors=T)


## --- Imputation of training data [df0.miss] -- ##
#
#-1st try of imputation with Predicting Mean Matching
imputed_Data.train1 <- mice(df0.miss, m=1, maxit = 10, method = 'pmm', seed = 1) 
#-2nd try with Classification and regression trees
imputed_Data.train2 <- mice(df0.miss , m=1 , maxit=20 , method = 'cart', minbucket = 10)
#-3d try with polytomous logistic regression (nnet)
imputed_Data.train3 <- mice(df0.miss , m=1 , maxit=20 , method = 'polyreg', nnet.MaxNWts = 15100)

pmm = mice::complete(imputed_Data.train1)
cart = mice::complete(imputed_Data.train2)
polyreg = mice::complete(imputed_Data.train3)

# For testing the accuracy of NA we will use the mixError function from missForest library
# mixError : ximp = the imputed data , xmis = the simulated NA_Dataset , xtrue = groundTruth data
err_pmm = mixError(ximp = pmm , xmis = df0.miss , xtrue = df0 )
err_cart = mixError(ximp = cart , xmis = df0.miss , xtrue = df0 )
err_polyreg = mixError(ximp = polyreg , xmis = df0.miss , xtrue = df0 )

cat("Error for Predicting Mean Matching: \n" , err_pmm)
cat("Error for RandomForest: \n" , err_cart)
cat("Error for Polytomus Logistic Regression: \n" , err_polyreg)

# NRMSE : normalized mean squared error. 
# Represents error derived from imputing continuous values.
# PFC : proportion of falsely classified. 
# Represents error derived from imputing categorical values.
#
# The previous procedure of Imputation had run on superComputer 'Duranium' 
# The best results of Imputation provided by RandomForest as below:
#
# NRMSE        PFC 
# NaN      0.05929779 
#
# According to many different tries with different methods and different Imputations 
# we conclude that the RandomForest imputation is giving the best results in the simulation daset
# so we implement this method for imputation in the initial dataset assuming that the specific 
# imputation method is efficient.
# 
# INFO : Imputation takes many hours in a common PC so Don't try to run it on laptop
#        We used superComputer 'Tritanium' to run the hole imputation on initial data



## --- Imputation for Initial Data [df] --- ##
# ----------- Tritanium_Script ------------- #
#
# data = read.table(file= /home/s2077981/DBDM/data_Init.csv)
# 
# final_imp = mice(mice(data , m=1 , maxit=20 , method = 'cart', minbucket = 10))
# 
# impData = mice::complete(final_imp)
#
# write.csv(x=impData , file="~/DBDM/imputedola.csv")
#
end_time = Sys.time()
time = end_time - start_time
cat("\n")
cat("Time in secs------> " , time )
cat("\n")

