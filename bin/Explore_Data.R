####~~ Data Bases and Data Mining Assignment ~~############################
#--------------------------------------------------------------------------
#
# Data : Census-income
# Tasks : Binary Classification and Exploration of Interesting Pattern relations
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++
# + Data Set : Multivariate                          +
# + NumberInstances : 199523                         + 
# + Area : Social                                    + 
# + Attribute Characteristics : Categorical, Integer +
# + Number of Attributes : 42                        +
# + Missing Values : Yes as '?'                      +
# ++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#
#++++++++++++++++++++++++++++++++++++
#+------ Script Instractions -------+
#++++++++++++++++++++++++++++++++++++
#
## First of all install and load usefull libraries and then follow the steps.
#
# Set working directory link 
# 
#_______________________________
#______Script Explore Data______
# 1. Initial data examination
# 2. Search data for strange characters and NAs
# 3. Initial Visualization
# 4. Preparation for Imputation
#
#
#############################################################################
# set working directory (change it before running in different machines)
setwd(dir = "~/Documents/DBDM/Assignment-3/census/Project/Final/")

# UNCOMMENT LINE 48 install.packages

# casual geeky runtime check (will be followed by end_time <- Sys.time() at the end of the script)
start_time <- Sys.time()

cat("\n")
cat("Explore_Data Script RUNS.....")

# Installing packages
# If you want to install uncomment line 41
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




## - Importing Data - ##
########################
# Fix the path for your dataset
linkData <- "data/Initial_Data/census-income.data"
data <- read.csv(linkData, header=FALSE , stringsAsFactors = F)

# Write your path for names
name_path = "data/Initial_Data/names_for_R.csv"


#
# Fix the names of the variables

names_cool <- read.csv(name_path, header=FALSE)
names(data) <- names_cool$V1
names(data) = strsplit(names(data) , split = ":")
names(data)[42] = "Binary"




## Step2 - NA_Examination (1)
#
# f : vector with columns with ' ?'
f = NULL
for (i in 1:42) {
  if(any(data[,i] == ' ?') ) {
    f[i] = i
  }
}
f = c(na.omit(f))


# Names of variables with ' ?'
cat("Names of variables with ' ?' : \n" , names(data[,f]))


# We will replace ' ?' with NA
# Transpose character variables into factors in df
df <- as.data.frame(unclass(data))
df$X.. = NULL

for (i in f){
  levels(df[,i])[levels(df[,i]) == " ?"] = NA
}

# Check if all ' ?' became NA
cat("\n")
cat("\nCheck if all ' ?' became NA: " , any(is.na(df)))


# Create df0 as the NAomited dataset using as groundtruth
df0 <- na.omit(df)
write.csv(x = df0 ,
          file = "data/Exported_Data/data_sim_truth.csv")

cat("\n")
cat( "\nDimensions of simulation dataset df0: \nnrow   ncol \n", dim(df0)) #dimensions without NA 
# [1] 95130    42


cat("\n")
cat("\nDimensions of initial correct dataset df: \nnrow  ncol\n" , dim(df)) #dimensions of initial dataset
# [1] 199523     42



# Export df as the real dataset in csv
cat("\n")
cat("Exporting correct initial Dataset as data_Init.csv........ under /data/Exported_Data dir    >_   \n")
write.csv(x = df ,
          file = "data/Exported_Data/data_Init.csv")


#######






##---- Visualization of MissingValues -----##
#############################################
#
# Plot Variables with -NA- from the Initial_Dataset [df] 
cat("\n")
cat("Exporting plots for NA_Pattern......... under /plots dir    >_ \n")
pdf("plots/Plot_NA-init.pdf" )
mice_plot <- aggr(df[f], col=c('navyblue','yellow'), 
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(df[f]), cex.axis=0.3,
                  gap=1, ylab=c("Missing data","Pattern"))
dev.off()

#
# 
# Export into a .html file the table with counts of NA
cat("\n")
cat("Exporting html code for NA_matrix....... under /xps dir     >_ \n")
write(print(xtable(mice_plot$missings[2], caption='MissingData'), 
            caption.placement = 'top', type="html",
            html.table.attributes='border=1 align="center" bgcolor="#FFCC00"') ,
      file="xps/NA_matrix.html")
cat("\n")
#####







## - Simulation - ##
####################
#
# We will build a simulation dataset according to the initial by generating NA values
# based on the NA_patter of the initial dataset [df] in order to make train/testset for the Imputation
#
# Simulation dataset 95130 observations
#
# generate random NA values for specific variables according to the previous plots
# we can see that migration variables follow the same NA_pattern while the others are random.
#
# we use the same random.num array for: "migration.code.change.in.msa"   
#                                       "migration.code.change.in.reg"  
#                                       "migration.code.move.within.reg" 
#                                       "migration.prev.res.in.sunbelt"
#
# but different random.num array for the rest in order to simulate the actual data.
#
# Generate alla random.num using prior prrobability of the dataset

set.seed(667)

rand.same4 = sample(1:dim(df0)[1] , 47533) # random observations for the four migration vars
rand.father = sample(1:dim(df0)[1] , 3200) 
rand.mother = sample(1:dim(df0)[1] , 2917)
rand.self = sample(1:dim(df0)[1] , 1617)
rand.resi = sample(1:dim(df0)[1] , 337)
#
# Generate fake NA data on df0 based on the previous permutations
df0.miss = df0

# fill NA into 4 variables of migration
df0.miss[rand.same4 , c(26,27,28,30)] <- NA

# fil NA into 'country.of.birth.father'
df0.miss[rand.father , 33] <- NA

# fill NA into 'country.of.birth.mother' 
df0.miss[rand.mother , 34] <- NA

# fill NA into 'country.of.birth.self'
df0.miss[rand.self , 35] <- NA

# and just a few into "state.of.previous.residence"
df0.miss[rand.resi , 22] <- NA

#
# Export Simulation data to a scv file
cat("\n")
cat("Exporting the simulation dataset as data_sim.csv.......... under /data/Exported_Data/ dir   >_ \n")
write.csv(x = df0.miss ,
          file = "data/Exported_Data/data_sim.csv")

end_time = Sys.time()
time = end_time - start_time
cat("\n")
cat("Time in secs---->" , time)
cat("\n")
#######
