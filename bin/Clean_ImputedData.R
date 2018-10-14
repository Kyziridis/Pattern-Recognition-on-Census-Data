#!/usr/bin/env Rscript

#####################################################################################
############################# READ CLEAN FACTORISE DATA #############################
#####################################################################################
#
# Script for data exploration on the imputed dataset, visualization and Discriptive statistics
#
# Set your working directory
# Run the Script
# Output : Plots in .png files , tables in .html/csv

# set working directory (change it before running in different machines)
setwd(dir = "~/Documents/DBDM/Assignment-3/census/Project/Final/")



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


 
# casual geeky runtime check (will be followed by end_time <- Sys.time() at the end of the script)
start_time <- Sys.time() 



# read data (prerequisite that the .csv is at the working directory)
x <- read.csv("data/Exported_Data/imputedola.csv") # read data

# keep a copy of the original data
x.original <- x

# remove NA rows
x <- x[complete.cases(x),]



## - Omitting - ## 
##################
#
# omit variables that have no explanation
x$X <- NULL
x$detailed.industry.recode <- NULL
x$detailed.occupation.recode <- NULL
x$capital.gains <- NULL
x$capital.losses <- NULL
x$dividends.from.stocks <- NULL
x$instance.weight <- NULL
x$own.business.or.self.employed <- NULL
x$veterans.benefits <- NULL
x$fill.inc.questionnaire.for.veteran.s.admin <- NULL
x$year <- NULL

# omit variables as subcategories of other variables
x$state.of.previous.residence <- NULL #  subcategory of $region.of.previous.residence
x$detailed.household.and.family.stat <- NULL #  subcategory of $detailed.household.summary.in.household
x$major.occupation.code <- NULL # subcategory of x$major.industry.code

# omit variables that are noisy as fuck
# x$country.of.birth.father <- NULL
# x$country.of.birth.mother <- NULL
# x$country.of.birth.self <- NULL
# x$reason.for.unemployment <- NULL
# x$migration.code.change.in.msa <- NULL
# x$migration.code.change.in.reg <- NULL
# x$migration.code.move.within.reg <- NULL
#####




## - Dscriptive and plots - ##
##############################
#
# Dscriptive Statistics
s1 = summary(x)

# Export summary table in .html file
cat("\n")
cat("Exporting html script.... SummaryInitial.html  >_  \n")
write(print(xtable(s1, caption='Summary (initial data)'), 
            caption.placement = 'top', type="html",
            html.table.attributes='border=10 align="center" bgcolor="#FFCC00"') , 
      file="xps/SummaryInitial.html")
cat("\n")
cat("Summary .html file exported: TRUE      >_\n")
cat("You can find it under /xps dir         >_ \n")

# Plots for Continues Variables
#
# age
p1 <- ggplot(x, aes(x=age)) + ggtitle("Age") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth=5, colour="white", fill="grey") + 
  ylab("Percentage")

# log(wage pare hour)
p2 <- ggplot(x[x$wage.per.hour != 0 , ], aes(x=log(x$wage.per.hour[x$wage.per.hour != 0]))) + ggtitle("wage.per.hour") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)) , binwidth=0.5  , colour="white", fill="grey") +
  ylab("Percentage") + xlab("log(wage.per.hour)")



#number of persons worked for employer
p3 <- ggplot(x, aes(x=num.persons.worked.for.employer)) + 
  ggtitle("num of persons worked for employer") + 
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth=1, colour="white", fill="grey") + 
  ylab("Percentage")


# Exporting Plots
cat("\n")
cat("Exporting plots for continues variables.......\nYou can find them under /plots directory >_ \n")

png("plots/Histogramms for Continues variables.png" , width = 13, height = 4, units = 'in', res = 300)
grid.arrange(p1, p2, p3 , ncol=3)
dev.off()


# Plots for Factor Variables
# 
# First sort the factors according to the frequency of their levels
#
# Define a function of sorting the factors
categ.sort <- function(x){reorder(x,x,function(y){-length(y)})} 
categ.var <- which(sapply(x, is.factor))
for (c in categ.var){  
  x[,c] <- categ.sort(x[,c])   
}
#
# Plots

# Class.of.Worker
p1 <- ggplot(x, aes(x=class.of.worker)) + ggtitle("Work Class") + xlab("Class") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), colour = "black" , fill = "grey") + ylab("Percentage") + coord_flip() + 
  scale_x_discrete(limits = rev(levels(x$class.of.worker)))

# Education
p2 <- ggplot(x, aes(x=education)) + ggtitle("Education") + xlab("Education") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)),colour = "black" , fill = "grey") + ylab("Percentage") + coord_flip() +
  scale_x_discrete(limits = rev(levels(x$education)))

# Major Industry code
p3 <- ggplot(x, aes(x=major.industry.code)) + ggtitle("Working Field") + xlab("IndustryCode") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)),colour = "black" , fill = "grey") + ylab("Percentage") + coord_flip() +
  scale_x_discrete(limits = rev(levels(x$major.industry.code)))

# Citizenship
p4 <- ggplot(x, aes(x=citizenship)) + ggtitle("Citizenship") + xlab("Citizen") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)),colour = "black" , fill = "grey") + ylab("Percentage") + coord_flip() + 
  scale_x_discrete(limits = rev(levels(x$citizenship))) 

# MaritalStatus
p5 <- ggplot(x, aes(x=marital.stat)) + ggtitle("Marital Status") + xlab("Stats") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)),colour = "black" , fill = "grey") + ylab("Percentage") + coord_flip() + 
  scale_x_discrete(limits = rev(levels(x$marital.stat)))

# race 
p6 <- ggplot(x, aes(x=race)) + ggtitle("Race") + xlab("Race") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)),colour = "black" , fill = "grey") + ylab("Percentage") + coord_flip() + 
  scale_x_discrete(limits = rev(levels(x$race)))

gr1 = grid.arrange(p2, p3,nrow=2)
gr2 =  grid.arrange(p4 ,p1 , p5 , p6 ,  nrow=2)

gr3 = grid.arrange(p5, p6 , nrow=2)


# Exporting
cat("\n")
cat("Exporting plots for Factor variables.......\nYou can find them under /plots directory >_ \n")
png("plots/Plot_Factors.png" , width = 2000, height = 800)
grid.arrange(p1, p3, p4, p2, p5, p6 , ncol=2)
dev.off()
#
#

# BoxPlots for correlation between continues variables and icnome(Binary)
cat("\n")


 par(mfrow=c(1,2)) # grid
  
  boxplot(x$age~x$Binary, main="Age vs. Income", 
          xlab="Income", ylab="Age")
  
  boxplot(x$wage.per.hour[x$wage.per.hour != 0]~x$Binary[x$wage.per.hour != 0], main="Wage vs. Income Class", 
          xlab="Income Class", ylab="Wage per hour")
  par(mfrow=c(1,1))
  
#####




## - Transpose variables into factors - ##
##########################################
#
# Grouping variables and fix their levels for the apriori algorithm 
# age grouping
x$age <- ordered(cut(x$age, breaks = c(-1, 18, 29, 45, 65, 150)),
                 labels = c("Minor", "Young", "Middle-aged", "Senior", "Old"))

# education grouping
levels(x$education)=c(" Not graduated", " Not graduated",                            
                      " Not graduated", " Not graduated",              
                      " Not graduated", " Not graduated",                     
                      " Not graduated", " Associates degree-academic program",    
                      " Associates degree-occup /vocational", " Bachelors degree(BA AB BS)",            
                      " Not graduated", " Doctorate degree(PhD EdD)",             
                      " High school graduate", " Not graduated",                   
                      " Masters degree(MA MS MEng MEd MSW MBA)", " Prof school degree (MD DDS DVM LLB JD)",
                      " Some college but no degree")

# wage per hour grouping
x$wage.per.hour=ordered(cut(x$wage.per.hour, breaks = c(-1, 1000, 2000, 4000, 10000)),
                        labels = c("Low", "Mid low", "Mid high", "High"))

# transform to ordered factor
x$num.persons.worked.for.employer = ordered(as.factor(x$num.persons.worked.for.employer))

# transform to ordered factor
x$weeks.worked.in.year =  ordered(cut(x$weeks.worked.in.year, breaks = c(-1, 10, 20, 30, 40, 60)),
                                  labels = c("0-10", "10-20", "20-30", "30-40", "40+"))


cat("\n")

s2 = summary(x)


# Export summary table in .html file
cat("\n")

  cat("\n")
  cat("Exporting html script....... SummaryCleanData.html under /xps dir    >_ \n")
  cat(">_ >_ >_ >_ >_\n")
  cat(">_>_>_>_>_>_>_>_>_>_>\n")
  cat(">_>_>_>_>_>_>_>_>_>_>_>_>_>_>>_")

write(print(xtable(s2, caption='Summary'), 
            caption.placement = 'top', type="html",
            html.table.attributes='border=10 align="center" bgcolor="#FFCC00"') , 
      file="xps/SummaryCleanData.html")
cat("\n")
cat("html script....... SummaryCleanData.html exported : TRUE under /xps dir    >_ \n")
cat(">_ >_ >_ >_ >_\n")



end_time <- Sys.time()
cat("\n")
cat("Time in secs---->",end_time - start_time)
cat("\n")
#####











