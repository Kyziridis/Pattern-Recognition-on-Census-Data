########################################################################
############################# APRIORI TIME #############################
########################################################################

# higher confidence value => less rules
# higher support value => less rules
# higher maxlen value => more rules

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


#CHAPTER 1: OVERVIEW OF RULES

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










# convert dataset to transactions format
x.trans <- as(x, "transactions") 

# Item frequencies of items with support greater than 20%. (prerequisite to fullscreen it)
cat("\n")
cat("Exporting itemFrequencyPlot.png under /plots dir    >_ \n")
png('plots/itemFrequencyPlot.png', width = 8, height = 6, units = 'in', res = 300)
itemFrequencyPlot(x.trans, support = 0.2, cex.names=0.5, res = 300)
dev.off()


cat("\n")
# rules construction
rules <- apriori(x.trans, parameter = list(support = 0.001, confidence = 0.5, minlen = 2, maxlen=4))

#number of rules
rules

#summary more info on 2 3 rules 
summary(rules)

# scatterplot of all rules (so pretty)
cat("\n")
cat("Exporting rulesPlot.png under /plots dir           >_ \n")
png('plots/rulesPlot.png', width = 8, height = 6, units = 'in', res = 300)
plot(rules)
dev.off()

cat("\n")


# saving to csv
cat("\n")
cat("Exporting rules under  /xps s dir       >_ \n Wait some time.....    >_")
df.rules <- as(rules, "data.frame")
write.csv(df.rules, file = "xps/allrules.csv")

# n highest lift scoring rules
inspect(head(rules, n = 2, by = "lift"))

# n highest confidence scoring rules
inspect(head(rules, n = 10, by = "confidence"))

# SO MANY FUCKING RULES ARE USELESS
# CHAPTER 2: FINDING USEFUL RULES

# 5 highest lift scoring rules by valiable (time consuming)
r1 <- as(head(subset(rules, subset = rhs %pin% "age="), n = 5, by = "lift"), "data.frame")
r2 <- as(head(subset(rules, subset = rhs %pin% "class.of.worker="), n = 5, by = "lift"), "data.frame")
r3 <- as(head(subset(rules, subset = rhs %pin% "education="), n = 5, by = "lift"), "data.frame")
r4 <- as(head(subset(rules, subset = rhs %pin% "wage.per.hour="), n = 5, by = "lift"), "data.frame")
r5 <- as(head(subset(rules, subset = rhs %pin% "enroll.in.edu.inst.last.wk="), n = 5, by = "lift"), "data.frame")
r6 <- as(head(subset(rules, subset = rhs %pin% "marital.stat="), n = 5, by = "lift"), "data.frame")
r7 <- as(head(subset(rules, subset = rhs %pin% "major.industry.code="), n = 5, by = "lift"), "data.frame")
r8 <- as(head(subset(rules, subset = rhs %pin% "race="), n = 5, by = "lift"), "data.frame")
r9 <- as(head(subset(rules, subset = rhs %pin% "hispanic.origin="), n = 5, by = "lift"), "data.frame")
r10 <- as(head(subset(rules, subset = rhs %pin% "sex="), n = 5, by = "lift"), "data.frame")
r11 <- as(head(subset(rules, subset = rhs %pin% "member.of.a.labor.union="), n = 5, by = "lift"), "data.frame")
r12 <- as(head(subset(rules, subset = rhs %pin% "reason.for.unemployment="), n = 5, by = "lift"), "data.frame")
r13 <- as(head(subset(rules, subset = rhs %pin% "full.or.part.time.employment.stat="), n = 5, by = "lift"), "data.frame")
r14 <- as(head(subset(rules, subset = rhs %pin% "tax.filer.stat="), n = 5, by = "lift"), "data.frame")
r15 <- as(head(subset(rules, subset = rhs %pin% "region.of.previous.residence="), n = 5, by = "lift"), "data.frame")
r16 <- as(head(subset(rules, subset = rhs %pin% "detailed.household.summary.in.household="), n = 5, by = "lift"), "data.frame")
r17 <- as(head(subset(rules, subset = rhs %pin% "migration.code.change.in.msa="), n = 5, by = "lift"), "data.frame")
r18 <- as(head(subset(rules, subset = rhs %pin% "migration.code.change.in.reg="), n = 5, by = "lift"), "data.frame")
r19 <- as(head(subset(rules, subset = rhs %pin% "migration.code.move.within.reg="), n = 5, by = "lift"), "data.frame")
r20 <- as(head(subset(rules, subset = rhs %pin% "live.in.this.house.1.year.ago="), n = 5, by = "lift"), "data.frame")
r21 <- as(head(subset(rules, subset = rhs %pin% "migration.prev.res.in.sunbelt="), n = 5, by = "lift"), "data.frame")
r22 <- as(head(subset(rules, subset = rhs %pin% "num.persons.worked.for.employer="), n = 5, by = "lift"), "data.frame")
r23 <- as(head(subset(rules, subset = rhs %pin% "family.members.under.18="), n = 5, by = "lift"), "data.frame")
r24 <- as(head(subset(rules, subset = rhs %pin% "country.of.birth.father="), n = 5, by = "lift"), "data.frame")
r25 <- as(head(subset(rules, subset = rhs %pin% "country.of.birth.mother="), n = 5, by = "lift"), "data.frame")
r26 <- as(head(subset(rules, subset = rhs %pin% "country.of.birth.self="), n = 5, by = "lift"), "data.frame")
r27 <- as(head(subset(rules, subset = rhs %pin% "citizenship="), n = 5, by = "lift"), "data.frame")
r28 <- as(head(subset(rules, subset = rhs %pin% "weeks.worked.in.year="), n = 5, by = "lift"), "data.frame")
r29 <- as(head(subset(rules, subset = rhs %pin% "Binary="), n = 5, by = "lift"), "data.frame")

rules.by.variable <- rbind(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10,
                           r11, r12, r13, r14, r15, r16, r17, r18, r19, r20,
                           r21, r22, r23, r24, r25, r26, r27, r28, r29)



# export to latex (table -> tabular format)
library(xtable)
xtable(rules.by.variable)

#export to csv as "rtable.csv"
cat("\n")
cat("Exporting rulesTable under  /xps  dir             >_ \n")
write.csv(rules.by.variable, file = "xps/rtable.csv")

# now all we have to do is inspect the rules.by.variable object and find interesting shit 

# runtime check (look at line 1)
end_time <- Sys.time()
end_time - start_time
cat("\n")
cat("Time in secs----->" , end_time - start_time)
cat("\n")


###########################################################################
############################# SOURCES & LINKS #############################
###########################################################################

# https://cran.r-project.org/web/packages/arules/vignettes/arules.pdf DA BEST
# http://r-statistics.co/Association-Mining-With-R.html
# https://rstudio-pubs-static.s3.amazonaws.com/274972_fe87238f7c0a48d49b05203585fb52a7.html#
# https://www.r-bloggers.com/implementing-apriori-algorithm-in-r/
# http://www.rdatamining.com/examples/association-rules
# https://datascienceplus.com/implementing-apriori-algorithm-in-r/