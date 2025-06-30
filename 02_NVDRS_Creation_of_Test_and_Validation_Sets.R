
###################################################################
## NVDRS Firearm Suicide -- Creation of test and validation sets ##
###################################################################

#Written by: Emma Gause
#Date: 12/20/21


#Load in libraries
library("tidyverse")

#Create path to directory
datadir <- "[insert path to directory]"

#Read in prepped data (missingness included)
dat <- readRDS(paste0(datadir, "NVDRS_Firearm_Suicide_2005_2018.rds"))

##------------------------------------------------------------------------##

#START WITH EXCLUSIONS

#Look at age <9
summary(dat$AgeYears_c) #only 1 death aged <9 years (aged 8)
table(is.na(dat$AgeYears_c)) #5 people missing age

#now remove age lt 9
data <- dat %>% filter(AgeYears_c>=9|is.na(AgeYears_c))
#100958 records remain

#now remove Guam, Puerto Rico (These are not included in NVSS data)
table(data$InjuryState_imp) #281 missing injury FIPS
data <- data %>% filter(InjuryState_imp!="Guam" & InjuryState_imp!="Puerto Rico") # 60 removed

#how many missing FIPS?
table(is.na(data$InjuryFIPS)) #280 missing injury FIPS
#these will just be missing for any county level variables...

#Create an arbitrary ID variable
data <- data %>% mutate(id = row_number())

#### THIS IS THE COHORT FOR RANDOMIZATION & SAMPLING ####
saveRDS(data, paste0(datadir, "NVDRS_ANALYSIS_SET.rds"))

##------------------------------------------------------------------------##

#randomly sample 80/20 by state/year
set.seed(122021)

#Group Data
data <- data %>% group_by(IncidentYear, InjuryState_imp) 

#Create training set
train <- data %>% slice_sample(prop=0.80)

#Create test set
test  <- anti_join(data, train, by = 'id')

##------------------------------------------------------------------------##
#test proportions in samples - state
tr <- table(train$InjuryState_imp)
te <- table(test$InjuryState_imp)

trp <- as.data.frame(prop.table(tr)*100)
tep <- as.data.frame(prop.table(te)*100)

xx <- merge(trp, tep, by = "Var1") # they look the same!

#now try by year
tr <- table(train$IncidentYear)
te <- table(test$IncidentYear)

trp <- as.data.frame(prop.table(tr)*100)
tep <- as.data.frame(prop.table(te)*100)

xx <- merge(trp, tep, by = "Var1") # they look the same!

##------------------------------------------------------------------------##

#ungroup data frames
train <- ungroup(train)
test <- ungroup(test)

#export the training and testing data to their own .rds
saveRDS(train, paste0(datadir, "NVDRS_TRAINING_SET.rds"))
saveRDS(test, paste0(datadir, "NVDRS_TESTING_SET.rds"))






