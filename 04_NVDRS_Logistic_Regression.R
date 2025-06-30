
#############################################################
## NVDRS Firearm Suicide -- LOGISTIC REGRESSION PREDICTION ##
#############################################################

#Written by: Emma Gause
#Date: 02/11/22


#Load in libraries
library("tidyverse")
library("dplyr")
library("caret")

#Create path to directory
datadir <- "[insert path to directory]"

#Read in prepped data (missingness included)
train <- readRDS(paste0(datadir, "NVDRS_TRAINING_SET.rds"))

##------------------------------------------------------------------------##

str(train)
colnames(train)

#Variables to include:
#IncidentYear
#month_f
#female
#AgeYears_c
#hispanic_f
#race_3class_f
#marital_f
#education_map_f
#died_med_fac_f
#as.factor(nchs_code)  --- SENSITIVITY ANALYSIS 
#InjuryState_imp --- SENSITIVITY ANALYSIS

#remove obs missing age
train <- train %>% filter(!is.na(AgeYears_c))

#make binary longgun outcome into factor -- LONG GUN IS OUTCOME -- FIRST
train$longgun_f <- factor(train$longgun, levels = c(1,0),
                          labels = c("longgun","handgun"))
  

##------------------------------------------------------------------------##

#remove obs missing age
train <- train %>% filter(!is.na(AgeYears_c))

#Create Stratified K-Folds
folds <- 10
set.seed(012022)
cvIndex <- createFolds(factor(train$longgun_f), folds, returnTrain = T)
set.seed(012022)
tc <- trainControl(index = cvIndex,
                   method = 'cv', 
                   number = folds,
                   savePredictions = TRUE, classProbs = TRUE)

##------------------------------------------------------------------------##

#EMPTY MODEL

#Set up empty regression model
set.seed(012022)
empty <- nullModel(y=train$longgun_f)

#predict null
results <- predict(empty)

#create the confusion matrix of null model (uses .5 as the threshold value)
confmat <- confusionMatrix(results, train$longgun_f)
confmat


##------------------------------------------------------------------------##

#DECEDENT VARIABLES ONLY

#create the logistic regression model
set.seed(012022)
logmod <- train(longgun_f ~ IncidentYear + month_f + female + AgeYears_c + 
                    hispanic_f + race_3class_f + marital_f + education_map_f + 
                    died_med_fac_f, 
                  data=train, 
                  method = "glm", 
                  family = "binomial"(link = "logit"),
                  trControl = tc,
                  metric="Kappa")

logmod
summary(logmod)

#create the confusion matrix (uses .5 as the threshold value)
confmat_log <- confusionMatrix(logmod$pred$pred, logmod$pred$obs)
confmat_log


##------------------------------------------------------------------------##

#DECEDENT VARIABLES + URBANICITY

#create the logistic regression model
set.seed(012022)
logurb <- train(longgun_f ~ IncidentYear + month_f + female + AgeYears_c + 
                      hispanic_f + race_3class_f + marital_f + education_map_f + 
                      died_med_fac_f + as.factor(nchs_code), 
                    data=train, 
                    method = "glm", 
                    family = "binomial"(link = "logit"),
                    trControl = tc,
                    metric="Kappa")

logurb
summary(logurb)

#create the confusion matrix (uses .5 as the threshold value)
confmat_urb <- confusionMatrix(logurb$pred$pred, logurb$pred$obs)
confmat_urb


##------------------------------------------------------------------------##

#DECEDENT VARIABLES + URBANICITY + STATE

#Fit logistic regression model w/ state
set.seed(012022)
logstate <- train(longgun_f ~ IncidentYear + month_f + female + AgeYears_c + 
                   hispanic_f + race_3class_f + marital_f + education_map_f + 
                   died_med_fac_f + as.factor(nchs_code) + as.factor(InjuryState_imp), 
                 data=train, 
                 method = "glm", 
                 family = "binomial"(link = "logit"),
                 trControl = tc,
                 metric="Kappa")

summary(logstate)
logstate

#create the confusion matrix (uses .5 as the threshold value)
confmat_state <- confusionMatrix(logstate$pred$pred, logstate$pred$obs)
confmat_state

##------------------------------------------------------------------------##