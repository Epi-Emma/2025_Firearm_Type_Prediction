
#######################################################
## NVDRS Firearm Suicide -- RANDOM FOREST PREDICTION ##
#######################################################

#Written by: Emma Gause
#Date: 02/11/22


#Load in libraries
library("tidyverse")
library("dplyr")
library("randomForest")
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
#InjuryState_imp --- AS SENSITIVITY ANALYSIS

#make binary longgun outcome into factor -- LONGGUN FIRST
train$longgun_f <- factor(train$longgun, levels = c(1,0),
                          labels = c("longgun","handgun"))

#make nchs 6-class into factor
train$nchs_code_f <- factor(train$nchs_code, levels = c(1, 2, 3, 4, 5, 6, 99),
                            labels = c("1","2","3","4","5","6","99"))

#make injury state into factor
train$injury_state_f <- as.factor(train$InjuryState_imp)

##------------------------------------------------------------------------##

#remove obs missing age
train <- train %>% filter(!is.na(AgeYears_c))

#set up the stratified k-fold cross-validation using 10 folds
folds <- 10
set.seed(012022)
cvIndex <- createFolds(factor(train$longgun_f), folds, returnTrain = T)
set.seed(012022)
tc <- trainControl(index = cvIndex,
                   method = 'cv', 
                   number = folds,
                   savePredictions = TRUE, classProbs = TRUE)

##------------------------------------------------------------------------##

#DECEDENT VARIABLES ONLY

#create the random forest model
# Choose the features and classes
x <- train[c("IncidentYear","month_f","female","AgeYears_c","hispanic_f",
             "race_3class_f","marital_f","education_map_f", "died_med_fac_f")]
y <- train$longgun_f

set.seed(012022)
rfmod <- train(x = x, y = y, method = "rf", 
                  tuneLength = 50,
                  ntree=100,
                  importance=TRUE,
                  preProcess = c("center", "scale"),
                  trControl = tc,
                  metric="Kappa")

#get model summary outputs
rfmod
predsrf <- filter(rfmod$pred, mtry==9)
#create the confusion matrix (uses .5 as the threshold value)
confmat <- confusionMatrix(predsrf$pred, predsrf$obs)
confmat

plot(varImp(rfmod), main="Variable Importance - RF Decedent Vars")

##------------------------------------------------------------------------##

#DECEDENT VARIABLES + URBANICITY

#create the random forest model
# Choose the features and classes
xu <- train[c("IncidentYear","month_f","female","AgeYears_c","hispanic_f",
             "race_3class_f","marital_f","education_map_f", "died_med_fac_f",
             "nchs_code_f")]

set.seed(012022)
rfurb <- train(x = xu, y = y, method = "rf", 
                  tuneLength = 50,
                  ntree=100,
                  importance=TRUE,
                  preProcess = c("center", "scale"),
                  trControl = tc,
                  metric="Kappa")

#get model summary outputs
rfurb
predsu <- filter(rfurb$pred, mtry==8)
#create the confusion matrix (uses .5 as the threshold value)
confmat_urb <- confusionMatrix(predsu$pred, predsu$obs)
confmat_urb

plot(varImp(rfurb), main="Variable Importance - RF + Urban")

##------------------------------------------------------------------------##

#DECEDENT VARIABLES + URBANICITY + STATE

# Choose the features and classes
xus <- train[c("IncidentYear","month_f","female","AgeYears_c","hispanic_f",
              "race_3class_f","marital_f","education_map_f", "died_med_fac_f",
              "nchs_code_f", "injury_state_f")]

set.seed(012022)
rfstate <- train(x = xus, y = y, method = "rf", 
                  tuneLength = 50,
                  ntree=500,
                  importance=TRUE,
                  preProcess = c("center", "scale"),
                  trControl = tc,
                  metric="Kappa")
rfstate

predsus <- filter(rfstate$pred, mtry==9)

#create the confusion matrix (uses .5 as the threshold value)
confmat_state <- confusionMatrix(predsus$pred, predsus$obs)
confmat_state

plot(rfstate)
plot(varImp(rfstate), main="Variable Importance - RF + Urban + State")

##------------------------------------------------------------------------##

