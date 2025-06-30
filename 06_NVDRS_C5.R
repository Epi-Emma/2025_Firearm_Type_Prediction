
############################################
## NVDRS Firearm Suicide -- C5 PREDICTION ##
############################################

#Written by: Emma Gause
#Date: 02/11/22


#Load in libraries
library("plyr")
library("tidyverse")
library("caret")
library("C50")
library("dplyr")
library("epiR")


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
#as.factor(nchs_code) --- SENSITIVITY ANALYSIS 
#InjuryState_imp --- AS SENSITIVITY ANALYSIS


#make binary longgun outcome into factor -- LONGGUN FIRST
train$longgun_f <- factor(train$longgun, levels = c(1,0),
                          labels = c("longgun","handgun"))

#make nchs 6-class into factor
train$nchs_code_f <- factor(train$nchs_code, levels = c(1, 2, 3, 4, 5, 6, 99),
                          labels = c("1", "2","3","4","5","6","99"))

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

# Choose the features and classes
x <- train[c("IncidentYear","month_f","female","AgeYears_c","hispanic_f",
             "race_3class_f","marital_f","education_map_f", "died_med_fac_f")]
y <- train$longgun_f

grid <- expand.grid( .winnow = c(TRUE,FALSE), .trials=c(1,5,10,15,20), .model="tree" )

#create the C5 model
set.seed(012022)
c5mod <- train(x = x, y = y, method = "C5.0", 
                  tuneGrid=grid,
                  trControl = tc,
                  metric="Kappa")

c5mod

#get predictions
predc5 <- c5mod$pred

#keep only best fit model based on grid tuning
predc5 <- predc5 %>% filter(winnow==FALSE&trials==5)

#create the confusion matrix (uses .5 as the threshold value)
confmat_c5 <- confusionMatrix(predc5$pred, predc5$obs)
confmat_c5

##------------------------------------------------------------------------##

#DECEDENT VARIABLES + URBANICITY

# Choose the features and classes
xu <- train[c("IncidentYear","month_f","female","AgeYears_c","hispanic_f",
             "race_3class_f","marital_f","education_map_f", "died_med_fac_f",
             "nchs_code_f" )]

#create the C5 model
set.seed(012022)
c5urb <- train(x = xu, y = y, method = "C5.0", 
               tuneGrid=grid,
               trControl = tc,
               metric="Kappa")

c5urb

#get predictions
predc5u <- c5urb$pred

#keep only best fit model based on grid tuning
predc5u <- predc5u %>% filter(winnow==FALSE&trials==5)

#create the confusion matrix (uses .5 as the threshold value)
confmat_urb <- confusionMatrix(predc5u$pred, predc5u$obs)
confmat_urb

##------------------------------------------------------------------------##

#DECEDENT VARIABLES + URBANICITY + STATE

# Choose the features and classes
xus <- train[c("IncidentYear","month_f","female","AgeYears_c","hispanic_f",
             "race_3class_f","marital_f","education_map_f", "died_med_fac_f",
             "nchs_code_f", "injury_state_f")]

#create the C5 model
set.seed(012022)
c5state <- train(x = xus, y = y, method = "C5.0", 
               tuneGrid=grid,
               trControl = tc,
               metric="Kappa")

c5state
predc5us <- c5state$pred

#keep only best fit model based on grid tuning
predc5us <- predc5us %>% filter(winnow==FALSE&trials==5)

#create the confusion matrix (uses .5 as the threshold value)
confmat_state <- confusionMatrix(predc5us$pred, predc5us$obs)
confmat_state

#get the 95% CIs for sensitivity and specificity
conftab <- as.table(matrix(c(3349,3475,16354,57363), nrow = 2, byrow = TRUE))
rval <- epi.tests(conftab, conf.level = 0.95)
print(rval)

#Get 95% CIs for Kappa
epi.kappa(conftab)


##------------------------------------------------------------------------##


#c5 with urbanicity and state selected as the best fitting model! 
  # Even though it is very bad! 

#load in validation data
test <- readRDS(paste0(datadir, "NVDRS_TESTING_SET.rds"))


#make binary longgun outcome into factor -- LONGGUN FIRST
test$longgun_f <- factor(test$longgun, levels = c(1,0),
                          labels = c("longgun","handgun"))

#make nchs 6-class into factor
test$nchs_code_f <- factor(test$nchs_code, levels = c(1, 2, 3, 4, 5, 6, 99),
                            labels = c("1", "2","3","4","5","6","99"))

#make injury state into factor
test$injury_state_f <- as.factor(test$InjuryState_imp)

#remove states with no obs in training
table(test$injury_state_f)
testx <- test %>% filter(injury_state_f!="Arkansas"&
                           injury_state_f!="Tennessee"&
                           injury_state_f!="Idaho"&
                           injury_state_f!="South Dakota")

#Get the predictions
valid <- predict(c5state, newdata = testx)


#create the confusion matrix (uses .5 as the threshold value)
confmat_valid <- confusionMatrix(valid, testx$longgun_f)
confmat_valid

#get the 95% CIs for sensitivity and specificity
confval <- as.table(matrix(c(643,623,4263,14820), nrow = 2, byrow = TRUE))
rval <- epi.tests(confval, conf.level = 0.95, digits=4)
print(rval)



##------------------------------------------------------------------------##
