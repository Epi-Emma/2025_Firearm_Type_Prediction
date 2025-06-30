
##################################################################
## NVDRS Firearm Suicide -- Paper Statistics, Including Table 1 ##
##################################################################

#Written by: Emma Gause
#Date: 01/20/22


#Load in libraries
library("tidyverse")
library("tableone")

#Create path to directory
datadir <- "[insert path to directory]"

#Read in prepped data (missingness included)
dat <- readRDS(paste0(datadir, "NVDRS_ANALYSIS_SET.rds"))

##------------------------------------------------------------------------##
#Get table of long gun to handgun proportion by state (essentially these are the state fixed effects)

#Summarize by Injury State
fe_state <- dat %>% group_by(InjuryState_imp, longgun) %>% 
  summarise(deaths = n())
fe_state <- ungroupfe_state

#convert to wide
fe_state2 <- spread(fe_state, key = "longgun", value = "deaths") %>% 
  select("state" = "InjuryState_imp", "handgun" = "0", "longgun"="1")
fe_state2 <- ungroup(fe_state2)

View(fe_state2)

fe_state2$ltoh <- fe_state2$longgun/fe_state2$handgun

write.csv(fe_state2, "Q:/FIPRP/Firearm_Type/NVSS_NVDRS_ML_Predict/Analysis/State_Proportions.csv",
          row.names=FALSE)

##------------------------------------------------------------------------##

#Table 1
colnames(dat)
CreateCatTable(c("age_cat", "race_3class_f", "hispanic_f", "education_map_f",
                 "marital_f", "year_cat", "died_med_fac_f", "month_f", 
                 "urban", "female"),
               data = dat, strata = "longgun", 
               includeNA = TRUE, test = FALSE)



















