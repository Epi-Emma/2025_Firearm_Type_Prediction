# 2025_Firearm_Type_Prediction

This repository contains code used to assess the feasibility of predicting missing firearm types used in firearm suicide deaths within the National Violent Death Reporting System (NVDRS) and National Vital Statistics System (NVSS) data systems. 

--------------
**The following files are included:**

- *01_NVDRS_State_Investiagate.R* - Investigates the proportion of handgun to long gun suicide deaths in each NVDRS reporting state over time (creates Figure 2). 
- *02_NVDRS_Creation_of_Test_and_Validation_Sets.R* - Prepares NVDRS data for analysis and splits into 80% training and 20% test sets. 
- *03_Table1.R* - Creates Table 1. 
- *04_NVDRS_Logistic_Regression.R* - Fits the initial empty model, as well as the logistic regression prediction model. 
- *05_NVDRS_Random_Forest.R* - Fits the random forest prediction model. 
- *06_NVDRS_C5.R* - Fits the C5 prediction model and calculates accuracy statistics on the test data. 

--------------
**Data Availability:**

Restricted use data were requested from the [National Violent Death Reporting System](https://www.cdc.gov/nvdrs/about/nvdrs-data-access.html), and the [National Vital Statistics System](https://www.cdc.gov/nchs/nvss/nvss-restricted-data.htm) and cannot be shared. Those interested in using these data should follow similar data request processes.

___

**If you use this R code, please cite the following publication:**

Gause EL, Ellyson AM, Rowhani-Rahbar A. Feasibility of predicting firearm type in firearm suicide deaths for better policy evaluation. *Discov Public Health.* 2025;22(1). doi:10.1186/s12982-025-00781-6

