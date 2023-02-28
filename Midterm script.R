#Midterm Script
Midterm <- read_excel("C:/Users/humay/Dropbox/HRM/HRM 753/Midterm/2023 MONICA Subset for Midterm Exam 949obs.xlsx")
save.image("C:/Users/humay/Dropbox/HRM/HRM 753/Midterm/Midterm.RData")

#mice: https://data.library.virginia.edu/getting-started-with-multiple-imputation-in-r/

options (scipen=999)

pacman::p_load(
  tidyverse,    # data management + ggplot2 graphics
  dplyr,        # select
  ggplot2,      # ggplot2 graphics
  skimr,        # get overview of data
  tidymodels,   # for tidy modelling
  survey,       # for survey functions
  srvyr,        # for tidy survey work
  lubridate,    # for converting date character to date format
  tidyquant,    # for tidy time series functions
  patchwork,    # easily combine ggplot rasters
  plyr,         # for seeing the frequency
  freqtables,   # for frequency table
  glue,
  ggpubr,
  mice,         #multiple imputation 
  caret,        # for easy machine learning workflow
  gmodels       # for cross tab percentage
)

#Data management 

Midterm$sex= as.factor (Midterm$sex)
Midterm$sex

Midterm$educ= as.factor (Midterm$educ)
Midterm$smoke= as.factor (Midterm$smoke)

Midterm$numcig

Midterm$startage
Midterm$bpdrugs= as.factor (Midterm$bpdrugs)
Midterm$bpdrugs
Midterm$bpmeasured= as.factor (Midterm$bpmeasured)
Midterm$bpmeasured
Midterm$choldrugs= as.factor (Midterm$choldrugs)
Midterm$choldrugs
Midterm$age
Midterm$bmi
Midterm$sbp
Midterm$dbp
Midterm$whratio
Midterm$tchol

Midterm %>% 
  freq_table(sex)

Midterm %>% 
  freq_table(educ)

Midterm %>% 
  freq_table(smoke)

Midterm %>% 
  freq_table(numcig)

#Work with the missing data

#new data set
Midterm_1 <- Midterm

#recode the missing data

Midterm_1$numcig.na <- ifelse(Midterm_1$numcig == "888", NA, Midterm_1$numcig)

Midterm_1$startage.na <- ifelse (Midterm_1$startage == "99" ,NA, Midterm_1$startage)
Midterm_1$startage.na <- ifelse (Midterm_1$startage.na == "88" ,NA, Midterm_1$startage.na)

Midterm_1$bpdrugs.na <- ifelse (Midterm_1$bpdrugs == "9",NA, Midterm_1$bpdrugs)

Midterm_1$bpmeasured.na <- ifelse (Midterm_1$bpmeasured == "9",NA, Midterm_1$bpmeasured)

Midterm_1$choldrugs.na <- ifelse (Midterm_1$choldrugs == "8",NA, Midterm_1$choldrugs)

Midterm_1$cholmeasured.na <- ifelse (Midterm_1$cholmeasured == "9",NA, Midterm_1$cholmeasured)



#To compare the missing data with the original data
comparison.startage <- cbind (Midterm_1$startage.na, Midterm_1$startage)


#The code calculates what percent of data is missing
P_Midterm <- unlist(lapply(Midterm_1, function(x) sum(is.na(x))))/nrow(Midterm_1)
sort(P_Midterm[P_Midterm > 0], decreasing = TRUE)

#Select out variables that could cause problems in the imputation process
library (dplyr)

Midterm_imputated <- Midterm_1 

#To see the % of missing data in Midterm_imputated 
P_Midterm_imputated <- unlist(lapply(Midterm_imputated, function(x) sum(is.na(x))))/nrow(Midterm_imputated)
sort(P_Midterm_imputated[P_Midterm_imputated > 0], decreasing = TRUE)

Midterm_imputated$cholmeasured.na

library(mice)

#To drop the unnecessary variable 
Midterm_imputated$numcig.na
Midterm_imputated <- subset(Midterm_imputated, select = -numcig)

Midterm_imputated$startage.na
Midterm_imputated <- subset(Midterm_imputated, select = -startage)

Midterm_imputated$bpdrugs.na
Midterm_imputated <- subset(Midterm_imputated, select = -bpdrugs)

Midterm_imputated$bpmeasured.na
Midterm_imputated <- subset(Midterm_imputated, select = -bpmeasured)

Midterm_imputated$choldrugs.na
Midterm_imputated <- subset(Midterm_imputated, select = -choldrugs)

Midterm_imputated$center
Midterm_imputated <- subset(Midterm_imputated, select = -center)

Midterm_imputated <- subset(Midterm_imputated, select = -cholmeasured)

#This three of them is deleted due to the missingness is more than 25% 
Midterm_imputated = subset(Midterm_imputated, select = -choldrugs.na)
Midterm_imputated = subset(Midterm_imputated, select = - numcig.na  )
Midterm_imputated = subset(Midterm_imputated, select = - startage.na )
       
#To see the % of the missing again
P_Midterm_imputated <- unlist(lapply(Midterm_imputated, function(x) sum(is.na(x))))/nrow(Midterm_imputated)
sort(P_Midterm_imputated[P_Midterm_imputated > 0], decreasing = TRUE)


#We run the mice code with 0 iterations 
imp <- mice(Midterm_imputated, maxit=0)

# Extract predictorMatrix and methods of imputation
predM <- imp$predictorMatrix
meth <- imp$method
  
# Setting values of variables I'd like to leave out to 0 in the predictor matrix
predM[, c("sex")] <- 0
predM[, c("educ")] <- 0
predM[, c("smoke")] <- 0
predM[, c("tchol")] <- 0

# If you like, view the first few rows of the predictor matrix
head(predM)

# Specify a separate imputation model for variables of interest 
#varibles have missing data: numcig.na, startage.na, tchol, dbp, sbp, bmi, whratio,  age, 
#Dichotomous variable: bpdrugs.na, bpmeasured.na, choldrugs.na, cholmeasured.na

sum (is.na(Midterm_imputated$tchol))
sum (is.na(Midterm_imputated$dbp))
sum (is.na(Midterm_imputated$sbp))
sum (is.na(Midterm_imputated$bmi))
sum (is.na(Midterm_imputated$whratio))
sum (is.na(Midterm_imputated$age))


# Dichotomous variable
poly < c ( "tchol", "dbp", "sbp", "bmi", "whratio", "age")
log <- c("bpdrugs.na", "bpmeasured.na",  "cholmeasured.na" )

# Turn their methods matrix into the specified imputation models
meth[poly] <- "polr"
meth[log] <- "logreg"

meth

# With this command, we tell mice to impute the anesimp2 data, create 5
# datasets, use predM as the predictor matrix and don't print the imputation
# process. If you would like to see the process, set print as TRUE

imp2 <- mice(Midterm_imputated, maxit = 5, 
             predictorMatrix = predM, 
             method = meth, print =  FALSE)

head(imp2$imp$age)
tail (imp2$imp$age)

# Can also extract the first imputed, complete dataset and look at the first
# rows using the complete function

Midterm_imp_comp <- mice::complete(imp2, 1)
head(Midterm_imp_comp)

# First, turn the datasets into long format
Midterm_imp_comp_long <- mice::complete(imp2, action="long", include = TRUE)

# Convert back to mids type - mice can work with this type
Midterm_imp_comp_long_mids<-as.mids(Midterm_imp_comp_long)

Midterm_imp_comp_long_mids #this is the data set of missing data imputed 

#Regression
fit.m.imp <- with(Midterm_imp_comp_long_mids,
               lm(bmi ~ age))
fit.m.imp

reg.m= lm (Midterm_1$bmi ~ Midterm_1$age)

reg.m

