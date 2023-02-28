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
  corrplot,     # for plotting the correlation 
  glue,
  ggpubr,
  car,          # omparison of the regression coefficients
  lmtest,       # package is used to conduct the Wald test
  mice,         # multiple imputation 
  pROC,         # ROC curve

  caret,        # for easy machine learning workflow
  gmodels,      # for cross tab percentage
  readxl        #to read xlsx
)
options (scipen=999)

Midterm.new <- read_excel("C:/Users/humay/Dropbox/HRM/HRM 753/Midterm/2023 MONICA Subset for Midterm Exam 949obs.xlsx")
save.image("C:/Users/humay/Dropbox/HRM/HRM 753/Midterm/Midterm.new.RData")

#recode the missing data
Midterm.new$numcig.na <- ifelse(Midterm.new$numcig == "888", NA, Midterm.new$numcig)
Midterm.new$numcig.na= as.integer(Midterm.new$numcig.na)

Midterm.new$numcig.na

Midterm.new$numcig.na= ifelse (Midterm.new$smoke == "2", "0", Midterm.new$numcig.na)
Midterm.new$numcig.na

comparison.numcig.na <- cbind (Midterm.new$numcig.na, Midterm.new$numcig)
comparison.numcig.na

Midterm.new$startage.na <- ifelse (Midterm.new$startage == "99" ,NA, Midterm.new$startage)
Midterm.new$startage.na <- ifelse (Midterm.new$startage.na == "88" ,NA, Midterm.new$startage.na)
Midterm.new$startage.na
sum(is.na(Midterm.new$startage.na))
Midterm.new$startage.na= ifelse (Midterm.new$smoke == "2", "0", Midterm.new$startage.na)
Midterm.new$startage.na
Midterm.new$startage.na= as.numeric (Midterm.new$startage.na)

Midterm.new$bpdrugs.na <- ifelse (Midterm.new$bpdrugs == "9",NA, Midterm.new$bpdrugs)
Midterm.new$bpdrugs.na <- ifelse (Midterm.new$bpdrugs.na == "8",NA, Midterm.new$bpdrugs.na)

Midterm.new$bpmeasured.na <- ifelse (Midterm.new$bpmeasured == "9",NA, Midterm.new$bpmeasured)

Midterm.new$choldrugs.na <- ifelse (Midterm.new$choldrugs == "8",NA, Midterm.new$choldrugs)
Midterm.new$choldrugs.na <- ifelse (Midterm.new$choldrugs.na == "9",NA, Midterm.new$choldrugs.na)


Midterm.new$cholmeasured.na <- ifelse (Midterm.new$cholmeasured == "9",NA, Midterm.new$cholmeasured)


#Frequency of the categorical variables 
Midterm.new %>% 
  freq_table(center) 

Midterm.new %>% 
  freq_table(sex) 

Midterm.new %>% 
  freq_table(educ) 

Midterm.new %>% 
  freq_table(smoke) 

Midterm.new %>% 
  freq_table(bpdrugs.na)

Midterm.new %>% 
  freq_table(choldrugs.na)

Midterm.new$choldrugs.na

#Frequency of the contentious variables 
summary(Midterm.new$age)
sd (Midterm.new$age, na.rm=T)

summary(Midterm.new$numcig.na)
sd (Midterm.new$numcig.na, na.rm=T)

summary(Midterm.new$startage.na)
sd (Midterm.new$startage.na, na.rm=T)

summary(Midterm.new$bmi)
sd (Midterm.new$startage.na, na.rm=T)

summary(Midterm.new$whratio)
sd (Midterm.new$whratio, na.rm=T)

summary(Midterm.new$sbp)
sd (Midterm.new$sbp, na.rm=T)

summary(Midterm.new$dbp)
sd (Midterm.new$dbp, na.rm=T)

summary(Midterm.new$tchol)
sd (Midterm.new$tchol, na.rm=T)

# RQ-1
Midterm.new$tchol

#Create HTN variable (RQ-2)
# 1= yes, 2 = no

Midterm.new$htn= as.factor (ifelse (Midterm.new$sbp>=140 | Midterm.new$dbp>=190, "1", 
                                    ifelse(Midterm.new$sbp== "NA" | Midterm.new$dbp== "NA", "NA", "2")))
Midterm.new$htn

comparison.htn <- cbind (Midterm.new$htn, Midterm.new$sbp, Midterm.new$dbp)
comparison.htn

Midterm.new %>% 
  freq_table(htn) 

#Create BMI categories (RQ-3) 
# 1 = obese, 2 = overweight, 3= normal, 4= underweight 
Midterm.new$bmi

Midterm.new$bmi.cat= as.factor (ifelse (Midterm.new$bmi>=30, "1", 
                                    ifelse(Midterm.new$bmi>=29.9, "2",
                                    ifelse (Midterm.new$bmi>=24.9, "3",
                                    ifelse (Midterm.new$bmi>=18.5, "4",
                                    "NA" )))))
Midterm.new$bmi.cat

comparison.bmi.cat <- cbind (Midterm.new$bmi.cat, Midterm.new$bmi)
comparison.bmi.cat

Midterm.new %>% 
  freq_table(bmi.cat) 


#Multiple Imputation
#old, Midterm.new.imp <- mice(Midterm.new, m = 5, method = 'pmm')

Midterm.new

summary (Midterm.new)

Midterm.new.imp= mice(data=Midterm.new, m=25, maxit = 10, seed= 12345, print=F)

head(complete(Midterm.new.imp, 3))

Midterm.new.imp.comp <- complete(Midterm.new.imp, "long", include = TRUE)
Midterm.new.imp.comp

#visualize the imputed data 
table(Midterm.new.imp.comp$.imp)
Midterm.new.imp.comp$bmi.NA <- cci(Midterm.new.imp.comp$bmi.cat)
head(Midterm.new.imp.comp[, c("bmi.cat", "bmi.NA")])

library(ggplot2)

ggplot(Midterm.new.imp.comp, 
       aes(x = .imp, y = bmi.cat, color = bmi.NA)) + 
  geom_jitter(show.legend = FALSE, 
              width = .1)

#checking analysis with imputed 
with(Midterm.new.imp, mean(bmi))

with(Midterm.new.imp, t.test(bmi ~ htn))

with(Midterm.new.imp, lm(tchol ~ age + htn))

mod1 <- with(Midterm.new.imp, lm(tchol ~ age +sex))

pool(mod1)

pool.mod1= pool(mod1)
summary (pool.mod1)

pool.r.squared(mod1)

#checking analysis without imputed 
mod2= lm (tchol ~ age +sex, data=Midterm.new)
mod3= lm (tchol ~ age, data=Midterm.new)

#sensitivity analysis 
car::compareCoefs(mod2,
                  mod3,
                  pvals = T)




###################################### Main analysis started#######################################
####################### To answer to the research question 1, I have to develop a linear model.######################################
#As the data is contentious, I have to build a linear model. 

#Model without the imputed data
#Check for normality of the outcome variable 
library(ggplot2)

histogram.tchol=ggplot(Midterm.new, aes(x = tchol), na.rm=T) + 
  geom_histogram(aes(y =..density..), binwidth = 2,
                 colour = "black", 
                 fill = "#00BA38") +
  labs(x="Total cholesterol",
       y= NULL,
       title= "")+
  stat_function(fun = dnorm, na.rm=T, args = list(mean = mean(Midterm.new$tchol, na.rm=T), sd = sd(Midterm.new$tchol, na.rm=T),  col = "#1b98e0",
                                         size = 5))

histogram.tchol 

#Based on the histogram, the total cholesterol (outcome variable) seems to be normally distributed. 
#however, we will do a conservative test for that such as shaprio wilk test. 

shapiro.test(Midterm.new$tchol)

#In the Shapiro test, the data was not normal distributed. 
#However, we still can do the linear regression as we see in the histogram, data is look normal.

#To see the % of the missing

Midterm.new$bpdrugs

P_Midterm.new <- unlist(lapply(Midterm.new, function(x) sum(is.na(x))))/nrow(Midterm.new)

sort(P_Midterm.new[P_Midterm.new > 0], decreasing = TRUE)

#There are variables such as choldrugs.na (78%), bpdrugs.na (78%) missing values. 
#Therefore, in the model, we are not considering them, as the power of the model will be very low due to very low sample size.

#do simple linear regression 
model.tchol.age= lm (tchol ~ 
                       age, data=Midterm.new)
summary (model.tchol.age) #1. include in the model 
confint(model.tchol.age)

Midterm.new$sex.fct= as.factor(Midterm.new$sex)
model.tchol.sex.fct= lm (tchol ~ sex.fct, data=Midterm.new)
summary (model.tchol.sex.fct) #2. include in the model based on possible confounder

Midterm.new$educ.fct= as.factor(Midterm.new$educ)
model.tchol.educ.fct= lm (tchol ~ educ.fct, data=Midterm.new)
summary (model.tchol.educ.fct) #not include in the model 


Midterm.new$smoke.fct= as.factor(Midterm.new$smoke)
model.tchol.smoke.fct= lm (tchol ~ smoke.fct, data=Midterm.new)
summary (model.tchol.smoke.fct) #not include in the model 
 

model.tchol.numcig.na= lm (tchol ~ numcig.na, data=Midterm.new)
summary (model.tchol.numcig.na) #not include in the model 

model.tchol.startage.na= lm (tchol ~ startage.na, data=Midterm.new)
summary (model.tchol.startage.na) #not include in the model

model.tchol.bmi= lm (tchol ~ bmi, data=Midterm.new)
summary (model.tchol.bmi) #3. include in the model 

model.tchol.whratio= lm (tchol ~ whratio, data=Midterm.new)
summary (model.tchol.whratio) #4. include in the model 

Midterm.new$htn.fct= as.factor(Midterm.new$htn)
model.tchol.htn.fct= lm (tchol ~ htn.fct, data=Midterm.new)
summary (model.tchol.htn.fct) #5. include in the model 

#Before building the model, we are likely to check the multicolinearity among the variables
library(corrplot)

typeof(Midterm.new$tchol)
typeof (Midterm.new$age)
typeof(Midterm.new$sex)
typeof(Midterm.new$bmi)
typeof (Midterm.new$whratio)
typeof (Midterm.new$htn)

Midterm.new$tchol= as.numeric(Midterm.new$tchol)
Midterm.new$htn= as.numeric (Midterm.new$htn)
mean (Midterm.new$tchol, na.rm=T)

cor.data= data.frame(Midterm.new$tchol, 
                     Midterm.new$age, 
                     Midterm.new$sex, 
                     Midterm.new$bmi, 
                     Midterm.new$whratio, 
                     Midterm.new$htn)
head (cor.data)

cor_matrix <- cor(na.omit(cor.data))
cor_matrix 

cor_matrix=cor(cor.data, use = "pairwise.complete.obs")
cor_matrix #same as above 

corrplot(cor_matrix, method = "color", type = "upper", 
         order = "hclust", tl.col = "black", tl.srt = 45)

#note: From the matrix, sex and whratio (Waist-to-hip ratio) were strongly negatively correlated. 
#note: Therefore, we are not including whratio in the model as it may violet the assumption of independence of the predictor variables

#############################################################(Table 1)##########################################################

#Final multiple regression model
Model.r.q1= lm (tchol~ 
                  age+
                  sex.fct+
                  bmi+
                  htn.fct, 
                data=Midterm.new)
Model.r.q1
summary (Model.r.q1)
confint(Model.r.q1, na.rm=T)


#now check the model fitness 
#VIF
library("olsrr")

ols_vif_tol(Model.r.q1)

#########Plot###########
library (tidyverse)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)

Model.r.q1.plot = plot_model(Model.r.q1,
                               vline.color = "#808000",
                               show.values = F,
                               width = 0.1,
                               value.offset = 0.4,
                               cex = 3,
                               p.shape = TRUE,
                               xmin=error_lower,
                               xmax=error_upper,,
                               face = "bold",
                              
                               face = "bold",
                               title = "") 
Model.r.q1.plot




##############################Considering the other three variables with missing value from 47% to 78% another model was fitted###############################

#Variables with missing: choldrugs.na (78%), bpdrugs.na (78%)
  
typeof(Midterm.new$choldrugs.na)
typeof (Midterm.new$bpdrugs.na)

#Simple linear regression for choldrugs.na (78%), bpdrugs.na (78%)
model.tchol.choldrugs.na= lm (tchol ~ choldrugs.na, data=Midterm.new)
summary (model.tchol.choldrugs.na) #6. include in the model 

model.tchol.bpdrugs.na= lm (tchol ~ bpdrugs.na, data=Midterm.new)
summary (model.tchol.bpdrugs.na) # Not include in the model

#correlation matrix 
cor.data.w.miss= 
  data.frame(Midterm.new$tchol, 
             Midterm.new$age, 
             Midterm.new$sex, 
             Midterm.new$bmi, 
             Midterm.new$whratio,
             Midterm.new$htn, 
             Midterm.new$choldrugs.na
  )
head (cor.data.w.miss)

cor_matrix.w.miss=cor(cor.data.w.miss, use = "pairwise.complete.obs")
cor_matrix.w.miss  

corrplot(cor_matrix.w.miss, method = "color", type = "upper", 
         order = "hclust", tl.col = "black", tl.srt = 45)

#note: From the matrix, sex and whratio (Waist-to-hip ratio) were strongly negatively correlated. 
#note: Therefore, we are not including whratio in the model as it may violet the assumption of independence of the predictor variables. 


#############################################################(Table 2)##########################################################
#Final multiple regression model with choldrugs.na (78%), bpdrugs.na (78%)
Model.r.q1.w.miss= lm (tchol~ 
                         age+
                         sex+
                         bmi+
                         htn.fct+
                         choldrugs.na, 
                       data=Midterm.new)
Model.r.q1.w.miss
summary (Model.r.q1.w.miss)
confint(Model.r.q1.w.miss)

#now check the model fitness with choldrugs.na (78%), bpdrugs.na (78%)
#VIF
library("olsrr")

ols_vif_tol(Model.r.q1.w.miss)

########################sensitivity analysis to compare the models with and without choldrugs.na (78%), bpdrugs.na (78%)
car::compareCoefs(Model.r.q1,
                  Model.r.q1.w.miss,
                  pvals = T)
car::Anova(Model.r.q1,   type = 3)
car::Anova(Model.r.q1.w.miss,   type = 3)

##############################################Model with the imputed data#####################################################
#Check for normality of the outcome variable 
library(ggplot2)
typeof (Midterm.new.imp$data$tchol)

histogram.tchol.imp=with(Midterm.new.imp, hist(x=Midterm.new.imp$data$tchol))
histogram.tchol.imp 

#note: the data look like normal in histogram. so, we can do a linear regression 

###########################do simple linear regression with the imputed data
sum (is.na (Midterm.new.imp$tchol))
mean.tchol= with (Midterm.new.imp, mean (tchol))


model.tchol.age.imp= with (Midterm.new.imp, lm (tchol ~ age))
summary (model.tchol.age.imp)  
pool.model.tchol.age.imp= pool(model.tchol.age.imp)
summary (pool.model.tchol.age.imp)
pool.r.squared(pool.model.tchol.age.imp) #1. include in the model

Midterm.new.imp$data$sex.fct= as.factor(Midterm.new.imp$data$sex)
model.tchol.sex.fct.imp= with (Midterm.new.imp, lm (tchol ~ sex.fct))
summary (model.tchol.sex.fct.imp) 
pool.model.tchol.sex.fct.imp= pool(model.tchol.sex.fct.imp)
summary (pool.model.tchol.sex.fct.imp) #2. include in the model as confounder

Midterm.new.imp$data$educ.fct= as.factor(Midterm.new.imp$data$educ)
model.tchol.educ.fct.imp= with (Midterm.new.imp, lm (tchol ~ educ.fct))
summary (model.tchol.educ.fct.imp) 
pool.model.tchol.educ.fct.imp= pool(model.tchol.educ.fct.imp)
summary (pool.model.tchol.educ.fct.imp) #Not include in the model 

Midterm.new.imp$data$smoke.fct= as.factor(Midterm.new.imp$data$smoke)
model.tchol.smoke.fct.imp= with (Midterm.new.imp, lm (tchol ~ smoke.fct))
summary (model.tchol.smoke.fct.imp) 
pool.model.tchol.smoke.fct.imp= pool(model.tchol.smoke.fct.imp)
summary (pool.model.tchol.smoke.fct.imp) #Not include in the model 

model.tchol.numcig.na.imp= with (Midterm.new.imp, lm (tchol ~ numcig.na))
summary (model.tchol.numcig.na.imp) 
pool.model.tchol.numcig.na.imp= pool(model.tchol.numcig.na.imp)
summary (pool.model.tchol.numcig.na.imp) #Not include in the model

model.tchol.startage.na.imp= with (Midterm.new.imp, lm (tchol ~ startage.na))
summary (model.tchol.startage.na.imp) 
pool.model.tchol.startage.na.imp= pool(model.tchol.startage.na.imp)
summary (pool.model.tchol.startage.na.imp) #Not include in the model

model.tchol.bmi.imp= with (Midterm.new.imp, lm (tchol ~ bmi))
summary (model.tchol.bmi.imp) 
pool.model.tchol.bmi.imp= pool(model.tchol.bmi.imp)
summary (pool.model.tchol.bmi.imp) #Include in the model

model.tchol.whratio.imp= with (Midterm.new.imp, lm (tchol ~ whratio))
summary (model.tchol.whratio.imp) 
pool.model.tchol.whratio.imp= pool(model.tchol.whratio.imp)
summary (pool.model.tchol.whratio.imp) #Include in the model

Midterm.new.imp$data$htn
typeof (Midterm.new.imp$data$htn)
Midterm.new.imp$data$htn.fct=as.factor (Midterm.new.imp$data$htn)
model.tchol.htn.fct.imp= with (Midterm.new.imp, lm (tchol ~ htn.fct))
summary (model.tchol.htn.fct.imp) 
pool.model.tchol.htn.fct.imp= pool(model.tchol.htn.fct.imp)
summary (pool.model.tchol.htn.fct.imp) #Include in the model

#correlation matrix for imputed data 

library(miceadds)

Midterm.new.imp.data.corr= subset(Midterm.new.imp$data, 
                              select = -c(center,
                                          educ,
                                          smoke,
                                          numcig,
                                          startage,
                                          bpdrugs,
                                          bpmeasured,
                                          choldrugs,
                                          cholmeasured,
                                          sbp,
                                          dbp,
                                          educ.fct,
                                          sex.fct,
                                          smoke.fct,
                                          htn.fct,
                                          startage.na,
                                          bpdrugs.na,
                                          bpmeasured.na,
                                          choldrugs.na,
                                          cholmeasured.na,
                                          numcig.na,
                                          bmi.cat) )

Midterm.new.imp.data.corr$htn= as.numeric(Midterm.new.imp.data.corr$htn)

Midterm.new.imp.data.corr.1chain <- miceadds::mice.1chain( Midterm.new.imp.data.corr)
Midterm.new.imp.data.corr.matrix <- miceadds::micombine.cor(mi.res=Midterm.new.imp.data.corr.1chain )

Midterm.new.imp.data.corr.matrix #From this matrix, the sex and whratio strongly correlated. 
#Therefore, we will not included it in the model.

##########################################################(Table 3)##################################################
#Final model for the imputed data 
Midterm.new.imp
Midterm.new.imp$data$htn.fct = as.factor(Midterm.new.imp$data$htn)

Midterm.new.imp.model <- with(Midterm.new.imp, 
                              lm(tchol ~ 
                                   age +
                                   sex.fct+
                                   bmi+
                                   htn.fct))
Midterm.new.imp.model

sum (is.na (Midterm.new.imp$data$tchol))
sum (is.na (Midterm.new$tchol))
Midterm.new.imp$data$tchol
Midterm.new$tchol

Midterm.new.imp.model.pool= pool(Midterm.new.imp.model)
Midterm.new.imp.model.pool
summary (Midterm.new.imp.model.pool, conf.int = TRUE)
pool.r.squared(Midterm.new.imp.model.pool)

#Getting F, df, p, and R in imputted model 

library('miceadds')

nul <- capture.output(
  aov_fit <- miceadds::mi.anova(mi.res=Midterm.new.imp, formula="tchol ~ 
                                   age +
                                   sex.fct+
                                   bmi+
                                   htn.fct" )
)
aov_fit$r.squared  ## R-squared
(fval <- mean(round(aov_fit$anova.table$`F value`, 2), na.rm=TRUE) ) ## F-statistic
df_mod <- aov_fit$anova.table$df1[- nrow(aov_fit$anova.table)]  ## DF model
df_res <- el(fit$analyses)$df.residual  ## DF residual
c(df_mod, df_res)

pf(q=fval, df1=sum(df_mod), df_2=df_res, lower.tail=FALSE)  ## p-value



#VIF the imputed data 

library(car)  # for variance inflation factor (measure of multicollinearity)

# Use mice::getfit to get model for each imputed data set
Midterm.new.imp.model.f1 <- getfit(Midterm.new.imp.model)

# Will hold variance inflation factor for each model, because we have a value 
# for each predictor, we'll use a list to store values
vif_mod <- list(length(Midterm.new.imp.model.f1))

# Cycle through each model and calculate variance inflation factor
for (i in 1:length(Midterm.new.imp.model.f1)) {
  # Pull out model from the ith data set
  one_model <- Midterm.new.imp.model.f1[[i]]
  # Run car::vif on model
  model_vif <- car::vif(one_model)
  # Store in that vif_mod list
  vif_mod[[i]] <- model_vif
}

# Convert list to a data frame
vif_df <- data.frame(do.call(rbind, vif_mod))
# Print distribution of VIF for each predictor
summary(vif_df)
#note: In the model of imputed data VIF was found less than 1.3 for all variables. 





####################### To answer to the research question 2, I have to develop logistic regression model.######################################
#Are people that are overweight or obese1 more likely to be hypertensive? 
#Are there any other factors associated with hypertension? 
Midterm.new$htn.fct

#Recoded the htn to 0 and 1
Midterm.new$htn.fct.recoded = 
  case_when(
    Midterm.new$htn.fct == "1" ~ "1",
    Midterm.new$htn.fct == "2" ~ "0", 
    Midterm.new$htn.fct == "NA" ~ "NA"
  )

#Recoded the bmi to 0=healthy weight and 1=overweight/obese 
Midterm.new$bmi.cat

freq_table(Midterm.new$bmi.cat)

Midterm.new %>% 
  freq_table(bmi.cat)


Midterm.new$bmi.cat.recoded = 
  case_when(
    Midterm.new$bmi.cat == "1" ~ "1",
    Midterm.new$bmi.cat == "2" ~ "1", 
    Midterm.new$bmi.cat == "3" ~ "0", 
    Midterm.new$bmi.cat == "4" ~ "0", 
    Midterm.new$bmi.cat == "NA" ~ "NA"
  )

Midterm.new$bmi.cat.recoded=as.factor (Midterm.new$bmi.cat.recoded)
Midterm.new$bmi.cat.recoded


#do simple logistic regression to answer the first part of the question 
glm.htn.fct.htn.cat= glm (htn.fct.recoded ~ 
                            Midterm.new$bmi.cat.recoded, data=Midterm.new, family= binomial)

summary (glm.htn.fct.htn.cat) #1. include in the model

or.glm.htn.fct.htn.cat=exp(cbind(coef(glm.htn.fct.htn.cat), confint(glm.htn.fct.htn.cat, level=0.95)))
or.glm.htn.fct.htn.cat

#The estimate of the association is 1.16638

#check for confouding varibles 
glm.htn.recoded.age= glm (htn.fct.recoded ~ 
                            Midterm.new$bmi.cat.recoded+age, data=Midterm.new,family= binomial)

summary (glm.htn.recoded.age) #1. include in the model as the change is 11.6% 

glm.htn.recoded.sex.fct= glm (htn.fct.recoded ~ 
                            Midterm.new$bmi.cat.recoded+sex.fct, data=Midterm.new,family= binomial)

summary (glm.htn.recoded.sex.fct) #2. include in the model as a potential confounder 

glm.htn.recoded.educ.fct= glm (htn.fct.recoded ~ 
                            Midterm.new$bmi.cat.recoded+educ.fct, data=Midterm.new,family= binomial)

summary (glm.htn.recoded.educ.fct) # not include in the model as the change is less than 10% 

glm.htn.recoded.smoke.fct= glm (htn.fct.recoded ~ 
                                 Midterm.new$bmi.cat.recoded+smoke.fct, data=Midterm.new,family= binomial)

summary (glm.htn.recoded.smoke.fct) # not include in the model as the change is less than 10% 

glm.htn.recoded.tcho= glm (htn.fct.recoded ~ 
                                  Midterm.new$bmi.cat.recoded+tcho, data=Midterm.new,family= binomial)

summary (glm.htn.recoded.tcho) # not include in the model as the change is less than 10% 

glm.htn.recoded.choldrugs.na= glm (htn.fct.recoded ~ 
                             Midterm.new$bmi.cat.recoded+choldrugs.na, data=Midterm.new,family= binomial)

summary (glm.htn.recoded.choldrugs.na) # include in the model as the change > 10% 

glm.htn.recoded.bpdrugs.na= glm (htn.fct.recoded ~ 
                                     Midterm.new$bmi.cat.recoded+bpdrugs.na, data=Midterm.new,family= binomial)

summary (glm.htn.recoded.bpdrugs.na) # include in the model as the change > 10% 

#Variables to include in the final model: age, sex, choldrugs.na, bpdrugs.na

Midterm.new %>% 
  freq_table(sex.fct) 

Midterm.new %>% 
  freq_table(choldrugs.na) 

Midterm.new %>% 
  freq_table(bpdrugs.na) 

typeof (Midterm.new$sex.fct)
typeof (Midterm.new$choldrugs.na)
typeof (Midterm.new$bpdrugs.na)

Midterm.new$htn.int.recoded=as.integer (Midterm.new$htn.fct.recoded)
Midterm.new$sex.int= as.integer(Midterm.new$sex.fct)
Midterm.new$choldrugs.na.int= as.integer(Midterm.new$choldrugs.na)
Midterm.new$bpdrugs.na.int= as.integer(Midterm.new$bpdrugs.na)

#Check for interaction
Midterm.new$htn.sex.int= Midterm.new$htn.int.recoded*Midterm.new$sex.int
Midterm.new$htn.sex.int

Midterm.new$htn.choldrugs.na.int= Midterm.new$htn.int.recoded*Midterm.new$choldrugs.na.int
Midterm.new$htn.choldrugs.na.int

Midterm.new$htn.bpdrugs.na.int= Midterm.new$htn.int.recoded*Midterm.new$bpdrugs.na.int
Midterm.new$htn.bpdrugs.na.int

#Interaction
glm.htn.sex.int= glm (htn.fct.recoded~bmi.cat.recoded
                  +htn.sex.int,
                  data=Midterm.new, 
                  family = "binomial")

glm.htn.sex.int
summary (glm.htn.sex.int) #No interaction

glm.htn.choldrugs.na.int= glm (htn.fct.recoded~bmi.cat.recoded
                      +htn.choldrugs.na.int,
                      data=Midterm.new, 
                      family = "binomial")

glm.htn.choldrugs.na.int
summary (glm.htn.choldrugs.na.int) #No interaction

glm.htn.bpdrugs.na.int= glm (htn.fct.recoded~bmi.cat.recoded
                               +htn.bpdrugs.na.int,
                               data=Midterm.new, 
                               family = "binomial")

glm.htn.bpdrugs.na.int
summary (glm.htn.bpdrugs.na.int) #No interaction
#Note: No interaction was found and so, no subgroup analysis is required 

#Final model by age, sex, choldrugs.na, bpdrugs.na #with complete cases 
glm.htn.bmi.factors= glm (htn.fct.recoded~
                            bmi.cat.recoded+
                               age+
                               sex.fct+
                               choldrugs.na+
                               bpdrugs.na               
                             ,
                             data=Midterm.new, 
                             family = "binomial")

glm.htn.bmi.factors
summary (glm.htn.bmi.factors) 
or.glm.htn.bmi.factors=exp(cbind(coef(glm.htn.bmi.factors), confint(glm.htn.bmi.factors, level=0.95)))
or.glm.htn.bmi.factors

lipsitz.test(glm.htn.bmi.factors)


Midterm.new$htn.fct.recoded= as.factor (Midterm.new$htn.fct.recoded)
#Model by age, sex, without choldrugs.na, bpdrugs.na 
glm.htn.bmi.factors.miss= glm (htn.fct.recoded~
                            bmi.cat.recoded+
                            age+
                            sex.fct
                              
                          ,
                          data=Midterm.new, 
                          family = "binomial")

glm.htn.bmi.factors.miss
summary (glm.htn.bmi.factors.miss) 
or.glm.htn.bmi.factors.miss=
  exp(cbind(coef(glm.htn.bmi.factors.miss), 
                                      confint(glm.htn.bmi.factors.miss, level=0.95)))
or.glm.htn.bmi.factors.miss

#ROC for the model 
library(pROC)

#for ROC the data should be complete 

Midterm.new.na.omit = na.omit (Midterm.new)

glm.htn.bmi.factors.roc= glm (htn.fct.recoded~
                            bmi.cat.recoded+
                            age+
                            sex.fct+
                            choldrugs.na+
                            bpdrugs.na               
                          ,
                          data=Midterm.new.na.omit, 
                          family = "binomial")

glm.htn.bmi.factors.prob=predict(glm.htn.bmi.factors.roc,type=c("response"))


#roc object
glm.htn.bmi.factors.roc_object <- roc( Midterm.new.na.omit$htn.fct.recoded,  glm.htn.bmi.factors.prob)
auc( glm.htn.bmi.factors.roc_object )

#roc plot 
plot (glm.htn.bmi.factors.roc_object, col=rainbow(7), main="ROC curve for hypertention", print.auc=TRUE)


##################Model with the imputted data###############################

library (miceafter)

#Recoded the htn to 0 and 1 in the imputted data
Midterm.new.imp$data$htn.fct.recoded = 
  case_when(
    Midterm.new.imp$data$htn.fct == "1" ~ "1",
    Midterm.new.imp$data$htn.fct == "2" ~ "0", 
    Midterm.new.imp$data$htn.fct == "NA" ~ "NA"
  )

Midterm.new.imp$data$htn.fct.recoded= as.factor (Midterm.new.imp$data$htn.fct.recoded)
#Recoded the bmi to 0=healthy weight and 1=overweight/obese 
Midterm.new.imp$bmi.cat

Midterm.new.imp$data$bmi.cat.recoded = 
  case_when(
    Midterm.new.imp$data$bmi.cat == "1" ~ "1",
    Midterm.new.imp$data$bmi.cat == "2" ~ "1", 
    Midterm.new.imp$data$bmi.cat == "3" ~ "0", 
    Midterm.new.imp$data$bmi.cat == "4" ~ "0", 
    Midterm.new.imp$data$bmi.cat == "NA" ~ "NA"
  )

Midterm.new.imp$data$bmi.cat.recoded=as.factor (Midterm.new.imp$data$bmi.cat.recoded)
Midterm.new.imp$data$bmi.cat.recoded

typeof (Midterm.new.imp$data$htn.fct)
Midterm.new.imp$data$htn.fct= as.factor (Midterm.new.imp$data$htn.fct)
typeof (Midterm.new.imp$data$sex.fct)
Midterm.new.imp$data$sex.fct= as.factor (Midterm.new.imp$data$sex.fct)

typeof (Midterm.new.imp$data$htn.fct.recoded)

glm.htn.bmi.imp <- with(Midterm.new.imp, 
                      glm(htn.fct.recoded ~ 
                            bmi.cat.recoded +
                            age.int+
                            sex.fct
                            ,   family = "binomial"))
glm.htn.bmi.imp

glm.htn.bmi.imp.pool= pool(glm.htn.bmi.imp)
summary (glm.htn.bmi.imp.pool) # with the imputted data

glm.htn.bmi.imp.pool.or <- summary(pool(glm.htn.bmi.imp), conf.int = TRUE, exponentiate = TRUE)
glm.htn.bmi.imp.pool.or


###############################To answer to the research question 3, I have to develop ordinal logistic regression model######################################
#Develop a suitable model to determine the association between Obesity1 and other risk factors. 
#Interpret your results. 
#Do your findings fit in with the current literature?
  
# Load the required packages
library(MASS)
library(car)

#To get p and CI
require(Hmisc)
require(MASS) 

############ordinal without the imputted data###################

# Fit an ordinal logistic regression model
Midterm.new$bmi.cat

Midterm.new %>% 
  freq_table(bmi.cat.recoded)

Midterm.new %>% 
  freq_table(bmi.cat)

Midterm.new$bmi.cat.3= 
  case_when(
    Midterm.new$bmi.cat == "1" ~ "1",
    Midterm.new$bmi.cat == "2" ~ "1", 
    Midterm.new$bmi.cat == "3" ~ "2",
    Midterm.new$bmi.cat == "4" ~ "3",
    Midterm.new$bmi.cat == "NA" ~ "NA"
  )
Midterm.new$bmi.cat.3= as.factor (Midterm.new$bmi.cat.3)

#bmi.cat= 1 = obese, 2 = overweight (2 observation), 3= normal, 4= underweight 
#bmi.cat.3= 1 = obese/overweight, 2= normal, 3= underweight 

#age
ordinal.model.bmi.age <- polr(bmi.cat.3 ~ 
                                age, data = Midterm.new)
ordinal.model.bmi.age
summary (ordinal.model.bmi.age)
brant::brant(ordinal.model.bmi.age) #Assumtion was violeted 

ordinal.model.bmi.age.ctable <- coef(summary(ordinal.model.bmi.age))
ordinal.model.bmi.age.ctable.p <- pnorm(abs(ordinal.model.bmi.age.ctable[, "t value"]), lower.tail = FALSE) * 2
ordinal.model.bmi.age.ctable.p.ctable <- cbind(ordinal.model.bmi.age.ctable, "p value" = ordinal.model.bmi.age.ctable.p)
ordinal.model.bmi.age.ctable.p.ctable #p<0.05, we will include it in the model. 
ordinal.model.bmi.age.ctable.p.ctable.ci <- confint(ordinal.model.bmi.age, level=0.95)
ordinal.model.bmi.age.ctable.p.ctable.ci

ordinal.model.bmi.age.or= exp (cbind (coef (ordinal.model.bmi.age), confint(ordinal.model.bmi.age, level=0.95)))
ordinal.model.bmi.age.or

#sex
ordinal.model.bmi.sex <- polr(bmi.cat.3 ~ 
                        sex, data = Midterm.new)
ordinal.model.bmi.sex
summary (ordinal.model.bmi.sex)
brant::brant(ordinal.model.bmi.sex) #Assumtion was violeted 

#To get p
ordinal.model.bmi.sex.ctable <- coef(summary(ordinal.model.bmi.sex))
ordinal.model.bmi.sex.ctable.p <- pnorm(abs(ordinal.model.bmi.sex.ctable[, "t value"]), lower.tail = FALSE) * 2
ordinal.model.bmi.sex.ctable.p.ctable <- cbind(ordinal.model.bmi.sex.ctable, "p value" = ordinal.model.bmi.sex.ctable.p)
ordinal.model.bmi.sex.ctable.p.ctable #p<0.05, but we will include it 

#educ
ordinal.model.bmi.educ.fct <- polr(bmi.cat.3 ~ 
                                educ.fct, data = Midterm.new)
ordinal.model.bmi.educ.fct
summary (ordinal.model.bmi.educ.fct)
brant::brant(ordinal.model.bmi.educ.fct) #Assumtion was not violeted 

#To get p
ordinal.model.bmi.educ.fct.ctable <- coef(summary(ordinal.model.bmi.educ.fct))
ordinal.model.bmi.educ.fct.ctable.p <- pnorm(abs(ordinal.model.bmi.educ.fct.ctable[, "t value"]), lower.tail = FALSE) * 2
ordinal.model.bmi.educ.fct.ctable.p.ctable <- cbind(ordinal.model.bmi.educ.fct.ctable, "p value" = ordinal.model.bmi.educ.fct.ctable.p)
ordinal.model.bmi.educ.fct.ctable.p.ctable #p>0.05, but we will not include it in the model. 


#smoke
ordinal.model.bmi.smoke <- polr(bmi.cat.3 ~ 
                                  smoke, data = Midterm.new)
ordinal.model.bmi.smoke
summary (ordinal.model.bmi.smoke)
brant::brant(ordinal.model.bmi.smoke) #Assumption was violated 

#To get p
ordinal.model.bmi.smoke.ctable <- coef(summary(ordinal.model.bmi.smoke))
ordinal.model.bmi.smoke.ctable.p <- pnorm(abs(ordinal.model.bmi.smoke.ctable[, "t value"]), lower.tail = FALSE) * 2
ordinal.model.bmi.smoke.fct.ctable.p.ctable <- cbind(ordinal.model.bmi.smoke.ctable, "p value" = ordinal.model.bmi.smoke.ctable.p)
ordinal.model.bmi.smoke.fct.ctable.p.ctable #p<0.05,  we will include it in the model. 

typeof (Midterm.new$numcig.na)
Midterm.new$numcig.na= as.integer (Midterm.new$numcig.na)
#numcig.na
ordinal.model.bmi.numcig.na <- polr(bmi.cat.3 ~ 
                                      numcig.na, data = Midterm.new)
ordinal.model.bmi.numcig.na
summary (ordinal.model.bmi.numcig.na)
brant::brant(ordinal.model.bmi.numcig.na) #Assumption was violated 

#To get p
ordinal.model.bmi.numcig.na.ctable <- coef(summary(ordinal.model.bmi.numcig.na))
ordinal.model.bmi.numcig.na.ctable.p <- pnorm(abs(ordinal.model.bmi.numcig.na.ctable[, "t value"]), lower.tail = FALSE) * 2
ordinal.model.bmi.numcig.na.fct.ctable.p.ctable <- cbind(ordinal.model.bmi.numcig.na.ctable, "p value" = ordinal.model.bmi.numcig.na.ctable.p)
ordinal.model.bmi.numcig.na.fct.ctable.p.ctable #p<0.05, but we will include it in the model. 

#startage.na
ordinal.model.bmi.startage.na <- polr(bmi.cat.3 ~ 
                                        startage.na, data = Midterm.new)
ordinal.model.bmi.startage.na
summary (ordinal.model.bmi.startage.na)
brant::brant(ordinal.model.bmi.startage.na) #Assumption was violated 

#To get p
ordinal.model.bmi.startage.na.ctable <- coef(summary(ordinal.model.bmi.startage.na))
ordinal.model.bmi.startage.na.ctable.p <- pnorm(abs(ordinal.model.bmi.startage.na.ctable[, "t value"]), lower.tail = FALSE) * 2
ordinal.model.bmi.startage.na.fct.ctable.p.ctable <- cbind(ordinal.model.bmi.startage.na.ctable, "p value" = ordinal.model.bmi.startage.na.ctable.p)
ordinal.model.bmi.startage.na.fct.ctable.p.ctable #p<0.05, we will include it in the model. 

#htn.fct
ordinal.model.bmi.htn.fct <- polr(bmi.cat.3 ~ 
                                    htn.fct, data = Midterm.new)
ordinal.model.bmi.htn.fct
summary (ordinal.model.bmi.htn.fct)
brant::brant(ordinal.model.bmi.htn.fct) #Assumption was not violated 

#To get p
ordinal.model.bmi.htn.fct.ctable <- coef(summary(ordinal.model.bmi.htn.fct))
ordinal.model.bmi.htn.fct.ctable.p <- pnorm(abs(ordinal.model.bmi.htn.fct.ctable[, "t value"]), lower.tail = FALSE) * 2
ordinal.model.bmi.htn.fct.ctable.p.ctable <- cbind(ordinal.model.bmi.htn.fct.ctable, "p value" = ordinal.model.bmi.htn.fct.ctable.p)
ordinal.model.bmi.htn.fct.ctable.p.ctable #p<0.05, we will include it in the model. 


#tchol
ordinal.model.bmi.tchol <- polr(bmi.cat.3 ~ 
                                  tchol, data = Midterm.new)
ordinal.model.bmi.tchol
summary (ordinal.model.bmi.tchol)
brant::brant(ordinal.model.bmi.tchol) #Assumption was not violated 

#To get p
ordinal.model.bmi.tchol.ctable <- coef(summary(ordinal.model.bmi.tchol))
ordinal.model.bmi.tchol.ctable.p <- pnorm(abs(ordinal.model.bmi.tchol.ctable[, "t value"]), lower.tail = FALSE) * 2
ordinal.model.bmi.tchol.ctable.p.ctable <- cbind(ordinal.model.bmi.tchol.ctable, "p value" = ordinal.model.bmi.tchol.ctable.p)
ordinal.model.bmi.tchol.ctable.p.ctable #p<0.05, we will include it in the model. 

#bpdrugs.na
ordinal.model.bmi.bpdrugs.na <- polr(bmi.cat.3 ~ 
                                       bpdrugs.na, data = Midterm.new)
ordinal.model.bmi.bpdrugs.na
summary (ordinal.model.bmi.bpdrugs.na)
brant::brant(ordinal.model.bmi.bpdrugs.na) #Assumption was not violated 

#To get p
ordinal.model.bmi.bpdrugs.na.ctable <- coef(summary(ordinal.model.bmi.bpdrugs.na))
ordinal.model.bmi.bpdrugs.na.ctable.p <- pnorm(abs(ordinal.model.bmi.bpdrugs.na.ctable[, "t value"]), lower.tail = FALSE) * 2
ordinal.model.bmi.bpdrugs.na.ctable.p.ctable <- cbind(ordinal.model.bmi.bpdrugs.na.ctable, "p value" = ordinal.model.bmi.bpdrugs.na.ctable.p)
ordinal.model.bmi.bpdrugs.na.ctable.p.ctable #p<0.05, we will include it in the model. 

#choldrugs.na
ordinal.model.bmi.choldrugs.na<- polr(bmi.cat.3 ~ 
                                        choldrugs.na, data = Midterm.new)
ordinal.model.bmi.choldrugs.na
summary (ordinal.model.bmi.choldrugs.na)
brant::brant(ordinal.model.bmi.choldrugs.na) #Assumption was violated 

#To get p
ordinal.model.bmi.choldrugs.na.ctable <- coef(summary(ordinal.model.bmi.choldrugs.na))
ordinal.model.bmi.choldrugs.na.ctable.p <- pnorm(abs(ordinal.model.bmi.choldrugs.na.ctable[, "t value"]), lower.tail = FALSE) * 2
ordinal.model.bmi.choldrugs.na.ctable.p.ctable <- cbind(ordinal.model.bmi.choldrugs.na.ctable, "p value" = ordinal.model.bmi.choldrugs.na.ctable.p)
ordinal.model.bmi.choldrugs.na.ctable.p.ctable #p>0.05, we will not include it in the model. 

#notes: based on simple ordinal regression, we will include age, sex,  smoke,  numcig.na, htn.fct, tchol,  bpdrugs.na 

Midterm.new$bpdrugs.fct=as.factor (Midterm.new$bpdrugs.na)

#######################Final model for complete cases###########################
ordinal.model.bmi.final<- polr(bmi.cat.3 ~ 
                                 age+
                                 sex.fct+
                                 smoke.fct+
                                 numcig.na+
                                 startage.na+
                                 htn.fct+
                                 tchol+
                                 bpdrugs.fct, data = Midterm.new)
ordinal.model.bmi.final
summary (ordinal.model.bmi.final)

poTest (ordinal.model.bmi.final) # assumption was meet. 
brant::brant(ordinal.model.bmi.final) #Assumption was violated 


#To get p
ordinal.model.bmi.final.ctable <- coef(summary(ordinal.model.bmi.final))
ordinal.model.bmi.final.ctable.p <- pnorm(abs(ordinal.model.bmi.final.ctable[, "t value"]), lower.tail = FALSE) * 2
ordinal.model.bmi.final.ctable.p.ctable <- cbind(ordinal.model.bmi.final.ctable, "p value" = ordinal.model.bmi.final.ctable.p)
ordinal.model.bmi.final.ctable.p.ctable #p>0.05, we will not include it in the model.

ordinal.model.bmi.final.or= exp (cbind (coef (ordinal.model.bmi.final), confint(ordinal.model.bmi.final, level=0.95)))
ordinal.model.bmi.final.or


#######################Final model for without  bpdrugs.fct###########################
ordinal.model.bmi.final.miss<- polr(bmi.cat.recoded ~ 
                                 age+
                                 sex.fct+
                                smoke.fct+
                                 numcig.na+
                                  startage.na+
                                 htn.fct+
                                 tchol
                                 , data = Midterm.new)

ordinal.model.bmi.final.miss
summary (ordinal.model.bmi.final.miss)


library(ordinal)
lipsitz.test(ordinal.model.bmi.final.miss, g=10)

lipsitz.test(ordinal.model.bmi.final.miss)
hltest(ordinal.model.bmi.final.miss, verbose = TRUE)

poTest (ordinal.model.bmi.final.miss, na.rm=T) # assumption was meet.
?lipsitz.test

#To get p
ordinal.model.bmi.final.miss.ctable <- coef(summary(ordinal.model.bmi.final.miss))
ordinal.model.bmi.final.miss.ctable.p <- pnorm(abs(ordinal.model.bmi.final.miss.ctable[, "t value"]), lower.tail = FALSE) * 2
ordinal.model.bmi.final.miss.ctable.p.ctable <- cbind(ordinal.model.bmi.final.miss.ctable, "p value" = ordinal.model.bmi.final.miss.ctable.p)
ordinal.model.bmi.final.miss.ctable.p.ctable #p>0.05, we will not include it in the model.

ordinal.model.bmi.final.miss.or= exp (cbind (coef (ordinal.model.bmi.final.miss), confint(ordinal.model.bmi.final.miss, level=0.95)))
ordinal.model.bmi.final.miss.or


# Test the proportional odds assumption
library (poTest) #for propotional odds test 
poTest (ordinal.model.bmi.sex)

library(brant) #for propotional odds test 
#A low p-value in a Brant-Wald test is an indicator that the coefficient does not satisfy the proportional odds assumption.
brant::brant(ordinal.model.bmi.sex)


