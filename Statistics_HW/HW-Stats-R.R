#Libraries and dataset setup
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(survival)
library(car)
library(survminer)
library(corrgram)
library(Hmisc)

path <- "C:/Users/sofnl/Documents/BIU_DS/BIU_DS_Course/Stats"

ds <- read.csv(paste0(path,"/heart_failure_clinical_records_dataset.csv"))

View(ds)
summary(ds) #no missing data

#convert nominal binary variables into factors
ds$anaemia <- factor(ds$anaemia, levels=c(0,1), labels=c("No","Yes"))
ds$high_blood_pressure <- factor(ds$high_blood_pressure, levels=c(0,1), labels=c("No","Yes"))
ds$sex <- factor(ds$sex, levels=c(0,1), labels=c("F","M"))
ds$smoking <- factor(ds$smoking, levels=c(0,1), labels=c("No","Yes"))
ds$diabetes <- factor(ds$diabetes, levels=c(0,1), labels=c("No","Yes"))
ds$DEATH_EVENT <- factor(ds$DEATH_EVENT, levels=c(0,1), labels=c("No","Yes"))


#1. Create a linear regression for the “time” variable. Exclude the variable “DEATH_EVENT” from the analysis.

mod1 <- lm(time ~ age + anaemia + creatinine_phosphokinase + diabetes + 
             ejection_fraction + high_blood_pressure + platelets + 
             serum_creatinine + serum_sodium + sex + smoking, data = ds)
summary(mod1)

#checking for correlation between variables
#using spearman for non-normal distribution
#correlogram
corrgram(ds, order=NULL, lower.panel=panel.shade,
         upper.panel=NULL, text.panel=panel.txt,
         main=" Heart Failure Clinical Records")
#Table
# Correlations with significance levels
corr <- rcorr(as.matrix(ds %>% dplyr::select(where(is.numeric))), type="spearman") 
print(corr)
#visualizing rho
ggcorrplot(as.matrix(corr[[1]]), hc.order = TRUE, type = "lower",
           lab = TRUE)

#looks like there is no significant correlation between numeric X vars

#checking betas
vif(mod1)
#all the betas are between 1 and 2; no inflation due to correlations; 
#no need to remove variables 

#optimizing the model by excluding problematic variables using step
step(mod1)

#Start:  AIC=2586.69; End: AIC=2577.18
mod1 <- lm(formula = time ~ age + anaemia + high_blood_pressure + serum_creatinine, 
   data = ds)
summary(mod1)

#age  -1.1699, p = 0.00158 **
#anaemia             -17.6592, p = 0.04237 *  
#high_blood_pressure -28.5654, p = 0.00164 ** 
#serum_creatinine     -8.6844, p = 0.03908 *

#From beta/p-value analysis:
#Looks like High blood pressure, anaemia, and serum creatinine decrease drastically the time
#The age variable has similar effect (although by a smoler factor)


#Relative Risk assesment: (usually is not done for Linear Regression?)
round(exp(mod1$coefficients), 7)
#Analysis:age and serum creatinine project the most risk (would shorten the time)

#2.   Create a logistic regression model for the “DEATH_EVENT” variable. Exclude the “time” variable from the analysis
mod2 <- glm(DEATH_EVENT ~ age + anaemia + creatinine_phosphokinase + diabetes + 
              ejection_fraction + high_blood_pressure + platelets + serum_creatinine + 
              serum_sodium + sex + smoking, data=ds, family = "binomial")
summary(mod2)

#checking betas
vif(mod2)
#all the betas are between 1 and 2; no inflation due to correlations; 
#no need to remove variables 

#optimizing the model with step
step(mod2)

#Start:  AIC=318.28; End: AIC: 312.1
mod2 <- glm(formula = DEATH_EVENT ~ age + anaemia + creatinine_phosphokinase + 
              ejection_fraction + high_blood_pressure + serum_creatinine + 
              serum_sodium, family = "binomial", data = ds)

summary(mod2)

#age 1.0544554, p=3.37e-05 ***             
#anaemia 1.5386917, p=0.14824 
#creatinine_phosphokinase 1.0002674, p= 0.05713 .             
#ejection_fraction 0.9343526, p=3.12e-06 ***
#high_blood_pressure 1.5850741, p=0.12368
#serum_creatinine 1.9384750, p=0.00011 ***              
#serum_sodium 0.9447417, p= 0.08562 .

#Looks like age, ejection fraction, 
#and serum_creatinine increase the death risk the most  

#Relative Risk assesment:
exp(mod2$coefficients)
#Here serum_creatinine(1.94), anaemia(1.54), and high_blood_pressure(1.55) 
#seem to affect the death rate the most


#Death event shold be numeric for time-related analysis in Q3&Q4
ds$DEATH_EVENT <- as.numeric(ds$DEATH_EVENT)

#3.    Calculate the Kaplan-Meyer curve for the survival time, using the “time” and “DEATH_EVENT” variables.
#Create KM model
f1 <- survfit(Surv(time, DEATH_EVENT) ~ 1, data = ds)
summary(f1)
plot(f1)

#####Comparing Survival Times

#Comparing survival times between males and females
survdiff(Surv(time, DEATH_EVENT) ~ sex, data = ds)
###Observations:
####1. Identical results for logrank tests of two groups; 0.00397
####2. p value is high enough to accept the H0; p= 0.9 
####3. observed/expected values are almost the same;
####Thus I infer that there is no difference in survival between sexes
####and sex doesn't influence Death rate  

#Visualizing comparison for two groups:
fit <- survfit(Surv(time, DEATH_EVENT) ~ sex, data = ds)
ggsurvplot(fit,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"))

#Comparing survival times between smokers and non-smokers
survdiff(Surv(time, DEATH_EVENT) ~ smoking, data = ds)
###Observations:
####1. Identical results for logrank tests of two groups; 0.00204
####2. p value is high enough to accept the H0; p= 1 
####3. observed/expected values are almost the same;
####Thus I infer that there is no difference in survival between smokers/non-smokers
####and smoking doesn't influence Death rate 

#Visualizing comparison for two groups:
fit <- survfit(Surv(time, DEATH_EVENT) ~ smoking, data = ds)
ggsurvplot(fit,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"))


#Comparing survival times between diabetics and non-diabetics
survdiff(Surv(time, DEATH_EVENT) ~ diabetes, data = ds)
###Observations:
####1. Identical results for logrank tests of two groups; 0.0405
####2. p value is high enough to accept the H0; p= 0.8
####3. observed/expected values are almost the same;
####Thus I infer that there is no difference in survival between diabetics/non-diabetics
####and diabetes doesn't influence Death rate 

#Visualizing comparison for two groups:
fit <- survfit(Surv(time, DEATH_EVENT) ~ diabetes, data = ds)
ggsurvplot(fit,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"))



#4.   Create a Cox regression using the “time” and “DEATH_EVENT” as the outcome and the other variables as the independent predictors (X).
f2 <- coxph(Surv(time, DEATH_EVENT) ~ ., data = ds)
summary(f2)

#checking betas
vif(f2)
#looking to improve the model
step(f2)

#improved model (AIC 958.46 to AIC 951.83)
f2 <- coxph(formula = Surv(time, DEATH_EVENT) ~ age + anaemia + creatinine_phosphokinase + 
              ejection_fraction + high_blood_pressure + serum_creatinine + 
              serum_sodium, data = ds)

#exp are given in the summary
summary(f2)

#                              coef  exp(coef)   se(coef)      z   Pr(>|z|)    
# age                       4.357e-02  1.045e+00  8.831e-03  4.934 8.05e-07 ***
#  anaemia                   4.460e-01  1.562e+00  2.150e-01  2.074   0.0380 *  
#  creatinine_phosphokinase  2.101e-04  1.000e+00  9.825e-05  2.138   0.0325 *  
#  ejection_fraction        -4.747e-02  9.536e-01  1.027e-02 -4.621 3.82e-06 ***
#  high_blood_pressure       4.965e-01  1.643e+00  2.137e-01  2.324   0.0201 *  
#  serum_creatinine          3.139e-01  1.369e+00  6.895e-02  4.552 5.31e-06 ***
#  serum_sodium             -4.569e-02  9.553e-01  2.336e-02 -1.956   0.0505 .  

#Factors that increase the hazard of death the most are: age, anemia, 
#high blood pressure, and serum creatinine
