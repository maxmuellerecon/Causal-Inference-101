########Exercise 2###############

'INSTRUMENTAL VARIABLE REGRESSION'


#########################################################
'0. Install/Load Packages
1. First stage and reduced form
2. OLS & 2 SLS
3. Tests
4. Multiple instruments --> Precision'
#########################################################
  
'JOSHUA D. ANGRIST AND ALAN B. KRUEGER (1991)

Compulsory schooling laws:
Quarter of birth as an instrumental variable of years of education
Students born earlier in the year are old for their grade and thus have completed 
less years of schooling when reaching the compulsory age for schooling

Question: Does this cause earlier dropouts? Therefore less education and less wages?
  
Assumption: Quarter of birth is unrelated to innate ability, motivation or family background

Remember: Z: Quarter of birth, X: Education, Y: Wages'


#0. Install/Load Packages#########################################################

install.packages("fastDummies") #For creating graphs
install.packages("haven") #For importing DTA Files
install.packages("dplyr") #For easier data manipulation
install.packages("AER") #For IV Regression
install.packages("stargazer") #For IV Regression
install.packages("ggplot2") #For neat graphs

library(fastDummies)
library(haven)
library(dplyr)
library(AER)
library(stargazer)
library(ggplot2)

#Get the data

setwd("INSERT LINK TO YOUR DIRECTORY")
School <- read_dta("INSERT DTA-FILE")
head(School)


# Lets look at some Graphs first! :)

School_modified <- School %>% 
  mutate(AGE = as.character(AGE)) %>%
  mutate(MARRIED = as.character(MARRIED))

School_modified %>% 
  ggplot(aes(x=AGE, y=LWKLYWGE, fill=MARRIED)) +
  geom_boxplot()

School_modified %>% 
  ggplot(aes(x=AGE, y=EDUC, fill=MARRIED)) +
  geom_boxplot()

School_modified %>%
  ggplot(aes(AGE,EDUC, fill = LWKLYWGE)) +
  geom_tile()



#1. First stage and reduced form   ##########################################

##############i. First stage: Effect of Z on X --> Relevance#####################

#Create dummy variables for quarter of birth
table(School$QOB)
School <- fastDummies::dummy_cols(School, select_columns = "QOB")

#Regress QOB and Years on Education
regression1 <- lm(EDUC ~ QOB_1 + QOB_2 + QOB_3 + YR20 + YR21 + YR22 + YR23 + YR24 + YR25 + YR26 + YR27 + YR28, data = School)
summary(regression1)

#Create new variable p_educ with the option to contain the linear prediction
School$p_educ <- predict(regression1)

#Create new variable with the birthyear and the birth quarter attached to it
School$YQOB <- School$YOB*10+ School$QOB	

School <- School[with(School, order(YQOB)),]

#For every YQOB we attach the predicted mean value of education 
School <- School %>% group_by(YQOB) %>% mutate(m_p_educ=mean(p_educ))

#-->e.g. if you are born in the first quarter of 1920 --> prediced value is 11.12483

#Model graph of Year/Quarter of Birth and predicted education	
?plot
plot(School$YQOB, School$m_p_educ, type = "l") 

#################ii. REDUCED FORM: Effect of Z on Y#########################

#Again: Regress, predict values (here wages) and then attached predicted values to new column by YQOB
regression2 <- lm(LWKLYWGE ~ QOB_1 + QOB_2 + QOB_3 + YR20 + YR21 + YR22 + YR23 + YR24 + YR25 + YR26 + YR27 + YR28, data = School)
summary(regression2)
School$p_lwage <- predict(regression2)
School <- School %>% group_by(YQOB) %>% mutate(m_p_lwage=mean(p_lwage))

#Model graph of Year/Quarter of Birth and predicted wages

### To-Do: plot predicted wage and the combined variable (yearofbirth+quarterofbirth) as line graph ###


#Let's look at both graphs simultaneously
par(mfrow = c(2,1))
plot(School$YQOB, School$m_p_educ, type = "l") 
### To-Do: Insert plot from previous To-Do ### 


#2. OLS & 2 SLS   ##########################################

#i. OLS
### To-Do: Regress WAGE on EDUCATION and YEARS, save it as regression3 ###
summary(regression3)

#ii. 2SLS with instrumented education by QOB_1 and store coefficient of Education as iv1, Hint:  exogenous regressors that are not instruments (controls) must appear both before and after the vertical line.
ivreg1 <- ivreg(LWKLYWGE ~ YR20 + YR21 + YR22 + YR23 + YR24 + YR25 + YR26 + YR27 + YR28 + EDUC | QOB_1 + YR20 + YR21 + YR22 + YR23 + YR24 + YR25 + YR26 + YR27 + YR28, data = School)
summary(ivreg1)

iv1 <- ivreg1$coefficients[11]


#First stage --> Store coefficient
### To-Do: Regress EDUCATION on QUARTEROFBIRTH_1 and YEARS, save it as FirstStage ###
summary(FirstStage)

### To-Do: Save coeffiecient of QUATEROFBIRTH_1 as fs###

School$educ_pred <- FirstStage$fitted.values

#Reduced Form

### To-Do: Regress WAGE on QUARTEROFBIRTH_1 and YEARS, save it as ReducedForm ###
summary(ReducedForm)

### To-Do: Save coeffiecient of QUATEROFBIRTH_1 as rf###

#Second stage --> Store coefficient
### To-Do: Regress WAGE on PREDICTED EDUCATION and YEARS, save it as SecondStage ###

summary(SecondStage)
### To-Do: Save coeffiecient of QUATEROFBIRTH_1 as fs###

iv3 <- rf/fs

#IV estimator is exactly the same as = reduced form/first stage and Second Stage with fitted values

cat(iv1, iv3, iv2)



#3. Tests  ##########################################
#i. weak instruments --> Is instrument relevant?
#Test F-statistic of the first stage regression of the instrumental variable
summary(ivreg1, vcov = sandwich, diagnostics = TRUE)
#--> Critical Value: 10 --> here: 60 --> Not a weak instrument
#--> Sargan is NA --> Not enough instruments

#ii. endogeneity --> Is instrument exogeneous? --> corr(Z,u)=0?
#J Test of overidentification --> need more Z?s than X?s
ivreg2 <- ivreg(LWKLYWGE ~ YR20 + YR21 + YR22 + YR23 + YR24 + YR25 + YR26 + YR27 + YR28 + EDUC | QOB_1 + QOB_2 + QOB_3 + YR20 + YR21 + YR22 + YR23 + YR24 + YR25 + YR26 + YR27 + YR28, data = School)
summary(ivreg2, vcov = sandwich, diagnostics = TRUE)

#VCOV = Cluster-robust variance-covariance matrix
#H0: the IVs are exogenous; H1: at least one of the IVs is not exogenous
#If some IVs are exogenous and others are endogenous, J-statistic will be large, and H0 will be rejected.
#Critical value chi-squared at q restrictions (q= # of IVs  #of endogenous variables - here 3 to 1: --> here 1% 9.21, 5% 5.99, 10% 4.61)
#Our value is 2.31
#J Statistic is lower than critical value --> no rejection --> instruments are exogenous


#4. Multiple instruments --> Precision   ####################################

#Generate Dummy not for each year, but each quarter
#New Dataframe
Short <- subset(School, select=c("LWKLYWGE", "EDUC", "YOB", "QOB", "YR20", "YR21", "YR22", "YR23", "YR24", "YR25", "YR26", "YR27", "YR28"))

Short$QYOB <- Short$YOB*10+Short$QOB

table(Short$QYOB)
Short <- fastDummies::dummy_cols(Short, select_columns = "QYOB")


#Execute more precise regression with more instruments
ivreg2 <- ivreg(LWKLYWGE ~ YR20 + YR21 + YR22 + YR23 + YR24 + YR25 + YR26 + YR27 + YR28 + EDUC | QYOB_19201 + QYOB_19202 + QYOB_19203 + QYOB_19211 + QYOB_19212 + QYOB_19213 + QYOB_19221 + QYOB_19222 + QYOB_19223 + QYOB_19231 + QYOB_19232 + QYOB_19233 + QYOB_19241 + QYOB_19242 + QYOB_19243 + QYOB_19251 + QYOB_19252 + QYOB_19253 + QYOB_19261 + QYOB_19262 + QYOB_19263 + QYOB_19271 + QYOB_19272 + QYOB_19273 + QYOB_19281 + QYOB_19282 + QYOB_19283 + QYOB_19291 + QYOB_19292 + QYOB_19293 + YR20 + YR21 + YR22 + YR23 + YR24 + YR25 + YR26 + YR27 + YR28, data = Short)
summary(ivreg2, vcov = sandwich, diagnostics = TRUE)

#More precise estimates --> Higher t Statistic






