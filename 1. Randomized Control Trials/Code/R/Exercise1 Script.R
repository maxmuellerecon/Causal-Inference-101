########Exercise 1###############

'Randomized Control Tial'

######################################
'0. Install Packages
1. Get the data and clean the data
2. Descriptive statistics
3. Graphs
4. Means Tests (simple mean difference)
5. OLS - Regressions with and without Controls(Table 4)'
######################################


'ALAN B. KRUEGER (1999): STAR Experiment:
What is the effect of classize on student achievments?
In the late 1980s the Tennessee state legislature funded a four-year experiment to evaluate the effect of small class sizes on learning (Student Teacher Achievement Ratio or STAR).

Random assignment of over 7,000 students in 79 schools to 2 interventions: 
- Treatment group: a small class (size 13-17 pupils)
- Control group: a regular-size class (size 22-25 pupils)

Classroom teachers were also randomly assigned to the classes

The interventions were initiated as the students entered school in kindergarten and continued through third grade.'

  
#0. Install Packages#########################################################

install.packages("ggplot2") #For creating graphs
install.packages("haven") #For importing DTA Files
install.packages("dplyr") #For easier data manipulation
install.packages("foreign") #export data as DTA
install.packages("vtable") #nice summary tables
install.packages("outreg") #export regression outputs


library(ggplot2)
library(haven)
library(dplyr)
library(foreign)
library(vtable)
library(outreg)

#1. Get the data and clean the data##########################################
setwd("C:/Users/Max/Dropbox/Seminar SS22/Exercise/Exercise 1/R")
getwd()

STAR <- read_dta("STAR.dta")
View(STAR)

head(STAR)
summary(STAR)
st(STAR) #produces a neat overview table

#Name variables
STAR <- rename(STAR, treated = sck)
table(STAR$treated)

#Rename labels of Treatment Variable, factor is used to encode a vector as a factor (can easy manipulate lables)
STAR$treated <- factor(STAR$treated, labels = c("Class has 22-15 students" ,"Class has 13-17 students"))
table(STAR$treated)

#Save Dataset
require(foreign)
write.dta(STAR, "STAR_clean.dta")


#2. Descriptive statistics################################################
#How to read help files
?summarize

#Treatment
str(STAR$treated)
table(STAR$treated)

#Outcomes
STAR %>% filter(treated == "Class has 22-15 students") %>% summarise(mean = mean(tscorek),sd = sd(tscorek), n = n())
STAR %>% filter(treated == "Class has 13-17 students") %>% summarise(mean = mean(tscorek),sd = sd(tscorek), n = n())

#summarize by Treated Condition
STAR %>%
  group_by(treated) %>%
  summarise(mean = mean(tscorek),sd = sd(tscorek), n = n())

#Controls
st(STAR, vars = c('boy','freelunk','totexpk'))

#summarize by Treated Condition
STAR %>%
  group_by(treated) %>%
  summarise(meanLunch = mean(freelunk), sdLunch = sd(freelunk), meanBoy = mean(boy), sdBoy = sd(boy), meanExp = mean(totexpk), sdExp = sd(totexpk), n = n())


#3. Graphs#####################################################################

# Histogram of experience
hist(STAR$totexpk,
     main="Total years of experience",
     xlab="Years of experience",
     xlim=c(0,30),
     col="darkmagenta",
     freq=FALSE
)

?hist


#Histogram by group
ggplot(STAR, aes(x = totexpk)) +
  geom_histogram(aes(color = treated), fill = "white",alpha = 0.5,
                 position = "identity", bins = 30) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) + facet_wrap(~boy)


#4. Means Tests###############################################################
#Test that the mean of variables is equal between two groups

# T Test of the difference of treated and non treated: outcome
lapply(split(STAR, factor(STAR$treated)), function(x)t.test(data=STAR, tscorek ~ treated, paired=FALSE))


# T Test of the difference of treated and non treated: controls
lapply(split(STAR, factor(STAR$treated)), function(x)t.test(data=STAR, boy ~ treated, paired=FALSE))
lapply(split(STAR, factor(STAR$treated)), function(x)t.test(data=STAR, freelunk ~ treated, paired=FALSE))
lapply(split(STAR, factor(STAR$treated)), function(x)t.test(data=STAR, totexpk ~ treated, paired=FALSE))




#5.Regressions##########################################################

regression1 <- lm(tscorek ~ treated, data = STAR)
summary(regression1)

REG1 <- outreg(regression1, digits = 3L, alpha = c(0.1, 0.05, 0.01),
       bracket = c("se"), starred = c("coef"), robust = FALSE, small = TRUE,
       constlast = FALSE, norepeat = TRUE, displayed = list())

View(REG1)  #Regression output is now a data frame, that is easily exportable


regression2 <- lm(tscorek ~ treated + boy + freelunk + totexpk, data = STAR)
summary(regression2)


