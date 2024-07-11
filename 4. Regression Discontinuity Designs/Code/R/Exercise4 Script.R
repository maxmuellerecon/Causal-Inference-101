########Exercise 4###############

'Regression Discontinuity Design'


#################################################################
'0. Install/Load Packages
1. Data manipulation
2. Create Figures
3. MLDA Regression Discontinuity'
##################################################################
  

'Carpenter & Dobkin, 2009

RDD: Assume that units on different sides of the discontinuity are
similar. Their treatment status differs only because of the
institutional setup, and therefore differences in outcomes can
be attributed to the different treatment status.

Lets take a look at one of the most important thresholds for
young americans: If they are over 21, they can drink legally.
This experiment emerges from the fact that a small change in
age (measured in months or even days) generates a big
change in legal access.
So we could assume that the individuals only differ in their
legal drinking access one day before and one day after turning
21.
We also know that Mortality risk shoots up on and immediately following a
twenty-fist birthday.
Causal question here is the effect of legal access to alcohol on
death rates.
Sharp RD Design: Treatment clearly switches on and off around the cutoff.
Fuzzy RD Design: Probability of treatment jumps at a cut-off --> Estimation with IV'
  

#0. Install/Load Packages#########################################################
install.packages("expss")
install.packages("tidyverse") 
install.packages("haven")
install.packages("rlang") 
install.packages("broom") 
install.packages("lmtest")
install.packages("sandwich")

library("expss")
library("tidyverse")
library("haven")
library("rlang")
library("broom")
library("lmtest")
library("sandwich")

#1.  Data manipulation####################################################
#Load MLDA data
setwd("C:/Users/Max/Dropbox/Seminar SS22/Exercise/Exercise 4/R")
getwd()

mlda <- read_dta("mlda.dta")
head(mlda)

#Label two variables
mlda <- apply_labels(mlda,
                      all = "Number of all death causes",
                     internal = "Number of internal death causes")


#Add an indicator variable for individuals over 21 years of age and manipulate age variable
mlda <- mutate(mlda,
               age = agecell - 21,
               over21 = as.integer(agecell >= 21))

#Add a variable for other causes of death
mlda <- mutate(mlda, ext_oth = external - homicide - suicide - mva)



#2. Create Figures ####################################################

#Investigate Relationship via a scatterplot
ggplot(mlda, aes(x=agecell, y=all)) + 
  geom_point()

#Check discontinuity linear with graph without CI
ggplot(mlda, aes(x=agecell, y=all)) + 
  geom_point() +
  geom_smooth(mapping = aes(group = over21), se = FALSE, method = "lm", 
              formula = y ~ x, color = "red")

#Check discontinuity linear with graph with CI
ggplot(mlda, aes(x=agecell, y=all)) + 
  geom_point() +
  geom_smooth(mapping = aes(group = over21), se = TRUE, method = "lm", 
              formula = y ~ x, color = "red") 

#Check discontinuity quadratic
ggplot(mlda, aes(x=agecell, y=all)) + 
  geom_point() +
  geom_smooth(mapping = aes(group = over21), se = FALSE, method = "lm", 
              formula = y ~ x, color = "black") +
  geom_smooth(mapping = aes(group = over21), se = FALSE, method = "lm",
              formula = y ~ poly(x, 2), color = "red")

#Create a list of variables for "all causes", "motor vehicle accidents", and "internal causes"
#All variable = all causes for deaths (measured as deaths per 100,000 persons per 30-day interval counting from the twenty-first birthday)
varlist <- c("all" = "All Causes",
             "mva" = "Motor Vehicle Accidents",
             "internal" = "Internal Causes")

#For "all causes", "motor vehicle accidents", and "internal causes" deaths plot the linear and quadratic trends on each side of age 21.
#Save the graph
jpeg(file = "Output.jpeg",   # The directory you want to save the file in
     width = 890, # The width of the plot in inches
     height = 770)

mlda %>%
  select(agecell, over21, one_of(names(varlist))) %>%
  gather(response, value, -agecell, -over21, na.rm = TRUE) %>%  #creates the key (named response) and the value columns and specify where to gather from
  mutate(response = dplyr::recode(response, !!!as.list(varlist))) %>%  #response column includes all death causes, mva and internal
  ggplot(aes(x = agecell, y = value)) +
  geom_point() +
  geom_smooth(mapping = aes(group = over21), se = FALSE, method = "lm", #Aesthetic mappings --> Here grouped by over21, no CI, by a linear model
              formula = y ~ poly(x, 2), color = "red") +
  geom_smooth(mapping = aes(group = over21), se = FALSE, method = "lm",
              formula = y ~ x, color = "black") +
  facet_grid(response ~ ., scales = "free_y") +         #forms a matrix of panels defined by row and column faceting variables. Scales are allowed to vary across rows
  labs(y = "Mortality rate (per 100,000)", x = "Age")

dev.off()

#3. MLDA Regression Discontinuity####################################################

#Define a function to run four regressions for a given response variable, e.g. all causes
'Use smaller bandwith:
For the small set of points close to the boundary, nonlinear trends need not concern us at
all. This suggests an approach that compares averages in a narrow
window just to the left and just to the right of the cutoff. A drawback
here is that if the window is very narrow, there are few observations left,
meaning the resulting estimates are likely to be too imprecise to be
useful. Still, we should be able to trade the reduction in bias near the
boundary against the increased variance suffered by throwing data
away, generating some kind of optimal window size.
--> We choose bandwith of 2 years'

#Controls? Treatment status (over21) is determined solely by age. Assuming that the effect of 
#age on death rates is captured by a linear/quadratic function, we can be sure that no OVB afflicts this short regression


#All Death Causes with different models#################################################
#All between 19-22 - linear
reg1 <- lm(all ~ age + over21, data = mlda)
summary(reg1)

#All between 19-22 - Quadratic
reg2 <- lm(all ~ poly(age, 2, raw = TRUE) * over21, data = mlda)
summary(reg2)

#All between 20-21 - Linear
reg3 <- lm(all ~ age + over21, data = filter(mlda, agecell >= 20, agecell <= 22))
summary(reg3)

#All between 20-21 - Quadratic
reg4 <- lm(all ~ poly(age, 2, raw = TRUE) * over21, data = filter(mlda, agecell >= 20, agecell <= 22))
summary(reg4)

#Create first column of table with coefficients and R squared
One <- reg1$coefficients[3]
Two <- reg2$coefficients[4]
Three <- reg3$coefficients[3]
Four <- reg4$coefficients[4]

Five <- summary(reg1)$r.squared
Six <- summary(reg2)$r.squared
Seven <- summary(reg3)$r.squared
Eight <- summary(reg4)$r.squared


Table1 <- cbind(One, Two, Three, Four)
Table2 <- cbind(Five, Six, Seven, Eight)
Output <- rbind(Table1, Table2)
colnames(Output) <- c("Age 19-22", "Age 19-22 Extended", "Age 20-21", "Age 20-21 Extended")
rownames(Output) <- c("All deaths", "R-Squared")


#Heterogeneity Regressions#################################################
reg5 <- lm(internal ~ poly(age, 2, raw = TRUE) * over21, data = mlda)
summary(reg5)
#No effect on internal causes.

reg6 <- lm(mva ~ poly(age, 2, raw = TRUE) * over21, data = mlda)
summary(reg6)
#Significant uptick in mva deaths.

