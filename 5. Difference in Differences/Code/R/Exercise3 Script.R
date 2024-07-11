########Exercise 3###############

'Difference in Difference Regression'


##################################################################
'0. Install/Load Packages
1. Get the data
2. Replication of the summary statistics
3. Graph (Figure 1)
4. Diff-in-Diff (Table 3) (simple mean difference)
5. OLS - Regressions with either Dummies for the State (Table 4)'
#################################################################

'Card & Krueger 1994

On April 1, 1992, New Jerseys minimum wage rose from
$4.25 to $5.05 per hour. To evaluate the impact of the law
we surveyed 410 fast-food restaurants in New Jersey and
Eastern Pennsylvania before and after the rise. Comparisons
of employment growth at stores in New Jersey and
Pennsylvania (where the minimum wage was constant)
provide simple estimates of the effect of the higher
minimum wage. We also compare employment changes at
stores in New Jersey that were initially paying high wages
(above $5) to the changes at lower-wage stores. We find no
indication that the rise in the minimum wage reduced
employment'


#0. Install/Load Packages#########################################################

install.packages("haven") #For importing DTA Files
install.packages("dplyr") #For easier data manipulation
install.packages("vtable") #nice summary tables
install.packages("writexl") #for exoprting Excel files
install.packages("ggplot2") #For creating graphs
install.packages("misty") #For CI´s

library(haven)
library(dplyr)
library(vtable)
library(writexl)
library(ggplot2)
library(misty)


#1. Get the data#########################################################
setwd("C:/Users/Max/Dropbox/Seminar SS22/Exercise/Exercise 3/R")
getwd()

Card <- read_dta("card_krueger.dta")
View(Card)
nrow(Card)
#Note: 410 restaurants, which corresponds to 473 contacted ones, of which 410 replied in wave one and only 409 in wave 2!

#Relabel Status variable
table(Card$status2)
Card$status2 <- factor(Card$status2 , labels = c("Refused", "Interviewed", "Under rennovation", "Closed", "Temporary closed: Construcion", "Temporary closed: Fire"))
table(Card$status2)

#Replicate Second Part of Table 1 from the paper
Table1 <- table(Card$status2, Card$state)
colnames(Table1) = c("PA", "NJ")
Table1

#Relabel Chain variable and check how many restaurants of each chain were involved
table(Card$chain)
Card$chain <- factor(Card$chain, labels = c("Burger King", "KFC", "Roy Rogers", "Wendy's"))
table(Card$chain)
Card %>%
  group_by(state) %>%
  count(chain)





#2. Replication of the summary statistics#########################################
#Overall summary statistics
st(Card, vars = c('fte1','fte2','wage_st', 'wage_st2'))

Pennsylvania <- subset(Card, state ==0)
st(Pennsylvania , vars = c('fte1', 'percentft1', 'wage_st', 'fte2', 'percentft2' ,'wage_st2'))

NewJersey <- subset(Card, state ==1)
st(NewJersey, vars = c('fte1', 'percentft1', 'wage_st', 'fte2', 'percentft2' ,'wage_st2'))

#T-test full time equivalent employment and store mean of both populations and the t-Statistic in both waves
T1 <- t.test(fte1 ~ state, data = Card, paired = FALSE)
fte1_PA <- T1$estimate[1]
fte1_NJ <- T1$estimate[2]
fte1_t <- T1$statistic[1]

T2 <- t.test(fte2 ~ state, data = Card, paired = FALSE)
fte2_PA <- T2$estimate[1]
fte2_NJ <- T2$estimate[2]
fte2_t <- T2$statistic[1]

#Do the same for starting wage
T3 <- t.test(wage_st ~ state, data = Card, paired = FALSE)
wage_st1_PA <- T3$estimate[1]
wage_st1_NJ <- T3$estimate[2]
wage_st1_t <- T3$statistic[1]

T4 <- t.test(wage_st2 ~ state, data = Card, paired = FALSE)
wage_st2_PA <- T4$estimate[1]
wage_st2_NJ<- T4$estimate[2]
wage_st2_t <- T4$statistic[1]

#Create table 2 and export it as xlsx 
Table2 <- matrix(c(fte1_PA, fte1_NJ, fte1_t, wage_st1_PA, wage_st1_NJ, wage_st1_t, fte2_PA, fte2_NJ, fte2_t, wage_st2_PA, wage_st2_NJ, wage_st2_t), nrow=4, ncol=3, byrow=TRUE)
colnames(Table2) <- c("PA", "NJ", "t-test")
rownames(Table2) <- c("Fulltime Employment Wave1", "Starting Wages Wave1", "Fulltime Employment Wave 2", "Starting Wages Wave 2")
View(Table2)

Table2 <- as.data.frame(Table2)
write_xlsx(Table2,"Table2.xlsx")





#3. Graphs################################################################
#Create Figure 1 in the paper
ggplot(Card, aes(x = wage_st)) +
  geom_histogram(aes(x=wage_st, y=..density..),alpha = 0.5,
                 position = "identity", bins = 30) + ggtitle("DISTRIBUTION OF STARTING WAGE RATES") + facet_wrap(~state)



ggplot(Card, aes(x = wage_st2)) +
  geom_histogram(aes(x=wage_st2, y=..density..),alpha = 0.5,
                 position = "identity", bins = 30) + ggtitle("DISTRIBUTION OF STARTING WAGE RATES") + facet_wrap(~state)




#4. Diff-in-Diff (Table 3) (simple mean difference)s############################
#Inter state differences 
#Save means, standard deviations and differences (Ttest with assumption of unequal variances), R reports SD, we want SE --> /sqrt(n)
#Wave 1

CI1 <- ci.mean(Pennsylvania$fte1, conf.level = 0.95)
m_fte1_PA <- CI1$result[5]
se_fte1_PA <- CI1$result[6]/sqrt(CI1$result[2])

CI2 <- ci.mean(NewJersey$fte1, conf.level = 0.95)
m_fte1_NJ <- CI2$result[5]
se_fte1_NJ <- CI2$result[6]/sqrt(CI2$result[2])

T1 <- t.test(fte1 ~ state, data = Card, paired = FALSE)
d_fte1 <- m_fte1_NJ-m_fte1_PA
se_fte1 <- T1$stderr

#wave2
CI3 <- ci.mean(Pennsylvania$fte2, conf.level = 0.95)
m_fte2_PA <- CI3$result[5]
se_fte2_PA <- CI3$result[6]/sqrt(CI3$result[2])

CI4 <- ci.mean(NewJersey$fte2, conf.level = 0.95)
m_fte2_NJ <- CI4$result[5]
se_fte2_NJ <- CI4$result[6]/sqrt(CI4$result[2])

T2 <- t.test(fte2 ~ state, data = Card, paired = FALSE)
d_fte2 <- m_fte2_NJ-m_fte2_PA
se_fte2 <- T2$stderr

#Difference within states between years
#Save means, standard deviations and differences (Ttest with assumption of unequal variances)
#PA
T5 <- t.test(Pennsylvania$fte1, Pennsylvania$fte2)
d_fte_PA <- m_fte2_PA-m_fte1_PA
se_fte_PA <- T5$stderr

#NJ
T6 <- t.test(NewJersey$fte1, NewJersey$fte2)
d_fte_NJ <- m_fte2_NJ-m_fte1_NJ
se_fte_NJ <- T6$stderr

#DiD
dd <- d_fte_NJ-d_fte_PA
se_dd <- 1.36

#Create table 3 and export it to Excel as CSV, Hint: Need to format list as character vector first --> use apply
Table3 <- matrix(c(m_fte1_PA, m_fte1_NJ, d_fte1, se_fte1_PA, se_fte1_NJ, se_fte1, m_fte2_PA, m_fte2_NJ, d_fte2, se_fte2_PA, se_fte2_NJ, se_fte2, d_fte_PA, d_fte_NJ, dd, se_fte_PA, se_fte_NJ, se_dd), nrow=6, ncol=3, byrow=TRUE)
colnames(Table3) <- c("PA", "NJ", "State difference")
rownames(Table3) <- c("Wave 1- Mean", "Wave 1- Standard error", "Wave 2 - Mean", "Wave 2 - Standard error", "Period difference - Mean", "Period difference - SE")
View(Table3)

Table3 <- apply(Table3,2,as.character)
write.csv(Table3,"Table3.csv")



#5. OLS - Regressions with either Dummies for the State (Table 4)################
CardLong <- read_dta("cardkrueger_long.dta")
View(CardLong)

#Replace periodic values to perform diff-in-diff
CardLong$period[CardLong$period == 1] <- 0 
CardLong$period[CardLong$period == 2] <- 1 

#Generate interaction term for diff in diff design
CardLong$reform <- CardLong$state*CardLong$period

#Regression Without controls
regression1 <- lm(fte ~ state + period + reform, data = CardLong)
summary(regression1)
#Approximately the same as our mean difference table

#Regression With controls
regression2 <- lm(fte ~ state + period + reform + co_owned, data = CardLong)
summary(regression2)


