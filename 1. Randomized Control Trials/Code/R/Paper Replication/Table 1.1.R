#Install all packages needed for this Exercise
install.packages("tidyverse")
install.packages("magrittr")
install.packages("haven")

#Put the packages in the Library (activate them)
library(tidyverse)
library(magrittr)
library(haven)

#import data from a file in stata format
#check that you use the right function to import your data (function depends on the data format)
NHIS2009_clean <- read_dta("C:/Users/Coci/Desktop/Arbeit/Felfe Projekt/4_R Metrics/R Code/Data/NHIS2009_clean.dta")
View(NHIS2009_clean)

#rename dataset
#also possible with <- instead of =
data=NHIS2009_clean

#watch dataset
data

#check variables in data
names(data)

#check summary of your data
summary(data)

#check for missing value (if there is missing value, R returns TRUE)
is.na(data)

#Code structure due to the margrittr package (uses %>% as Forward-Pipe-Operator)
#the Pipe Operator can be seen as "then" in order to chain functions together
data <- data %>%
  
#Remove missing values
  #kick out all cases for which annual basic weight, married adult = 0
  filter(marradult, perweight != 0) %>%
  #sort by sequential serial number (number for households)
  group_by(serial) %>%
  #compute new variable hi_hsb and exclude missing values with the na.rm=TRUE
  mutate(hi_hsb = mean(hi_hsb1, na.rm = TRUE)) %>%
  #kick out cases with missing variables 
  filter(!is.na(hi_hsb), !is.na(hi)) %>%
  #create new variable female as sum of females
  mutate(female = sum(fml)) %>%
  #filter for females (==1 as being female)
  filter(female == 1) %>%
  #drop variabble female
  select(-female)

#remove single householts, only use married adults of age 26-59
data <- data %>%
  #select households with married adults of a certain age
  filter(between(age, 26, 59),
         marradult, adltempl >= 1)

#only single family household
data <- data %>%
  #exclude and drop households with multiple families via the ungroup function
  group_by(serial) %>%
  filter(length(serial) > 1L) %>%
  ungroup()

#compute table of wives and husbands with weight in order to get sample size
#group by gender
data %>%
  group_by(fml) %>%
  
#normalize person weights to match number of observations in each group
  mutate(perweight = perweight / sum(perweight) * n()) %>%
  #split sample size in order to get size for health insurance and non health insurance
  group_by(fml, hi) %>%
  #sum up in order to get sample size
  summarise(n_wt = sum(perweight)) %>%
  #split sample size in order to get female vs non-female
  group_by(fml) %>%
  #create properties/percetanges as a new variable
  mutate(prop = n_wt / sum(n_wt))

#compare men and women
#compute demographics table
#create vector containing the variables of interest
varlist <- c("hlth", "nwhite", "age", "yedu", "famsize", "empl", "inc")
data_diff <- data %>%
  
# rlang::set_attrs with NULL removes attributes from columns this avoids a warning from gather about differing attributes
  #returns dataframes created by column-binding
  map_dfc(~ rlang::set_attrs(.x, NULL)) %>%
  select(fml, hi, one_of(varlist)) %>%
  #gather creates data paires for a grid, the - puts variables in the first columns
  gather(variable, value, -fml, -hi) %>%
  group_by(fml, hi, variable) %>%
  #summarise the calculated means and sd
  summarise(mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE)) %>%
  #create the data pairs in order to fill the cells
  #use the stat function? in order to bind the mean sd values
  gather(stat, value, -fml, -hi, -variable) %>%
  #unites multiple columns
  unite(stat_hi, stat, hi) %>%
  #divide the columns again to use mean_0 and mean_1
  spread(stat_hi, value) %>%
  #compute new variable for differences in mean and sd
  mutate(diff = mean_1 - mean_0)
#compute HTML compatible table with 3 digits
knitr::kable(data_diff, digits = 3)
