*************Exercise 1******************************

*Randomized Control Trial

clear
clear matrix
set memory 400m  //How much memory will be allocated to STATA --> here 400 Megabytes
set more off // For full output, without "more" message
capture log close //close all log files


log using "C:\Users\Max\Dropbox\Seminar SS22\Exercise\Exercise 1\STATA\Exercise1.log", replace


/********************************************************************************
0. Install Package
1. Get the data and clean the data
2. Descriptive statistics
3. Graphs
4. Means Tests (simple mean difference)
5. OLS - Regressions with and without Controls 
********************************************************************************/

/*ALAN B. KRUEGER (1999): STAR Experiment:
What is the effect of classize on student achievments?
In the late 1980s the Tennessee state legislature funded a four-year experiment to evaluate the effect of small class sizes on learning (Student Teacher Achievement Ratio or STAR).

Random assignment of over 7,000 students in 79 schools to 2 interventions: 
- Treatment group: a small class (size 13-17 pupils)
- Control group: a regular-size class (size 22-25 pupils)

Classroom teachers were also randomly assigned to the classes

The interventions were initiated as the students entered school in kindergarten and continued through third grade.
*/

********************************************************************************
***0. Install Package

ssc install outreg2


********************************************************************************
***1. Get the data 

cd "C:\Users\Max\Dropbox\Seminar SS22\Exercise\Exercise 1\STATA"
use STAR
describe

tab boy

***Name variables
rename sck treated
label var treated "Treated class: max. 17 students"
tab treated

#delimit ;  //set delimiter for multi column lines
label define treatedlab
1 "Class has 13-17 students"
0 "Class has 22-15 students"; 
#delimit cr // To restore the carriage return delimiter inside a file

label val treated treatedlab
tab treated

save STAR_clean, replace
****************************************************************************/


********************************************************************************
*** 2. Descriptive Statistics

*Treatment
tabulate treated


*Outcomes
summarize tscorek
summarize tscorek if treated==0
summarize tscorek if treated!=0 //Difference btw treated and non treated

bysort treated: summarize tscorek

*How to use help files
help table
table treated, statistic (mean tscorek)  statistic (sd tscorek)

*Controls
sum boy freelunk totexpk
bysort treated: summarize boy freelunk totexpk
table treated, statistic (mean boy freelunk totexpk) 



******************************************************************************************
*** 3. Graph

histogram totexpk, by(treated)
twoway (histogram totexpk if treated==0,color(blue%30) bin(20)) (histogram totexpk if treated==1,color(red%30) bin(20)), legend(order(1 "Non-Treated" 2 "Treated")) title("Years of total teaching experience") 
********************************************************************************



********************************************************************************
*** 4. Means Tests
*Test that the mean of variables is equal between two groups

*outcome
ttest tscore, by(treated) //significant difference in test scores

help ttest
ttest tscore, by(treated) level(90)

*controls
ttest boy, by(treated)       // no significant difference
ttest freelunk, by(treated)  // no significant difference
ttest totexpk, by(treated)  //significant difference 


********************************************************************************



******************************************************************************************
*** 5. Regression Analysis
regress tscore treated
outreg2 using Regression1.doc, replace ctitle(Model 1)


regress tscore treated boy freelunk totexpk
outreg2 using Regression1.doc, append ctitle(Model 2)


log close


