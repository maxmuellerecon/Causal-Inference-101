*************Exercise 3******************************

*Difference in Difference Regression


clear
clear matrix
set memory 400m
set more off
capture log close


/********************************************************************************
0. Install Package
1. Get the data
2. Replication of the summary statistics
3. Graph (Figure 1)
4. Diff-in-Diff (Table 3) (simple mean difference)
5. OLS - Regressions with either Dummies for the State (Table 4)
********************************************************************************/
log using "C:\Users\Max\Dropbox\Seminar SS22\Exercise\Exercise 3\STATA\Exercise3.log", replace

/*
Card & Krueger 1994

On April 1, 1992, New Jersey's minimum wage rose from
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
employment */



*0.Install Package*****************************************************************************

ssc install xml_tab
ssc install outreg2

*1. Get the data****************************************************

cd "C:\Users\Max\Dropbox\Seminar SS22\Exercise\Exercise 3\STATA"
use card_krueger
count
*Note: 410 restaurants, which corresponds to 473 contacted ones, of which 410 replied in wave one and only 409 in wave 2!
****************************************************************************/

*Relabel Status variable
#delimit ;  //set delimiter for multi column lines
label define statusLab
5 "Temporary closed: Fire"
4 "Temporary closed: Construcion"
3 "Closed"
2 "Under rennovation"
1 "Interviewed"
0 "Refused"; 
#delimit cr
label val status2 statusLab

*Replicate Second Part of Table 1 from the paper
count
tabulate status2
tab status2 state

*Relabel Chain variable and check how many restaurants of each chain were involved
label define ChainLab 4 "Wendy's" 3 "Roy Rogers" 2 "KFC" 1 "Burger King"
label val chain ChainLab

bysort state: tab chain
********************************************************************************



********************************************************************************
***2. Replication of Summary statistics (Table 2)

*overall summary statistics
summarize fte1 fte2 wage_st wage_st2
bysort state: sum fte1 percentft1 wage_st fte2 percentft2 wage_st2, sep(0)


*T-test full time equivalent employment and store mean of both populations and the t-Statistic in both waves
ttest fte1, by(state)
scalar define fte1_PA=r(mu_1)
scalar define fte1_NJ=r(mu_2)
scalar define fte1_t=r(t)

ttest fte2, by(state)
scalar define fte2_PA=r(mu_1)
scalar define fte2_NJ=r(mu_2)
scalar define fte2_t=r(t)


*Do the same for the starting wage
ttest wage_st, by(state)
scalar define wage_st1_PA=r(mu_1)
scalar define wage_st1_NJ=r(mu_2)
scalar define wage_st1_t=r(t)

ttest wage_st2, by(state)
scalar define wage_st2_PA=r(mu_1)
scalar define wage_st2_NJ=r(mu_2)
scalar define wage_st2_t=r(t)


*Create table 2
#delimit ;
matrix define table2=[fte1_PA, fte1_NJ, fte1_t \
wage_st1_PA, wage_st1_NJ, wage_st1_t \
fte2_PA, fte2_NJ, fte2_t \
wage_st2_PA, wage_st2_NJ, wage_st2_t 
] ;
matrix colnames table2= "PA" "NJ" "t-test" ;
matrix rownames table2= "Fulltime Employment Wave1" "Starting Wages Wave1" "Fulltime Employment Wave 2" "Starting Wages Wave 2" ;
#delimit cr


matrix list table2
xml_tab table2, save(table2.xls) replace
********************************************************************************



*3. Graph******************************************************************
histogram wage_st, by(state)
twoway (histogram wage_st if state==0, color(red%30) bin(20)) (histogram wage_st if state==1,color(blue%30) bin(20)), legend(order(1 "Pennsylvania" 2 "New Jersey")) title("DISTRIBUTION OF STARTING WAGE RATES") 
twoway (histogram wage_st2 if state==0, color(red%30) bin(20)) (histogram wage_st2 if state==1,color(blue%30) bin(20)), legend(order(1 "Pennsylvania" 2 "New Jersey")) title("DISTRIBUTION OF STARTING WAGE RATES") 


*4. Diff and Diff (Table 3)******************************************************
***Inter state differences 
*Save means, standard deviations and differences (Ttest with assumption of unequal variances)
*Wave 1
ci means fte1 if state==0
scalar define m_fte1_PA=r(mean)
scalar define se_fte1_PA=r(se)

ci means fte1 if state==1
scalar define m_fte1_NJ=r(mean)
scalar define se_fte1_NJ=r(se)


ttest fte1, by(state) unequal
scalar define d_fte1=m_fte1_NJ-m_fte1_PA
scalar define se_fte1=r(se)

*Wave 2
ci means fte2 if state==0
scalar define m_fte2_PA=r(mean)
scalar define se_fte2_PA=r(se)

ci means fte2 if state==1
scalar define m_fte2_NJ=r(mean)
scalar define se_fte2_NJ=r(se)

ttest fte2, by(state) unequal
scalar define d_fte2=m_fte2_NJ-m_fte2_PA
scalar define se_fte2=r(se)


***Difference within states between years
*Save means, standard deviations and differences (Ttest with assumption of unequal variances)
*PA
preserve
keep if state==0
ttest fte1=fte2
scalar define d_fte_PA=m_fte2_PA-m_fte1_PA
scalar define se_fte_PA=r(se)
restore

***NJ
preserve
keep if state==1
ttest fte1=fte2
scalar define d_fte_NJ=m_fte2_NJ-m_fte1_NJ
scalar define se_fte_NJ=r(se)
restore


*** DiD
scalar define dd=d_fte_NJ-d_fte_PA
scalar se_dd=1.36
scalar list 

*Create table 3
#delimit ;
matrix define table3=[m_fte1_PA, m_fte1_NJ, d_fte1 \
se_fte1_PA, se_fte1_NJ, se_fte1 \
m_fte2_PA, m_fte2_NJ, d_fte2 \
se_fte2_PA, se_fte2_NJ, se_fte2 \
d_fte_PA, d_fte_NJ, dd\
se_fte_PA, se_fte_NJ, se_dd
] ;
matrix colnames table3= "PA" "NJ" "State difference" ;
matrix rownames table3= "Wave 1- Mean" "Wave 1- Standard error"  "Wave 2 - Mean" "Wave 2 - Standard error" "Period difference - Mean"  "Period difference - SE" ;
#delimit cr

matrix list table3
xml_tab table3, save(table3.xls) replace

**************************************************************




**************************************************************
***5. OLS - Regressions with either Dummies for the State (Table 4)
use cardkrueger_long, clear

*Replace periodic values to perform diff-in-diff
replace period=0 if period==1
replace period=1 if period==2

*Generate interaction term for diff in diff design
gen reform=state*period

*Regression Without controls
reg fte state period reform
outreg2 using Regression1.doc, replace ctitle(Model 1)
*Approximately the same as our mean difference table

*Regression With controls
reg fte state period reform co_owned
outreg2 using Regression1.doc, append ctitle(Model 2)

log close


