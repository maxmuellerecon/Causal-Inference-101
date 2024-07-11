*************Exercise 4******************************

*Regression Discontinuity Design


clear
clear matrix
set memory 400m
set more off
capture log close


/********************************************************************************
1. Data manipulation
2. Create Figures 4.2 4.4 4.5
3. MLDA Regression Discontinuity
********************************************************************************/

/*
Carpenter & Dobkin, 2009

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
Here: Sharp RD Design: Treatment clearly switches on and off around the cutoff.
Other option: Fuzzy RD: The probability or intensity of treatment jumps at a
cutoff.
*/


log using "C:\Users\Max\Dropbox\Seminar SS22\Exercise\Exercise 4\STATA\Exercise4.log", replace

// set to directory where data is located
cd "C:\Users\Max\Dropbox\Seminar SS22\Exercise\Exercise 4\STATA"

use mlda

*1. Data manipulation******************************************
* Create Cutoff age variable (centered age variable for easier interpretation) and dummy for over 21
gen age = agecell - 21
gen over21 = agecell >= 21

*Create Second order polynomial and interaction terms
gen age2 = age^2
gen over_age = over21*age
gen over_age2 = over21*age2


*2. Create Figures 4.2 4.4 4.5******************************************
* Regressions for Figure 4.2.
* Regress for linear trend, and linear trend on each side, predict total mortality
* All variable = all causes for deaths (measured as deaths per 100,000 persons per 30-day interval counting from the twenty-first birthday)
reg all age over21
predict allfitlin
reg all age over21 over_age
predict allfitlini

* Figure 4.2. 
twoway (scatter all agecell) (line allfitlin agecell if age < 0, lcolor(black) lwidth(medthick)) ///
(line allfitlin agecell if age >= 0, lcolor(black red) lwidth(medthick)), legend(off)
graph export "Figure2.png", replace


* Regressions for Figure 4.4.
* Quadratic, and quadratic on each side
reg all age age2 over21
predict allfitq
reg all age age2 over21 over_age over_age2
predict allfitqi

label variable all       "Mortality rate from all causes (per 100,000)"
label variable allfitlin "Mortality rate from all causes (per 100,000)"
label variable allfitqi  "Mortality rate from all causes (per 100,000)"

* Figure 4.4.		 
twoway (scatter all agecell) (line allfitlin allfitqi agecell if age < 0,  lcolor(red black) lwidth(medthick medthick) lpattern(dash)) ///
                             (line allfitlin allfitqi agecell if age >= 0, lcolor(red black) lwidth(medthick medthick) lpattern(dash)), legend(off)
graph export "Figure4.png", replace


* Regressions for Fig 4.5
* Regress "Motor Vehicle Accidents" on running and treatment variable (linear, and quadratic on each side) an predict mva
reg mva age over21
predict exfitlin
reg mva age age2 over21 over_age over_age2
predict exfitqi

*Do we need more controls? Treatment status (over21) is determined solely by age. Assuming that the effect of age on death rates is captured by a linear/quadratic function, we can be sure that no OVB afflicts this short regression

*Regress suicide on running and treatment variable and predict
reg suicide age over21
predict sufitlin
reg suicide age age2 over21 over_age over_age2
predict sufitqi

* Regress "Internal causes" on running and treatment variable (linear, and quadratic on each side) and predict internal causes
reg internal age over21
predict infitlin
*TD
reg internal age age2 over21 over_age over_age2
predict infitqi

*Label Variabels
label variable mva  "Mortality rate (per 100,000)"
label variable infitqi  "Mortality rate (per 100,000)"
label variable exfitqi  "Mortality rate (per 100,000)"

*Figure 4.5
twoway (scatter mva internal agecell) (line exfitqi infitqi agecell if agecell < 21) ///
                                       (line exfitqi infitqi agecell if agecell >= 21), ///
									   legend(off) text(28 20.1 "Motor Vehicle Fatalities") ///
									   text(17 22 "Deaths from Internal Causes")
graph export "Figure5.png", replace


* 3.  MLDA Regression Discontinuity******************************************

* Generate dummy for other causes
gen ext_oth = external - homicide - suicide - mva

/*Use smaller bandwith:
For the small set of points close to the boundary, nonlinear trends need not concern us at
all. This suggests an approach that compares averages in a narrow
window just to the left and just to the right of the cutoff. A drawback
here is that if the window is very narrow, there are few observations left,
meaning the resulting estimates are likely to be too imprecise to be
useful (harder to extrapolate). Still, we should be able to trade the reduction in bias near the
boundary against the increased variance suffered by throwing data
away, generating some kind of optimal window size.
--> We choose bandwith of 2 years*/

foreach x in all mva suicide homicide ext_oth internal alcohol {

reg `x' age over21, robust
if ("`x'"=="all"){
	outreg2 over21 using table41.xls, replace bdec(2) sdec(2) noaster excel
}
else{
	outreg2 over21 using table41.xls, append bdec(2) sdec(2) noaster excel
}

reg `x' age age2 over21 over_age over_age2, robust
outreg2 over21 using table41.xls, append bdec(2) sdec(2) noaster excel

reg `x' age over21 if agecell >= 20 & agecell <= 22, robust
outreg2 over21 using table41.xls, append bdec(2) sdec(2) noaster excel

reg `x' age age2 over21 over_age over_age2 if agecell >= 20 & agecell <= 22, robust
outreg2 over21 using table41.xls, append bdec(2) sdec(2) noaster excel

}




