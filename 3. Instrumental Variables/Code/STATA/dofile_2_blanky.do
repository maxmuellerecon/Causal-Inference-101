*************Exercise 2******************************

*INSTRUMENTAL VARIABLE REGRESSION

clear
clear matrix
set mem 500m
set more off
capture log close
//
// log using "C:\Users\danie\Desktop\Arbeit Lehrstuhl\Seminar\Exercise\Exercise\Exercise 2\STATA\Exercise2.log", replace
//
// cd "C:\Users\danie\Desktop\Arbeit Lehrstuhl\Seminar\Exercise\Exercise\Exercise 2\STATA"


log using "INSERT LINK TO YOUR DIRECTORY\Exercise2.log", replace

cd "INSERT LINK TO YOUR DIRECTORY"
use dataset_2


/********************************************************************
1. First stage and reduced form
2. OLS & 2 SLS
3. Tests
4. Multiple instruments --> Precision

********************************************************************/

/*JOSHUA D. ANGRIST AND ALAN B. KRUEGER (1991)

Compulsory schooling laws:
Quarter of birth as an instrumental variable of years of education
Students born earlier in the year are old for their grade and thus have completed 
less years of schooling when reaching the compulsory age for schooling

Question: Does this cause earlier dropouts? Therefore less education and less wages?

Assumption: Quarter of birth is unrelated to innate ability, motivation or family background

Remember: Z: Quarter of birth, X: Education, Y: Wages

*/





****************************************************************************************
*** 1. FIRST STAGE & REDUCED FORM

*** i. First stage: Effect of Z on X --> Relevance

*Create dummy variables for quarter of birth
tab QOB, gen(QOB_)            

*Regress QOB and Years on Education
reg EDUC QOB_1-QOB_3 YR20-YR28

*Create new variable p_educ with the option to contain the linear prediction
predict p_educ, xb      

*Create new variable with the birthyear and the birth quarter attached to it   
gen YQOB=YOB*10+QOB		

*For every YQOB we attach the predicted mean value of education 
bysort YQOB: egen m_p_educ=mean(p_educ)

*-->e.g. if you are born in the first quarter of 1920 --> prediced value is 11.12483

*Model graph of Year/Quarter of Birth and predicted education									
twoway line m_p_educ YQOB, saving(educ)


*** ii. REDUCED FORM: Effect of Z on Y

*Again: Regress, predict values (here wages) and then attached predicted values to new column by YQOB
reg LWKLYWGE QOB_1-QOB_3 YR20-YR28
predict p_lwage, xb
bysort YQOB: egen m_p_lwage=mean(p_lwage)

*Model graph of Year/Quarter of Birth and predicted wages	
twoway line m_p_lwage YQOB, saving(wage)

*Let's look at both graph simultaneously
gr combine educ.gph wage.gph, col(1) iscale(1)






****************************************************************************************
*** 2. OLS & 2SLS

*** i. OLS
***** To-Do: Regress WAGE on EDUCATION and YEARs *****

*** ii. 2SLS with instrumented education by QOB_1 and store coefficient of Education as iv1
ivregress 2sls LWKLYWGE (EDUC = QOB_1) YR20-YR28
scalar iv1=_b[EDUC]

*First stage --> Store coefficient
***** To-Do: Regress EDUCATION on QUARTEROFBIRTH_1 and YEARs *****
***** To-Do: save coefficient of QUARTEROFBIRTH_1 as fs *****

*Reduced Form --> Store coefficient
***** To-Do: Regress WAGE on QUARTEROFBIRTH_1 and YEARs *****
***** To-Do: save coefficient of QUARTEROFBIRTH_1 as rf *****


scalar iv2=rf/fs
scalar list

*IV estimator is exactly the same as = reduced form/first stage






**************************************************************************************************************
*** 3. TESTS

*** i. weak instruments --> Is instrument relevant?
*Test F-statistic of the first stage regression of the instrumental variable

***** To-Do: Regress EDUCATION on QUARTEROFBIRTH_1 and YEARs *****
test QOB_1

*--> Critical Value: 10 --> here: 61.45 --> Not a weak instrument

*** ii. endogeneity --> Is instrument exogeneous? --> corr(Z,u)=0?
***J Test of overidentification --> need more Z´s than X´s

*Compute underlying regression and store residuals
ivregress 2sls LWKLYWGE (EDUC = QOB_1-QOB_3)YR20-YR28
predict resid, residuals

*residual correlated with exogeneous variables (e.g. Z´s)?
***** To-Do: Regress RESIDUALS on QUARTEROFBIRTH's and YEARs *****

*test statistic - chi-squared distributed
scalar test=e(r2)*e(N) //number of observations times R2
scalar list test 
*H0: the IVs are exogenous; H1: at least one of the IVs is not exogenous
*If some IVs are exogenous and others are endogeneous, J-statistic will be large, and H0 will be rejected.
*Critical value chi-squared at q restrictions (q= # of IVs  #of endogeneous variables - here 3 to 1: --> here 1% 9.21, 5% 5.99, 10% 4.61)
*Our value is 2.31
*J Statistic is lower than critical value --> no rejection --> instruments are exogenous


*Alternative Solution
ivregress 2sls LWKLYWGE (EDUC = QOB_1-QOB_3) YR20-YR28, r small
estat overid






**************************************************************************************************************
**** 4. Multiple instruments -->  Increase precision

*Generate Dummy not for each year, but each quarter
gen QYOB=YOB*10+QOB
tab QYOB, gen(QYOB_)

*Execute original regression
ivregress 2sls LWKLYWGE (EDUC = QOB_1-QOB_3) YR20-YR28
*Execute with all instruments
ivregress 2sls LWKLYWGE (EDUC = QYOB_1-QYOB_39)YR20-YR28
*Exclude baselines that are ommitted because of collinearity
ivregress 2sls LWKLYWGE (EDUC = QYOB_1-QYOB_3 QYOB_5-QYOB_7 QYOB_9-QYOB_11 QYOB_13-QYOB_15 QYOB_17-QYOB_19 QYOB_21-QYOB_23 QYOB_25-QYOB_27 QYOB_29-QYOB_31 QYOB_33-QYOB_35 QYOB_37-QYOB_39)YR20-YR28

*More precise estimates --> Higher t Statistic

log close