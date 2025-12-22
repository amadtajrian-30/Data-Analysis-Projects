/*

Tajrian Amad
BIST 0551 Fall 2024  
Class Project Task #4
12/03/2024

*/


/*
	PMID: 36166875
	Table: 3
	
	Outcome variable of interest: HAVARTH4
	Explanatory variable of interest: EXERANY2
	
	All six explanatory variables:
	- Numeric variables: BMI, PHYSHLTH
	- Categorical variables (2 levels): EXERANY2, SEX
	- Categorical variables (> 2 levels): AGEGROUP, SMOKDAY2
	
*/


libname cp "/home/u63507303/BIST0551/Class Project";


* Specify name and location of format catalog: pf24format.sas7bcat;
options FMTSEARCH= (cp.pf24format);

* Create a working dataset to include only 7 relevant variables;
data cp.working;
	set cp.pf24;
	keep HAVARTH4 EXERANY2 BMI PHYSHLTH SEX AGEGROUP SMOKDAY2;
run;

* Print out the first 10 observations from the working dataset; 
proc print data = cp.working (OBS = 10);
	VAR HAVARTH4 EXERANY2 BMI PHYSHLTH SEX AGEGROUP SMOKDAY2;
run;

* Check the normality of numerical explanatory variables BMI PHYSHLTH by HAVARTH4 group;

proc univariate data = cp.working normal;
   var BMI PHYSHLTH;
   class HAVARTH4;
run;

* Both BMI and PHYSHLTH variables are not normally distributed, so use the Wilcoxon rank sum test;
proc npar1way data = cp.working wilcoxon;
	var BMI PHYSHLTH;
	class HAVARTH4;
run;

* Compare the distribution of categorical variables using chi square test;
proc freq data = cp.working;
   tables EXERANY2*HAVARTH4 SEX*HAVARTH4 AGEGROUP*HAVARTH4 SMOKDAY2*HAVARTH4/ chisq nopercent norow expected;
RUN;

* Generate 6 simple logistic regression models for explanatory variables;
proc logistic data = cp.working;
	model HAVARTH4 (event = "Yes") = BMI;
	title 'logistic regression: BMI HAVARTH4';
run;

proc logistic data = cp.working;
	model HAVARTH4 (event = "Yes") = PHYSHLTH;
	title 'logistic regression: PHYSHLTH HAVARTH4';
run;

proc logistic data = cp.working;
	class EXERANY2 (ref = "No") / param = ref;
	model HAVARTH4 (event = "Yes") = EXERANY2;
	title 'logistic regression: EXERANY2 HAVARTH4';
run;

proc logistic data = cp.working;
	class SEX (ref = "Male") / param = ref;
	model HAVARTH4 (event = "Yes") = SEX;
	title 'logistic regression: SEX HAVARTH4';
run;

proc logistic data = cp.working;
	class AGEGROUP (ref = "Age 45 - 54") / param = ref;
	model HAVARTH4 (event = "Yes") = AGEGROUP;
	title 'logistic regression: AGEGROUP HAVARTH4';
run;

proc logistic data = cp.working;
	class SMOKDAY2 (ref = "Not at all") / param = ref;
	model HAVARTH4 (event = "Yes") = SMOKDAY2;
	title 'logistic regression: SMOKDAY2 HAVARTH4';
run;

* Perform multiple regression model;
proc logistic data = cp.working;
	class EXERANY2 (ref = "No") SEX (ref = "Male") AGEGROUP (ref = "Age 45 - 54") SMOKDAY2 (ref = "Not at all") / param = ref;
	model HAVARTH4 (event = "Yes") = EXERANY2 BMI AGEGROUP PHYSHLTH SEX SMOKDAY2 / selection = stepwise include = 3;
	title 'Multiple regression model with Stepwise selection for HAVARTH4';
run;

proc logistic data = cp.working;
	class EXERANY2 (ref = "No") SEX (ref = "Male") AGEGROUP (ref = "Age 45 - 54") SMOKDAY2 (ref = "Not at all") / param = ref;
	model HAVARTH4 (event = "Yes") = EXERANY2 BMI AGEGROUP PHYSHLTH SEX SMOKDAY2 / selection = forward include = 3;
	title 'Multiple regression model with Forward selection for HAVARTH4';
run;

proc logistic data = cp.working;
	class EXERANY2 (ref = "No") SEX (ref = "Male") AGEGROUP (ref = "Age 45 - 54") SMOKDAY2 (ref = "Not at all") / param = ref;
	model HAVARTH4 (event = "Yes") = EXERANY2 BMI AGEGROUP PHYSHLTH SEX SMOKDAY2 / selection = backward include = 3;
	title 'Multiple regression model with Backward elimination for HAVARTH4';
run;

* Model with backward elimination approach is selected;

* Final model with options to assess model fitting;
proc logistic data = cp.working plots = (roc influence dfbetas);
	class EXERANY2 (ref = "No") AGEGROUP (ref = "Age 45 - 54") SEX (ref = "Male") / param = ref;
	model HAVARTH4 (event = "Yes") = EXERANY2 BMI AGEGROUP SEX PHYSHLTH / rsquare lackfit;
	title 'Final model';
run;




	
	
	
	
	
	
	
