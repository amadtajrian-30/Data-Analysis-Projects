libname  proj "/home/u63507303/BIST0615";

proc contents data = proj.nhis2022_cat_class varnum;
run;

/* 
Selected variables for analysis: 

NATUSBORN_A: Born in U.S. or U.S. territory
PHQCAT_A: Severity of depressive symptoms - PHQ scale categorization
EDUCP_A: Educational level of the sample adult
RATCAT_A: The ratio of the family's income to the poverty threshold, grouped into 14 ordered categories
AGEP_A: The age of the sample adult
SEX_A: The sex of the sample adult
*/

* Dataset including selected variables for the project;
data proj.nhis2022_project;
	set proj.nhis2022_cat_class;
	keep NATUSBORN_A PHQCAT_A EDUCP_A RATCAT_A AGEP_A SEX_A;
run;

proc contents data = proj.nhis2022_project;
run;

proc freq data = proj.nhis2022_project;
	tables NATUSBORN_A PHQCAT_A EDUCP_A RATCAT_A AGEP_A SEX_A;
run;


* Recode selected variables;
data proj.nhisproj_recode;
	set proj.nhis2022_project;
	* Depressive symptoms;
	if PHQCAT_A = 1 then PHQCAT_A_3LVL = 1; * None/Minimal;
	else if PHQCAT_A = 2 then PHQCAT_A_3LVL = 2; * Mild;
	else if PHQCAT_A in (3, 4) then PHQCAT_A_3LVL = 3; * Moderate/Severe;
	else PHQCAT_A = .;
	
	* Nativity status;
	if NATUSBORN_A = 1 then NATUSBORN_A = 1; *US born;
	else if NATUSBORN_A = 2 then NATUSBORN_A = 2; *Non-US born;
	else if NATUSBORN_A in (7, 8) then NATUSBORN_A = .;
	
	* Education level;
	if EDUCP_A in (1, 2) then EDUCP_A = 1; * Less than high school;
	else if EDUCP_A in (3, 4) then EDUCP_A = 2; * High school diploma or GED equivalent;
	else if EDUCP_A = 5 then EDUCP_A = 3; * Some college, no degree;
	else if EDUCP_A in (6, 7) then EDUCP_A = 4; * Associate's degree;
	else if EDUCP_A = 8 then EDUCP_A = 5; * Bachelor's degree;
	else if EDUCP_A in (9, 10) then EDUCP_A = 6; * Postgraduate degree;
	else EDUCP_A = .;
	
	* Family income to poverty ratio - poverty groups/levels;
	if RATCAT_A IN (1, 2, 3) then RATCAT_A = 1; * Below Poverty (<100% FPL);
    else if RATCAT_A IN (4, 5, 6, 7) then RATCAT_A = 2; * Low Income (100% to <200% FPL);
    else if RATCAT_A IN (8, 9, 10, 11) then RATCAT_A = 3; * Middle Income (200% to <400% FPL);
    else if RATCAT_A IN (12, 13, 14) then RATCAT_A = 4; * High Income (>= 400% FPL);
    else RATCAT_A = .; 
    
    * Age;
    if AGEP_A = . or AGEP_A in (97, 99) then AGE_GROUP = .; * Handle missing original age;
    else if 18 <= AGEP_A <= 34 then AGE_GROUP = 1; * 18-34 years;
    else if 35 <= AGEP_A <= 49 then AGE_GROUP = 2; * 35-49 years;
    else if 50 <= AGEP_A <= 64 then AGE_GROUP = 3; * 50-64 years;
    else if AGEP_A >= 65 then AGE_GROUP = 4; * 65+ years;
    else AGE_GROUP = .; 
run;

* Dataset applying inclusion and exclusion criteria - respondents with missing responses are excluded;
data proj.nhisproj_subset;
	set proj.nhisproj_recode;
	if NATUSBORN_A in (1, 2) and EDUCP_A ne . and AGE_GROUP ne . and PHQCAT_A_3LVL ne .;
run;

proc contents data = proj.nhisproj_subset;
run;

* Check distributions of each variable;
proc freq data = proj.nhisproj_subset;
	tables NATUSBORN_A PHQCAT_A_3LVL EDUCP_A RATCAT_A SEX_A AGE_GROUP;
run;


/* Bivariate associations */

* NATUSBORN_A and independent variables;
proc freq data = proj.nhisproj_subset;
	tables (AGE_GROUP SEX_A EDUCP_A RATCAT_A) * NATUSBORN_A / norow chisq trend measures cl;
run;

* PHQCAT_A and predictors;
proc freq data = proj.nhisproj_subset;
	table (NATUSBORN_A SEX_A AGE_GROUP EDUCP_A RATCAT_A) * PHQCAT_A_3LVL / chisq cmh measures cl;
run;


/* Proportional odds model: checking for proportional odds assumption */

* Unadjusted model with predictor of interest;
proc logistic data = proj.nhisproj_subset descending;
	model PHQCAT_A_3LVL = NATUSBORN_A;
	oddsratio NATUSBORN_A;
run; 

* Proportional odds assumption met for nativity status (p-value: 0.3822);

* Unadjusted model with covariates;
* Model with all covariates (without predictor of interest);
proc logistic data = proj.nhisproj_subset descending;
	class AGE_GROUP SEX_A EDUCP_A RATCAT_A;
	model PHQCAT_A_3LVL = AGE_GROUP SEX_A EDUCP_A RATCAT_A;
run;

* Proportional odds assumption met for all covariates (p-value: 0.7932)

* Now, run models with each covariate by itself to check for proportional odds assumption;
* Age group - ordinal categorical variable;
proc logistic data = proj.nhisproj_subset descending;
	class AGE_GROUP;
	model PHQCAT_A_3LVL = AGE_GROUP;
run;

* Proportional odds assumption met for age group (p-value: 0.5734);

* Compare models to check if age group should be treated as categorical or continuous;
proc logistic data = proj.nhisproj_subset descending;
	model PHQCAT_A_3LVL = AGE_GROUP;
run;

/* 
Predictor	        -2 Log L	DF		Difference	P-value
Age - continuous	1215.749	1		0.398	    0.819549893
Age - categorical	1215.351	3	

Linearity assumption holds. Age should be treated as a continuous variable.
*/

* Sex - categorical (nominal) variable;
proc logistic data = proj.nhisproj_subset descending;
	class SEX_A;
	model PHQCAT_A_3LVL = SEX_A;
run;

* Proportion odds assumption holds for sex (p-value: 0.5001);

* Education level - categorical (ordinal) variable;
proc logistic data = proj.nhisproj_subset descending;
	class EDUCP_A;
	model PHQCAT_A_3LVL = EDUCP_A;
run;

* Proportion odds assumption holds for education level (p-value: 0.3721);

* Compare models to check if education level should be treated as categorical or continuous;
proc logistic data = proj.nhisproj_subset descending;
	model PHQCAT_A_3LVL = EDUCP_A;
run;

/* 
Predictor	            -2 Log L	DF		Difference	P-value
Education - continuous	1210.138	1		4.043	    0.400217711
Education - categorical	1206.095	5	

Linearity assumption holds. Education level should be treated as a continuous variable.
*/

* Federal poverty level;
proc logistic data = proj.nhisproj_subset descending;
	class RATCAT_A;
	model PHQCAT_A_3LVL = RATCAT_A;
run;

* Proportion odds assumption holds for federal poverty level (p-value: 0.6177);

* Compare models to check if federal poverty level should be treated as categorical or continuous;
proc logistic data = proj.nhisproj_subset descending;
	model PHQCAT_A_3LVL = RATCAT_A;
run;

/* 
Predictor	        -2 Log L	DF		Difference	P-value
FPL - continuous	1196.474	1		0.124	    0.939882887
FPL - categorical	1196.35	    3	

Linearity assumption holds. Federal poverty level should be treated as a continuous variable.
*/		

* Proportional odds assumption holds for all predictor of interest and covariates;

* Adjusted proportional odds model: PHQCAT_A_3LVL and predictor of interest and covariates;
proc logistic data = proj.nhisproj_subset descending;
	class NATUSBORN_A (ref = '1') SEX_A (ref = '1');
	model PHQCAT_A_3LVL = NATUSBORN_A AGE_GROUP SEX_A EDUCP_A RATCAT_A / clodds = wald;
	oddsratio NATUSBORN_A;
run;

proc logistic data = proj.nhisproj_subset descending;
	class NATUSBORN_A (ref = '1') SEX_A (ref = '1');
	model PHQCAT_A_3LVL = NATUSBORN_A AGE_GROUP RATCAT_A/ clodds = wald;
	oddsratio NATUSBORN_A;
run;



