* Keep the selected variables from the merged dataset into a new subset;
libname NHANES "/home/u63507303/EPI0657/Project";

data nhanes.nhanes1318_subset;
	set nhanes.nhanes_1318combined;
	keep SDMVPSU SDMVSTRA weight_mec_adj weight_int_adj RIAGENDR RIDAGEYR RIDRETH3 DMDEDUC2 DMDMARTL DMDFMSIZ DMDHHSZA INDHHIN2 INDFMMPI 
	INDFMMPC HIQ011 PAQ605 SMQ020 SMQ040 BPQ020 BPQ030 BPQ080 DIQ010 FSDHH FSQ165 FSQ012 KIQ026 BMXBMI;
run;

proc contents data = nhanes.nhanes1318_subset varnum;
run;

/* 
SAS Variable Names: 

RIAGENDR: Gender
RIDAGEYR: Age in years at screening
RIDRETH3: Race/Hispanic origin w/ NH Asian
DMDEDUC2: Education level - Adults 20+
DMDMARTL: Marital status
DMDFMSIZ: Total number of people in the Family
DMDHHSZA: # of children 5 years or younger in HH
INDHHIN2: Annual household income
BMXBMI: Body Mass Index (kg/m^2)
BPQ020: Ever told you had high blood pressure
BPQ030: Told had high blood pressure - 2+ times
BPQ080: Doctor told you - high cholesterol level
FSDHH: Household food security category
FSQ165: HH FS benefit: ever received
FSQ012: HH FS benefit: receive in last 12 months
HIQ011: Covered by health insurance
INDFMMPC: Family monthly poverty level category
INDFMMPI: Poverty to income ratio
KIQ026: Ever had kidney stones?
PAQ605: Vigorous work activity
SMQ020: Smoked at least 100 cigarettes in life
SMQ040: Do you now smoke cigarettes
DIQ010: Doctor told you have diabetes
*/ 

*-------------------------------------------------;
* Create new variables - those with missing values;
data nhanes.nhanes1318_project;
	set nhanes.nhanes1318_subset;
	* education level;
	if DMDEDUC2 in (1, 2) then edu_level = 1; * Less than high school;
	else if DMDEDUC2 = 3 then edu_level = 2; * High school graduate/GED or equivalent;
	else if DMDEDUC2 = 4 then edu_level = 3; * Some college or AA degree;
	else if DMDEDUC2 = 5 then edu_level = 4; * College graduate or above;
	else edu_level = .;
	
	* marital status;
	if DMDMARTL in (77, 99) then marital = .;
	else marital = DMDMARTL;
	
	* family size;
	if DMDFMSIZ in (1, 2, 3) then famsize = 1; * Less than 3 people in the family;
	else if DMDFMSIZ in (4, 5, 6) then famsize = 2; *4 to 6 people;
	else if DMDFMSIZ = 7 then famsize = 3; * 7 or more people;
	else famsize = .;
	
	* number of children in household;
	if DMDHHSZA = 0 then childrenHH = 0; * No children;
	else if DMDHHSZA = 1 then childrenHH = 1; * 1 children;
	else if DMDHHSZA in (2, 3) then childrenHH = 2; * 2 or more children;
	else childrenHH = .;
	
	* annual household income;
	if INDHHIN2 in (1, 2, 3, 4, 13) then income = 1; * Less than $20,000;
	else if INDHHIN2 in (5, 6, 7) then income = 2; * $20,000 to $44,999;
	else if INDHHIN2 in (8, 9, 10, 14) then income = 3; * $45,000 to $74,999;
	else if INDHHIN2 in (12, 15) then income = 4; * $75,000 and over;
	else if INDHHIN2 in (77, 99) then income = .;
	
	* family income level;
	if INDFMMPI < 1.3 then income_level = 1; * < 1.3 - low family income;
	else if 1.3 <= INDFMMPI < 3.5 then income_level = 2; *1.3-3.5 - middle family income;
	else if INDFMMPI >= 3.5 then income_level = 3; * >=3.5 - high family income;
	else INDFMMPI = .;
	
	* health insurance status;
	if HIQ011 in (7, 9) then health_ins = .;
	else health_ins = HIQ011;
	
	* 4-level BMI variable;
	if BMXBMI < 18.5 then bmi_cat = 1; /*underweight*/
	else if BMXBMI < 25 then bmi_cat = 2; /*normal weight*/
	else if BMXBMI < 30 then bmi_cat = 3; /*overweight*/
	else bmi_cat = 4; /*obese*/
	if BMXBMI = . then bmi_cat = .;
	
	* obesity binary variable;
	if BMXBMI <30 then obese = 0; *Not Obese;
	else if BMXBMI >=30 then obese = 1; *Obese;
	
	* hypertension status;
	if BPQ030 = 1 then htn = 1; * Yes - hypertension;
	else if BPQ020 = 2 or BPQ030 = 2 then htn = 0; * No - hypertension;
	else htn = .;
	
	* cholesterol status;
	if BPQ080 in (7, 9) then cholesterol = .;
	else cholesterol = BPQ080;
	
	* diabetes status;
	if DIQ010 = 1 then diabetes = 1; * Yes;
	else if DIQ010 = 2 then diabetes = 2; * No;
	else diabetes = .;

	* food security status;
	* 3-level status;
	if FSDHH in (1, 2) then foodsec = 1; * full/marginal food security;
	else if FSDHH = 3 then foodsec = 2; * low food security;
	else if FSDHH = 4 then foodsec = 3; * very low food security;
	else foodsec = .;
	
	* binary variable;
	if FSDHH in (1, 2) then foodsec2 = 1; *food secure;
	else if FSDHH in (3, 4) then foodsec2 = 2; *food insecure;
	else foodsec2 = .;
	
	* receipt of food stamps or SNAP benefits;
	if FSQ165 = 1 or FSQ012 = 1 then fsben = 1; * Yes - food stamps or SNAP benefits;
	else if FSQ165 = 2 then fsben = 2; * No - food stamps or SNAP benefits;
	else fsben = .;
	
	* kidney stones status - binary;
	if KIQ026 = 1 then kidney_stones = 1; * yes;
	else if KIQ026 = 2 then kidney_stones = 0; * no;
	else kidney_stones = .;
	
	* physical activity status;
	if PAQ605 = 1 then phys_act = 1; * yes;
	else if PAQ605 = 2 then phys_act = 0; * no;
	else phys_act = .;
	
	* smoking status;
	if SMQ020 = 1 and SMQ040 in (1, 2) then smoker = 1; * Current smoker;
	else if SMQ020 = 1 and SMQ040 = 3 then smoker = 2; * Former smoker;
	else if SMQ020 = 2 then smoker = 3; * Never smoked;
run;

proc contents data = nhanes.nhanes1318_project;
run;


* Check work for newly classified variables; 
proc freq data = nhanes.nhanes1318_project;
	tables DMDEDUC2*edu_level DMDMARTL*marital DMDFMSIZ*famsize DMDHHSZA*childrenHH INDHHIN2*income HIQ011*health_ins
		   BPQ030*htn BPQ080*cholesterol DIQ010*diabetes FSDHH*foodsec FSQ165*fsben KIQ026*kidney_stones 
		   PAQ605*phys_act SMQ020*smoker / missing nocol norow nopercent;
run;

proc means data = nhanes.nhanes1318_project;
	class income_level;
	var INDFMMPI;
run;

proc means data = nhanes.nhanes1318_project;
	class bmi_cat obese;
	var BMXBMI;
run;

* -----------------------------------------------------------------------------------------------------------;
* Descriptive and inferential statistical analysis using BINARY food security variable;

* Sample limited to individuals responding to questions about food security and kidney stones;
data nhanes.nhanes1318_strat2;
	set nhanes.nhanes1318_project;
	if foodsec2 in (1, 2) and kidney_stones in (1, 0);
run;

proc freq data = nhanes.nhanes1318_strat2;
	tables (RIAGENDR RIDRETH3 edu_level marital famsize childrenHH health_ins bmi_cat obese htn cholesterol diabetes 
	fsben kidney_stones phys_act smoker income_level) * foodsec2 / norow chisq relrisk;
run;

proc freq data = nhanes.nhanes1318_strat2;
	tables (RIAGENDR RIDRETH3 edu_level marital famsize childrenHH health_ins bmi_cat obese htn cholesterol diabetes 
	fsben foodsec2 phys_act smoker income_level) * kidney_stones / norow chisq relrisk;
run;

* Use survey procedures to account for complex sampling design and weighting;
proc surveyfreq data = nhanes.nhanes1318_strat2 nomcar;
	strata SDMVSTRA;
	cluster sdmvpsu;
	weight weight_int_adj;
	tables (RIAGENDR RIDRETH3 edu_level marital famsize childrenHH health_ins bmi_cat obese htn cholesterol diabetes 
	fsben kidney_stones phys_act smoker income_level) * foodsec2 / col;
run;

proc surveymeans data = nhanes.nhanes1318_strat2 nomcar mean std cv;
	strata SDMVSTRA;
	cluster sdmvpsu;
	weight weight_int_adj;
	var RIDAGEYR;
	domain foodsec2;
run;

* Examine association stratified by income level;
proc freq data = nhanes.nhanes1318_strat2;
	tables income_level*foodsec2*kidney_stones / norow nopercent chisq relrisk cmh;
run;

* Test for interaction;
proc surveylogistic data = nhanes.nhanes1318_strat2;
	strata SDMVSTRA;
	cluster sdmvpsu;
	weight weight_int_adj;
	class foodsec2 (ref = '1') income_level (ref = '3') / param = ref;
	model kidney_stones (event = '1') = foodsec2 income_level foodsec2*income_level RIDAGEYR RIDRETH3 RIAGENDR obese htn cholesterol diabetes;
run;

*No significant interaction was detected (p = 0.8773), suggesting the association is consistent across income groups.;

* Unadjusted model: food insecurty and kidney stones association;
proc surveylogistic data = nhanes.nhanes1318_strat2 nomcar;
	strata SDMVSTRA;
	cluster sdmvpsu;
	weight weight_int_adj;
	class foodsec2 (ref = '1') RIDRETH3 (ref = '3') / param = ref;
	model kidney_stones (event = '1') = foodsec2;
run;

* Adjusted model: food insecurity and kidney stones association adjusted for selected covariates;
proc surveylogistic data = nhanes.nhanes1318_strat2 nomcar;
	strata SDMVSTRA;
	cluster sdmvpsu;
	weight weight_int_adj;
	class foodsec2 (ref = '1') RIDRETH3 (ref = '3') / param = ref;
	model kidney_stones (event = '1') = foodsec2 RIDAGEYR RIDRETH3 RIAGENDR obese htn cholesterol diabetes;
run;

* Unadjusted model: food insecurity and kidney stones association by PIR level;
proc surveylogistic data = nhanes.nhanes1318_strat2 nomcar;
	strata SDMVSTRA;
	cluster sdmvpsu;
	weight weight_int_adj;
	domain income_level;
	class foodsec2 (ref = '1') RIDRETH3 (ref = '3') / param = ref;
	model kidney_stones (event = '1') = foodsec2;
run;

* Adjusted model: food insecurity and kidney stones association by PIR level adjusted for selected covariates;
proc surveylogistic data = nhanes.nhanes1318_strat2 nomcar;
	strata SDMVSTRA;
	cluster sdmvpsu;
	weight weight_int_adj;
	domain income_level;
	class foodsec2 (ref = '1') RIDRETH3 (ref = '3') / param = ref;
	model kidney_stones (event = '1') = foodsec2 RIDAGEYR RIDRETH3 RIAGENDR obese htn cholesterol diabetes;
run;

	
	
	
	

