***********************************
**	ACTG175 Table 1 Exercise	***
**	Tajrian Amad				***
**	02/05/2024					***
***********************************;

/* Comment 1*/
* Comment 2;

* Create the library;
libname actg175 "/home/u63507303/biocomputing";
run;

* Create a temporary dataset from the permanent dataset;
data work.inclusion;
	set actg175.baseline;
run;

* Use proc contents to see the meta-data;
proc contents data = inclusion varnum; 
run;

* Look at our data, specifically strat2 variable;
proc print data = inclusion (obs = 10);
run;

* Remove patients who are missing data on prior ARV;
data inclusion;
	set inclusion;
	if strat2 ne /*not equal*/ .; * . means missing;
run;

* Check the number of observations in inclusion;
proc contents data = inclusion varnum;
run;

* ---------------------------------------------------;

* 1 - White, non-Hispanic 2 - Black, non-Hispanic 3 - Hispanic 4 - Other;
* 1 - naive (no ARV) 2 - exposed;

* Run frequencies for categorical variables in all participants;
proc freq data = inclusion;
	tables sex raceth homosex ivdrug hemophi karnof symptom strat2;
run;
	
* Cross classification (stratification) of frequencies;
proc freq data = inclusion;
	tables sex * strat2;
run;

* Many cross classifications by strat2 and reduce output;
proc freq data = inclusion;
	tables (sex raceth homosex ivdrug hemophi karnof symptom) * strat2 / nopercent norow;
run;

* Calculate means for age and average CD4 cell count;
proc means data = work.inclusion;
	var age avecd4;
run;

* Calculate means stratified by strat2 variable;
proc means data = inclusion;
	class strat2;
	var age avecd4;
run;

