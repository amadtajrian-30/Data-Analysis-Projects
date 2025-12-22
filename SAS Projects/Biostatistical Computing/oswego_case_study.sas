***********************************
**		Oswego Case Study		***
**		Tajrian Amad			***
**		03/04/2024				***
***********************************;

%let path = /home/u63507303/biocomputing;	* macro variable only takes text;

* Import our csv file;
data food;
	infile "&path/OswegoCaseStudy1.csv" dlm = ',' dsd firstobs = 2;	* firstobs because data starts second;
	input 	subject :$12. counties :$15. age :2. sex :$6. Ill bakedham spinach mashedpota 
			cabbagesal jello rolls brownbread milk coffee water cakes vanilla chocolate 
			fruitsalad :2. timesupper :date11. dateonset :date11. recstatus uniquekey :2.;
run; 

* Look at our import;
proc print data = food;
run;

* The easier way!;
proc import datafile = "&path/OswegoCaseStudy1.csv"
	out = food2 
	dbms = csv
	replace;
	getnames = yes;
run;

* Look at proc import;
proc print data = food2;
run;


* ------------------------------------------------------------;


* Look at the metadata for the dataset;
proc contents data = food2 varnum;
run;

* How many got Ill;
proc freq data = food2;
	table Ill;
run;

* How many males got Ill;
proc freq data = food2;
	tables Ill;
	where sex = "Female";
run;

* The better way to find out Ill stratified by sex;
proc freq data = food2;
	tables Ill * sex / nopercent norow;
run;

* Mean, min, max, and median for age;
proc means data = food2 mean min max median;
	var age;
run;

* Let's analyze all the foods;
proc freq data = food2;
	tables (bakedham spinach mashedpota cabbagesal jello rolls brownbread milk coffee water 
			cakes vanilla chocolate fruitsalad) * ill;
run;










