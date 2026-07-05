libname asgar "H:/My Drive/Project/mental health papers/Data/data/dataset for work";

/*"C:\Users\Dell\Dropbox\Adolecent mental health papers With Asgar\Data\data\dataset for work";*/
;


proc import datafile= "H:/My Drive/Project/mental health papers/Data/data/dataset for work/full dataset_update.dta"
    out= asgar.full
    dbms=DTA;
run;


*PROC PRINT data= asgar.full;
*RUN;

proc contents data=asgar.full_1;
run;



DATA asgar.full;
    set asgar.full;
    /* Recode string to numeric */
    if QHTYPE = "Other urban" then residence = 1;
    	else if QHTYPE = "Rural" then residence = 1;
    		else if QHTYPE = "Urban" then residence = 0;
    			else residence = .; /* Missing value for unmatched cases */

	if c_attend_school = "No" then attend_s = 0;
	else if c_attend_school = "Yes , madrasa" then attend_s = 1;
		else if c_attend_school = "Yes, school" then attend_s = 1;
			else if c_attend_school = "Yes, school/college" then attend_s = 1;
				else if c_attend_school = "Yes,madrasa" then attend_s = 1;
					else attend_s = .;
	
	if 0<= Depression <=4 then dep1 = 0;
		else if 5<= Depression <=9  then dep1 = 1;
			 else if 10<= Depression <=14  then dep1 = 2;
			 	else if 15<= Depression <=19  then dep1 = 3;
					else if 20<= Depression <=27  then dep1 = 4;
						else dep1 = .;

	if QRTYPE = "Married Female" then gender = 1;
    	else if QRTYPE = "Unarried Female" then gender = 1;
    		else if QRTYPE = "Unmarried male" then gender = 0;
    			else gender = .; /* Missing value for unmatched cases */


	if QRTYPE = "Married Female" then marital_status = 1;
    	else if QRTYPE = "Unarried Female" then marital_status = 0;
    		else if QRTYPE = "Unmarried male" then marital_status = 0;
    			else marital_status = .; /* Missing value for unmatched cases */


	if physical = 1 | cyb_bul = 1 | ver_bul = 1 then composite = 1;
		else if physical = 0 & cyb_bul = 0 & ver_bul = 0 then composite = 0;
			else composite = .;
	if uf201 = "Smart phone" | uf201 = "Both smart and basic phone" | uf201 = "Both smart & basic set" then smartphone = 1;
	else smartphone = 0;

	if uf206 = "At least once a week" | uf206 = "Every day" | uf206 = "Less than one a week" then pc = 1;
	else pc = 0;
	if smartphone = 1 | pc = 1 then internet_device = 1;
	else internet_device = 0;

	bully_score = sum(physical, ver_bul, cyb_bul);


label residence = "Residence of the adolecent";
label attend_s = "currently attend school/college";
label dep1 = "Depressions Category";
label gender = "Gender of the Respondent";
label marital_status = " Marital Status of the Respondent";
label composite= "Composite bullying";


RUN;



PROC FREQ DATA = asgar.full;
TABLES QRTYPE;
RUN;

PROC FREQ DATA = asgar.full;
TABLES physical cyb_bul ver_bul uf201 uf206 QHWLTHI ;
RUN;

PROC FREQ DATA = asgar.full;
TABLES physical cyb_bul ver_bul  QHWLTHI;
RUN;

PROC FREQ DATA = asgar.full;
TABLES uf201 uf206/missing;
RUN;




proc freq data = asgar.full;
tables marital_status gender composite dep1 bully_score;
run;






ODS PDF FILE="H:/My Drive/Project/mental health papers/Data/data/dataset for work/Describtive Statistics_output_revised020626.pdf" ;

*Decribtive statistics;

* Table 1: Demographic Profile of study;

TITLE "Table 1: Demographic Profile of study";

PROC FREQ DATA = asgar.full;
TABLES age residence marital_status gender  QRTYPE internet_device QHWLTHI attend_s physical ver_bul cyb_bul bully_score composite QHDIVISION; 
RUN;

PROC FREQ DATA = asgar.full;
TABLES age residence marital_status gender  QRTYPE internet_device QHWLTHI attend_s physical ver_bul cyb_bul bully_score composite QHDIVISION; 
weight weight;
RUN;



/*PROC SURVEYFREQ DATA = asgar.full;*/
/*    CLUSTER QCLUSTER; * Replace with your actual cluster variable;*/
/*    WEIGHT weight;*/
/*    TABLES age residence QHWLTHI attend_s dep1 physical ver_bul cyb_bul;*/
/*RUN;*/


PROC MEANS data = asgar.full;
var Depression ;
weight weight;
run;


/*PROC FREQ DATA = asgar.full;*/
/*TABLES (age residence QHWLTHI attend_s dep_cat)*Depression ; */
/*RUN;*/

* Table 2 : ;

TITLE "TAble 2: ";

PROC ttest DATA = asgar.full;
CLASS residence;
var Depression;
weight weight;
RUN;

PROC ttest DATA = asgar.full;
CLASS attend_s;
var Depression;
weight weight;
RUN;


proc anova data=asgar.full;
    class QHWLTHI;
    model Depression = QHWLTHI;
    means QHWLTHI / tukey; /* Post-hoc test (Tukey) */
run;


proc anova data=asgar.full;
    class age;
    model Depression = age;
    means age / tukey; /* Post-hoc test (Tukey) */
run;

PROC ttest DATA =asgar.full;
Class marital_status;
Var Depression;
weight weight;
RUN;


PROC ttest DATA =asgar.full;
Class gender;
Var Depression;
weight weight;
RUN;
 

PROC ttest DATA =asgar.full;
Class internet_device;
Var Depression;
weight weight;
RUN;



PROC ttest DATA = asgar.full;
CLASS residence;
var Depression;
weight weight;
RUN;
PROC ttest DATA = asgar.full;
CLASS physical;
var Depression;
weight weight;
RUN;
PROC ttest DATA = asgar.full;
CLASS cyb_bul;
var Depression;
weight weight;
RUN;
PROC ttest DATA = asgar.full;
CLASS ver_bul;
var Depression;
weight weight;
RUN;

PROC ttest DATA = asgar.full;
CLASS internet_device;
var Depression;
weight weight;
RUN;


proc anova data=asgar.full;
    class bully_score;
    model Depression = bully_score;
    means bully_score / tukey; /* Post-hoc test (Tukey) */
run;

proc anova data=asgar.full;
    class QRTYPE;
    model Depression = QRTYPE;
    means QRTYPE / tukey; /* Post-hoc test (Tukey) */
run;


PROC SORT data=asgar.full;
by bully_score;
run;


PROC MEANS data = asgar.full;
var Depression;
weight weight;
by bully_score;
run;



PROC SORT data=asgar.full;
by QHDIVISION;
run;


PROC MEANS data = asgar.full;
var Depression;
weight weight;
by QHDIVISION;
run;



PROC SORT data=asgar.full;
by QRTYPE;
run;


PROC MEANS data = asgar.full;
var Depression;
weight weight;
by QRTYPE;
run;



TITLE "Figure 1: Depressions among Ever Married Female, Unmarried Male and Unmarried Female Adolecents";

/**/
/*proc sgplot data= asgar.full;*/
/*    hbox Depression / category=QRTYPE;*/
/*run;*/
/**/
/*proc sgplot data= asgar.full;*/
/*    vbox Depression / category=QRTYPE fillattrs=(color=lightblue)  nooutliers;*/
/*    title "Depressions among Ever Married Female, Unmarried Male and Unmarried Female Adolecents";*/
/*    xaxis label="Survey Respondents";*/
/*    yaxis label="Depressions Score" grid values=(0 to 15 by 2) offsetmin=0.1 offsetmax=0.1;*/
/*run;*/
/**/
/**/
/*proc sgpanel data=asgar.full;*/
/*    panelby QRTYPE / columns=2; /* Creates separate panels for each QRTYPE */*/
/*    hbox Depression ;*/
/*    vbox Depression / category=QRTYPE fillattrs=(color=lightblue) nooutliers name="VBox";*/
/*    title "Depressions among Ever Married Female, Unmarried Male, and Unmarried Female Adolescents";*/
/*    colaxis label="Survey Respondents";*/
/*    rowaxis label="Depression Score" grid values=(0 to 15 by 2);*/
/*run;*/;


ods graphics / reset width=10in height=5in;

ods layout gridded columns=1; /* Create a 2-column layout */

ods region; /* Region for the first plot */
proc sgplot data=asgar.full;
    hbox Depression / fillattrs=(color=lightblue) nooutliers;
    title "";
    xaxis label="Depression Score" grid values=(0 to 15 by 2);
run;

ods region; /* Region for the second plot */
proc sgplot data=asgar.full;
    vbox Depression / category=QRTYPE fillattrs=(color=lightblue) nooutliers;
    title "";
    xaxis label="Survey Respondents";
    yaxis label="Depression Score" grid values=(0 to 15 by 2);
run;

ods layout end; /* End layout */



/*proc sgplot data= asgar.full;*/
/*    vbar physical ver_bul cyb_bul / response= Depression stat=mean datalabel;*/
/*    title "Mean Bar Diagram by Group";*/
/*    xaxis label="Group";*/
/*    yaxis label="Mean Score" grid;*/
/*run;*/



data bullying_long;
    set asgar.full;
    /* Convert binary bullying variables to a long format */
    if physical = 1 then BullyingType = "Physical Bullying";
    else if ver_bul = 1 then BullyingType = "Verbal Bullying";
    else if cyb_bul = 1 then BullyingType = "Cyber Bullying";
    else delete;
    keep BullyingType dep1;
run;

/* Count occurrences of each category */
proc freq data=bullying_long;
    tables BullyingType*dep1/ out=bullying_summary;
run;


/*proc sgplot data=bullying_summary;*/
/*    vbar BullyingType / response=COUNT group=dep1 groupdisplay=cluster*/
/*                         datalabel fillattrs=(transparency=0.2);*/
/*    xaxis label="Bullying Type";*/
/*    yaxis label="Count of Responses" grid;*/
/*    title "Depression Categories by Bullying Type";*/
/*    keylegend / position=bottom; /* Legend at the bottom */*/
/*    styleattrs datacolors=(blue green orange red); /* Custom colors */*/
/*run;*/;

proc freq data=bullying_long noprint;
    tables BullyingType*dep1 / out=bullying_summary_pct outpct;
run;


proc format;
    value depcate_fmt
        0 = "Minimal"
        1 = "Mild"
        2 = "Moderate"
        3 = "Moderately severe"
		4 = "Severe";
run;

data bullying_summary_pct;
    set bullying_summary_pct;
    format dep1 depcate_fmt.;
run;



ods graphics / reset width=8in height=4in imagename="graph_output" imagefmt=png dpi=600;


proc sgplot data=bullying_summary_pct;
    vbar BullyingType / response=PCT_ROW group=dep1 groupdisplay=cluster
                         datalabel fillattrs=(transparency=0.2);
    xaxis label="Bullying Type";
    yaxis label="Percentage (%)" grid;
    title "Depression Categories by Bullying Type (Percentage)";
    keylegend / position=bottom; /* Legend at the bottom */
    styleattrs datacolors=(blue green orange red); /* Custom colors */
run;

ods graphics / reset;


ods graphics on / reset width=8in height=4in imagename="graph_output" imagefmt=tiff dpi=600;

proc sgplot data=bullying_summary_pct;
    vbar BullyingType / response=PCT_ROW group=dep1 groupdisplay=cluster
                         datalabel fillattrs=(transparency=0.2);
    xaxis label="Bullying Type";
    yaxis label="Percentage (%)" grid;
    title "Depression Categories by Bullying Type (Percentage)";
    keylegend / position=bottom; /* Legend at the bottom */
    styleattrs datacolors=(blue green orange red); /* Custom colors */
run;

ods graphics / reset;



proc means data = asgur.full;
var depression;
run;




ODS PDF CLOSE;






/*proc mixed data=asgar.full method=ml; /* ML (Maximum Likelihood) estimation */*/
/*   class QHDIVISION QHDISTRICT;*/
/*   model Depression = physical ver_bul cyb_bul/ solution;*/
/*   random intercept / subject=QHDIVISION;*/
/*   random intercept / subject=QHDISTRICT(QHDIVISION);*/
/*   weight QWEIGHT_UF; /* Observation weights */*/
/*run;*/
;








PROC SORT data=asgar.full;
by QHDISTRICT QCLUSTER;
run;



*Check Null model performance, Based on AIC AICC & BIC values;

proc mixed data=asgar.full method=ml; /* ML (Maximum Likelihood) estimation */
   class QHDISTRICT;
   model Depression = / solution;
   random intercept / subject=QCLUSTER;
   weight weight; /* Observation weights */
run;


proc mixed data=asgar.full method=ml; /* ML (Maximum Likelihood) estimation */
   class QCLUSTER QHDISTRICT;
   model Depression = / solution;
   random intercept / subject=QHDISTRICT;
   random intercept / subject=QCLUSTER(QHDISTRICT);
   weight weight; /* Observation weights */
run;


* Run Best model performance based on 2 level and three level model;


proc mixed data=asgar.full method=ml; /* ML (Maximum Likelihood) estimation */
   class QHDISTRICT;
   model Depression = physical ver_bul cyb_bul/ solution;
   random intercept / subject=QCLUSTER;
   weight weight; /* Observation weights */
run;


proc mixed data=asgar.full1 method=ml; /* ML (Maximum Likelihood) estimation */
   class QCLUSTER QHDISTRICT;
   model Depression = physical ver_bul cyb_bul/ solution;
   random intercept / subject=QHDISTRICT;
   random intercept / subject=QCLUSTER(QHDISTRICT);
   weight weight; /* Observation weights */
run;




* Without Random Effect models;

proc mixed data=asgar.full method=ml; /* ML (Maximum Likelihood) estimation */
   class  QHDIVISION (ref ='1') QHWLTHI (ref ='1') ;
   model Depression = physical ver_bul cyb_bul age QHWLTHI residence attend_s QHDIVISION marital_status gender internet_device/ solution;
   weight weight; /* Observation weights */
run;

ODS PDF FILE="H:/My Drive/Project/mental health papers/Data/data/dataset for work/Describtive Statistics_output_revised080626_Final" ;


* Two level Models;
ods output FitStatistics=fit2;
proc mixed data=asgar.full method=ml; /* ML (Maximum Likelihood) estimation */
   class QCLUSTER  QHDIVISION (ref ='1')  QHWLTHI (ref ='1');
   model Depression = physical ver_bul cyb_bul physical*gender ver_bul*gender cyb_bul*gender age QHWLTHI residence attend_s QHDIVISION marital_status gender internet_device/ solution;
   random intercept / subject=QCLUSTER;
   weight weight; /* Observation weights */
run;

ods output FitStatistics=fit3;
proc mixed data=asgar.full method=ML covtest; /* ML (Maximum Likelihood) estimation */
   class QCLUSTER QHDISTRICT age (ref ='15') QHDIVISION (ref ='1') QHWLTHI (ref ='1');
   model Depression = physical ver_bul cyb_bul physical*gender ver_bul*gender cyb_bul*gender age QHWLTHI residence attend_s QHDIVISION marital_status gender internet_device/ solution;
   random intercept / subject=QHDISTRICT;
   random intercept / subject=QCLUSTER(QHDISTRICT);
   weight weight; /* Observation weights */
run;


Title "Table 3: ";

data lrt;
   merge fit2(rename=(value=LL2))
         fit3(rename=(value=LL3));
   if descr='-2 Log Likelihood';

   LR = LL2 - LL3;
   df = 1;
   pvalue = 1 - probchi(LR,df);
run;

proc print data=lrt;
   var LL2 LL3 LR df pvalue;
run;



Title "Table 4: ";
* Three Level Models;
proc mixed data=asgar.full method=ML; /* ML (Maximum Likelihood) estimation */
   class QCLUSTER QHDISTRICT age (ref ='15') QHDIVISION (ref ='1') QHWLTHI (ref ='1');
   model Depression = physical ver_bul cyb_bul gender physical*gender ver_bul*gender cyb_bul*gender age QHWLTHI residence attend_s QHDIVISION marital_status internet_device/ solution;
   random intercept / subject=QHDISTRICT;
   random intercept / subject=QCLUSTER(QHDISTRICT);
   weight weight; /* Observation weights */
run;




proc mixed data=asgar.full method=ML covtest;
   class QCLUSTER QHDISTRICT age(ref='15')
         QHDIVISION(ref='1')
         QHWLTHI(ref='1');

   model Depression =
         physical
         ver_bul
         cyb_bul
         physical*gender
         ver_bul*gender
         cyb_bul*gender
         age
         QHWLTHI
         residence
         attend_s
         QHDIVISION
         marital_status
         gender
         internet_device
         / solution outp=diag;

   random intercept / subject=QHDISTRICT;
   random intercept / subject=QCLUSTER(QHDISTRICT);

   weight weight;
run;

proc contents data=diag;
run;

proc sgplot data=diag;
   scatter x=Pred y=Resid;
   refline 0 / axis=y;
   loess x=Pred y=Resid;
   xaxis label="Predicted Depression Symptoms score";
   yaxis label="Residual";
   title "Residuals versus predicted values";
run;


proc univariate data=diag normal;
   var Resid;
   qqplot Resid / normal(mu=est sigma=est);
   title "Normal Q-Q plot of residuals";
run;


/*proc mixed data=asgar.full method=ML;*/
/*   class QCLUSTER QHDISTRICT;*/
/**/
/*   model Depression =*/
/*         physical*/
/*         ver_bul*/
/*         cyb_bul*/
/*         age*/
/*         residence;*/
/**/
/*   random intercept / subject=QHDISTRICT solution;*/
/*   random intercept / subject=QCLUSTER(QHDISTRICT) solution;*/
/**/
/*   ods output SolutionR=RandEffects;*/
/*run;*/


/*proc univariate data=RandEffects normal;*/
/*   var Estimate;*/
/**/
/*   qqplot Estimate /*/
/*          normal(mu=est sigma=est);*/
/**/
/*   title "Q-Q Plot of Random Effects";*/
/*run;*/


















Title "Table 5: ";
proc mixed data=asgar.full method=ML; /* ML (Maximum Likelihood) estimation */
   class QCLUSTER QHDISTRICT age (ref ='15') QHDIVISION (ref ='1') QHWLTHI (ref ='1') bully_score (ref ='0');
   model Depression = bully_score gender age QHWLTHI residence attend_s QHDIVISION marital_status gender internet_device/ solution;
   random intercept / subject=QHDISTRICT;
   random intercept / subject=QCLUSTER(QHDISTRICT);
   weight weight; /* Observation weights */
run;


Title " Figure 1: ";

PROC SORT data=asgar.full;
by QRTYPE;
run;


proc mixed data=asgar.full method=ml; /* ML (Maximum Likelihood) estimation */
   by QRTYPE;
   class QCLUSTER QHDISTRICT age (ref ='15') QHDIVISION (ref ='1') QHWLTHI (ref ='1');
   model Depression = physical ver_bul cyb_bul gender age QHWLTHI residence attend_s QHDIVISION internet_device/ solution;
   random intercept / subject=QHDISTRICT;
   random intercept / subject=QCLUSTER(QHDISTRICT);
   weight weight; /* Observation weights */
run;












proc import datafile= "H:/My Drive/Project/mental health papers/Data/data/dataset for work/full missing.dta"
    out= asgar.full_1
    dbms=DTA;
run;


PROC MEANS DATA =asgar.full_1;
VAR Depression;
RUN;




DATA asgar.full_1;
    set asgar.full_1;
    /* Recode string to numeric */
    if QHTYPE = "Other urban" then residence = 1;
    	else if QHTYPE = "Rural" then residence = 1;
    		else if QHTYPE = "Urban" then residence = 0;
    			else residence = .; /* Missing value for unmatched cases */

	if c_attend_school = "No" then attend_s = 0;
	else if c_attend_school = "Yes , madrasa" then attend_s = 1;
		else if c_attend_school = "Yes, school" then attend_s = 1;
			else if c_attend_school = "Yes, school/college" then attend_s = 1;
				else if c_attend_school = "Yes,madrasa" then attend_s = 1;
					else attend_s = .;
	
	if 0<= Depression <=4 then dep1 = 0;
		else if 5<= Depression <=9  then dep1 = 1;
			 else if 10<= Depression <=14  then dep1 = 2;
			 	else if 15<= Depression <=19  then dep1 = 3;
					else if 20<= Depression <=27  then dep1 = 4;
						else dep1 = .;

	if QRTYPE = "Married Female" then gender = 1;
    	else if QRTYPE = "Unarried Female" then gender = 1;
    		else if QRTYPE = "Unmarried male" then gender = 0;
    			else gender = .; /* Missing value for unmatched cases */


	if QRTYPE = "Married Female" then marital_status = 1;
    	else if QRTYPE = "Unarried Female" then marital_status = 0;
    		else if QRTYPE = "Unmarried male" then marital_status = 0;
    			else marital_status = .; /* Missing value for unmatched cases */


	if physical = 1 | cyb_bul = 1 | ver_bul = 1 then composite = 1;
		else if physical = 0 & cyb_bul = 0 & ver_bul = 0 then composite = 0;
			else composite = .;
	if uf201 = "Smart phone" | uf201 = "Both smart and basic phone" | uf201 = "Both smart & basic set" then smartphone = 1;
	else smartphone = 0;

	if uf206 = "At least once a week" | uf206 = "Every day" | uf206 = "Less than one a week" then pc = 1;
	else pc = 0;
	if smartphone = 1 | pc = 1 then internet_device = 1;
	else internet_device = 0;

	bully_score = sum(physical, ver_bul, cyb_bul);


label residence = "Residence of the adolecent";
label attend_s = "currently attend school/college";
label dep1 = "Depressions Category";
label gender = "Gender of the Respondent";
label marital_status = " Marital Status of the Respondent";
label composite= "Composite bullying";

RUN;

TITLE "Table S1: Demographic Profile of study";

PROC FREQ DATA = asgar.full_1;
TABLES age residence marital_status gender  QRTYPE internet_device QHWLTHI attend_s physical ver_bul cyb_bul bully_score composite QHDIVISION; 
RUN;

PROC FREQ DATA = asgar.full_1;
TABLES age residence marital_status gender  QRTYPE internet_device QHWLTHI attend_s physical ver_bul cyb_bul bully_score composite QHDIVISION; 
weight weight;
RUN;
ODS pdf close;
