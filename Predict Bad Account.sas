libname Data "C:\Users\HP\Downloads\data science\SAS\Data";
* Splitting the data into train and test dataset;
data Train_Data Test_Data;
  set Data.hmeq;
  if ranuni(1234567)<=0.6 then output Train_Data; 
  else 								 output Test_Data; 
run;
*Display contents of train dataset;
proc contents data=Train_Data;
run;
* Cross table;
proc freq data=Train_Data;
   tables bad reason job;
run;
* Barchart of Loan by Years of job;
proc gchart data=Train_Data;
 vbar Yoj / discrete type=mean
 sumvar=Loan mean;
run;
quit;
*Converting Categorical variables into numeric;
data Train_Data(drop=job reason);
	set Train_Data;
	Job_Mgr=(Job='Mgr');
	Job_Office=(Job='Office');
	Job_Other=(Job='Other');
	Job_ProfExe=(Job='ProfExe');
	Job_Sales=(Job='Sales');
	Job_Self=(Job='Self');
	Job_miss=(Job=' ');
   Reason_DebtCon=(Reason='DebtCon');
	Reason_HomeImp=(Reason='HomeImp');
	Reason_Miss=(Reason=' ');
   run;
proc contents data=Train_Data;
run;
* For Fixing Missing Values, creating tables and graphs to figure out values to replace for missing data;
%Let all_var=
clage
clno
debtinc
delinq
derog
loan
mortdue
ninq
value
yoj
;
%LET DSN=Train_Data;
%LET RESP=BAD;
%LET GROUPS=10;

%MACRO LOGTCONT                         ;
      OPTIONS CENTER PAGENO=1 DATE;
	  data test;
	    set &DSN;
	  run;
	  %do i=1 %to 10;
	  %LET VBLE=%scan(&all_var, &i);  
       PROC RANK DATA =TEST (KEEP=&RESP &VBLE)
               GROUPS = &GROUPS
                  OUT = TEMP1     ;
            RANKS NEWVBLE         ;
            VAR &VBLE             ;
       RUN                        ;

       PROC SUMMARY DATA = TEMP1 NWAY ;
            CLASS NEWVBLE             ;
            VAR &RESP &VBLE           ;
            OUTPUT OUT = TEMP2
                  MEAN =
                  MIN(&VBLE)=MIN
                  MAX(&VBLE)=MAX
                     N = NOBS         ;
       RUN                            ;

       DATA TEMP2                     ;
            SET TEMP2                 ;
            IF &RESP NE 0 THEN
               LOGIT = LOG ( &RESP / (1- &RESP) ) ;
            ELSE IF &RESP = 0 THEN LOGIT = .       ;
       RUN;

       PROC SQL NOPRINT;
        CREATE TABLE TEMP3 AS
        SELECT 99 AS NEWVBLE, COUNT(*) AS NOBS, MEAN(&RESP) AS &RESP
        FROM test
        WHERE &VBLE=.
       ;

       DATA TEMP3;
        SET TEMP3;
        LOGIT=LOG(&RESP/(1-&RESP));
       RUN;


       DATA TEMP4;
        SET TEMP2 TEMP3;
       RUN;
	

       PROC PLOT DATA = TEMP4         ;
            TITLE1 "Plot of Logit(Response) by &&VBLE" ;
            PLOT  LOGIT* &VBLE        ;
       RUN  ;


        proc plot data=TEMP4;
        plot &resp*&vble;
        plot _freq_*&vble;
        TITLE2 "Plot of Response by &&VBLE" ;
        run;
		proc sort data=TEMP4 out=TEMP4;
		by Bad;
		run;
       PROC PRINT DATA = TEMP4 LABEL SPLIT = '*' NOOBS ;
            TITLE3 "Table of Response by Grouped &&VBLE" ;
            VAR NEWVBLE NOBS &VBLE MIN MAX &RESP ;
            LABEL NEWVBLE = "&&VBLE Grouping"
                     NOBS = '# of*Records'
                     LOGIT = "Logit of Response"
                     MIN   ='MIN'
                     MAX   ='MAX'                       ;
       RUN                                             ;

	   %end;


%MEND LOGTCONT      ;
%LOGTCONT      ;

*Filling missing values with the nearest values calculated above;
Data Train_Data1;
	set Train_Data;
  if CLAGE=. then CLAGE=94.325;
  if CLAGE>295 then CLAGE=295;
  if CLNO<10 then CLNO=0;
  if CLNO=. then CLNO=31.3878;
  if CLNO<15 then CLNO=15;
     DEBTINC_MISS=(DEBTINC=.);
  if DEBTINC=. then DEBTINC=45.4824;
  if DELINQ=. then DELINQ=0;
  if DEROG=. then DEROG=0;
  if MORTDUE=. then MORTDUE=54175.95;
  if NINQ=. then NINQ=0;
     VALUE_MISS=(VALUE=.);
  if YOJ=. then YOJ=25.1988;
  run;

PROC CONTENTS DATA=Train_Data;
RUN;

%LET INPUT2=

JOB_Mgr
JOB_Office
JOB_Other
JOB_ProfExe
JOB_Sales
JOB_Self
JOB_miss
REASON_DebtCon
REASON_HomeImp
REASON_Miss
VALUE_MISS
clage
clno
delinq
derog
loan
mortdue
ninq
yoj
;
* Applying the logistic regression;
proc logistic data=Train_Data1 descending;
model bad=&input2
  /selection=stepwise fast lackfit rsquare corrb stb;
run;

%LET INPUT3=
DEBTINC_MISS
JOB_Office
JOB_Other
JOB_Sales
JOB_Self
JOB_miss
REASON_HomeImp
VALUE_MISS
clage
clno
delinq
derog
ninq
yoj
;

proc logistic data=Train_Data descending;
model bad=&input3
  /selection=stepwise fast lackfit rsquare corrb stb;
run;


%LET INPUT4=
DEBTINC_MISS
JOB_Office
JOB_Sales
JOB_miss
REASON_HomeImp
VALUE_MISS
clage
clno
delinq
derog
ninq
yoj
;

proc logistic data=Train_Data1 descending;
model bad=&input4
  /selection=stepwise fast lackfit rsquare corrb stb;
run;

* Test the model with test data;
* 1. Create logit function and calculate probability using p=1/(1+exp(-logit));
data Test_Data1 (drop=JOB REASON);
  set Test_Data;
  
  JOB_Mgr=(JOB='Mgr');
  JOB_Office=(JOB='Office');
  JOB_Other=(JOB='Other');
  JOB_ProfExe=(JOB='ProfExe');
  JOB_Sales=(JOB='Sales');
  JOB_Self=(JOB='Self');
  JOB_miss=(JOB=' ');
  REASON_DebtCon=(REASON='DebtCon');
  REASON_HomeImp=(REASON='HomeImp');
  REASON_Miss=(REASON=' ');
  
  if CLAGE=. then CLAGE=94.325;
  if CLAGE>295 then CLAGE=295;
  if CLNO<10 then CLNO=0;
  if CLNO=. then CLNO=31.3878;
  if CLNO<15 then CLNO=15;
     DEBTINC_MISS=(DEBTINC=.);
  if DEBTINC=. then DEBTINC=45.4824;
  if DELINQ=. then DELINQ=0;
  if DEROG=. then DEROG=0;
  if MORTDUE=. then MORTDUE=54175.95;
  if NINQ=. then NINQ=0;
     VALUE_MISS=(VALUE=.);
  if YOJ=. then YOJ=25.1988;

Logit=
-1.4968		
+2.6920		*	DEBTINC_MISS	/*Debt to income ratio IS MISSING */
-0.4873		*	JOB_Office		/*Office Job*/
+0.8347		*	JOB_Sales		/*Sales Job*/
-1.9472		*	JOB_miss			/*Job Missing*/
+4.4237		*	VALUE_MISS		/*Value of current property IS MISSING */
-0.00752		*	clage			/*Age of oldest trade line in months*/
+0.6784		*	delinq			/*Number of delinquent trade lines*/
+0.5819		*	derog			/*Number of major derogatory reports*/
+0.1157		*	ninq			/*Number of recent credit inquiries*/
-0.0251		*	yoj				/*Years on current job*/
;

prob=1/(1+exp(-logit)); 
run;
* Sorting the test data by descending probabilities;
proc sort data=Test_Data1 out=Test_Data1;
   by descending prob;
run;
*Ranking;
proc rank		data = Test_Data1
				out = Test_ranked
				groups = 20
				descending;
		var		prob;
		ranks	rank;
run;
*changing ranks from 0-19 to 1-20;
data Test_ranked(drop=rank prob);
set Test_ranked;
	model_rank=rank + 1;
	model_score=prob;
	
run;
* Creating the result table;
PROC TABULATE DATA = Test_ranked MISSING NOSEPS;
            CLASS model_rank        ;
            VAR   model_score BAD;
	TABLES model_rank ALL, model_score*MEAN*F=5.3 bad='BAD'*(sum='# of Bad' n='# of Acct' mean*F=5.3);
RUN;
