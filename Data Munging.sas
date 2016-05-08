/*---------------------------AIRBNB DATA-----------------------------*/

libname Airbnb "E:\Pranesh\GMAT\MS in Analytics\UTD\CoursWork\BUAN 6324 -Business Intelligence Using SAS and R\Russell Torress\BI Project\Airbnb";
/* Main training dataset*/
data Airbnb.newtrain;
set SASUSER.TRAIN2;
run;

data newtrain;
set Airbnb.newtrain;
run;
/* Understanding the data*/
proc print data = newtrain(obs = 20);
run;

PROC freq data = newtrain;
table gender;
run;
/* Gender column is unclean..creating binary bins to sort it out*/
data newtrain1(drop = gender);
set newtrain;
if gender = "MALE" then MALE = 1; else MALE = 0;
if gender = "FEMALE" then FEMALE = 1; else FEMALE = 0;
if gender = "OTHER" then OTHERS = 1;else OTHERS = 0;
run;
proc print data = newtrain1(obs = 20);run;
/*Importing the sessions data*/
data sessions;
set SASUSER.SESSIONS;
run;

proc print data = sessions(obs = 20);
run;

/* Joining the 2 tables - Training and Sessions using Left outer Join*/
proc sql;
create table SASUSER.bnbmergeddata as select * from newtrain1 left outer join sessions on newtrain1.id = sessions.user_id;run;
proc print data = SASUSER.bnbmergeddata(obs = 20);run;
/*To create count(Id) amd Sum(Secs_elapsed) columns for analysis*/
proc sql;
create table SASUSER.bnbmergeddata1 as select id, age, signup_method, signup_flow, language, affiliate_channel, affiliate_provider, first_affiliate_tracked, signup_app, first_device_type, first_browser, country_destination, Month_account_created, Date_account_created, Year_account_created, Month_first_Booking, Date_first_Booking, Year_first_Booking, Year_first_active, Month_first_active, MALE, FEMALE, OTHERS, action, action_type, action_detail, device_type, secs_elapsed, count(id) as TotSessions, sum(secs_elapsed) as TotSecs from SASUSER.bnbmergeddata group by id;
run;

proc print data = SASUSER.bnbmergeddata1(obs = 100);
run;

/*Identifying Days required by user for booking..This should provide more information about the user*/
data SASUSER.bnbmergeddata1;
set SASUSER.bnbmergeddata1;
Daysforbooking = (Year_first_Booking-Year_account_created)*365 + (Month_first_Booking-Month_account_created)*30 + (Date_first_Booking-Date_account_created);
if Daysforbooking >= 0 and Daysforbooking <= 2 then BookingType = "Immediate";
if Daysforbooking >= 3 and Daysforbooking <= 5 then BookingType = "Quick";
if Daysforbooking >= 6 then BookingType = "Normal";
if Daysforbooking = "." then BookingType = "Not booked";
run;
proc freq data = SASUSER.bnbmergeddata1;table BookingType;run;
proc freq data = SASUSER.bnbmergeddata1;table country_destination;run;
data SASUSER.bnbmergeddata1;
set SASUSER.bnbmergeddata1;
if age >=10 and age <= 100;
run;


/* Sampling to handle class imbalance and aiding to run the model in miner and base sas*/
proc sort data = SASUSER.bnbmergeddata1; by country_destination;run;

proc surveyselect data = SASUSER.bnbmergeddata1 out = SASUSER.mergesample method = srs samprate = (1,.36,.63,.245,.105,.233,.162,.0087,.65,1,.0083,.05) seed = 12345;
strata country_destination;
run;

/*understanding distribution*/
proc freq data = SASUSER.bnbmergeddata1;table country_destination;run;
proc freq data = SASUSER.bnbmergeddata1;table BookingType;run;
proc freq data = SASUSER.mergesample;table country_destination;run;

data SASUSER.mergesample(drop = SelectionProb SamplingWeight);
set SASUSER.mergesample;
run;

/* Exporting as csv*/
proc export data = SASUSER.bnbmergeddata1 outfile = "E:\Pranesh\GMAT\MS in Analytics\UTD\CoursWork\BUAN 6324 -Business Intelligence Using SAS and R\Russell Torress\BI Project\Airbnb\DataSources\fullmerge1.csv" dbms = csv replace;
run;

proc export data = SASUSER.mergesample outfile = "E:\Pranesh\GMAT\MS in Analytics\UTD\CoursWork\BUAN 6324 -Business Intelligence Using SAS and R\Russell Torress\BI Project\Airbnb\DataSources\samplemerge1.csv" dbms = csv replace;
run;
proc print data = SASUSER.mergesample(obs = 20); run;

/*Creating another sets of data in the Airbnb Library*/
data AIRBNB.bnbmergeddata1;
set SASUSER.bnbmergeddata1;
run;

data AIRBNB.mergesample;
set SASUSER.mergesample;
run;
/*bookingType*/
proc freq data = SASUSER.mergesample;table country_destination;run;
proc freq data = SASUSER.bnbmergeddata1;by country_destination; table Bookingtype;run;

proc print data = SASUSER.mergesample(obs = 100);run;
proc print data = SASUSER.mergesample(obs = 20);run;

/* DV Alteration*/

data SASUSER.dtree(drop = id age Year_first_active Month_first_active action action_detail secs_elapsed TotSessions TotSecs affiliate_channel signup_flow);
set SASUSER.mergesample;;

/* Age Bins*/
if age>=10 and age<=14 then agemod =  "10-14";
if age>=15 and age<=19 then agemod = "15-19";
if age>=20 and age<=24 then agemod = "20-24";
if age>=25 and age<=29 then agemod = "25-29";
if age>=30 and age<=34 then agemod = "30-34";
if age>=35 and age<=39 then agemod = "35-39";
if age>=40 and age<=44 then agemod = "40-44";
if age>=45 and age<=49 then agemod = "45-49";
if age>=50 and age<=54 then agemod = "50-54";
if age>=55 and age<=59 then agemod = "55-59";
if age>=60 and age<=64 then agemod = "60-64";
if age>=65 and age<=69 then agemod = "65-69";
if age>=70 and age<=74 then agemod = "70-74";
if age>=75 and age<=79 then agemod = "75-79";
if age>=80 and age<=84 then agemod = "80-84";
if age>=85 and age<=89 then agemod = "85-89";
if age>=90 and age<=94 then agemod = "90-94";
if age>=95 then agemod = "95+";
run;

proc print data = SASUSER.dtree(obs = 20);run;

/* Partition Sampling*/
proc surveyselect data = SASUSER.dtree method = srs n = 25000 out = LOGIT.sample;
run;
data sample;
set LOGIT.sample;
run;

/* Decision Tree*/
proc hpsplit data = SASUSER.dtree maxdepth = 10 maxbranch = 2;
target country_destination;
input signup_method language affiliate_provider first_affiliate_tracked signup_app first_device_type first_browser Month_account_created Date_account_created Year_account_created Month_first_Booking Date_first_Booking Year_first_Booking MALE FEMALE OTHERS action_type device_type Daysforbooking BookingType agemod /level = nom;
criterion entropy;
partition fraction(validate=0.2);
rules file='hpsplhme2-rules.txt';
output importance=import;
score out=scored2;
run;
















/*Not Required*/
proc freq data = SASUSER.bnbmergeddata;
table action_type action_detail;
run;

/* Understanding Missing Values*/
proc means data = SASUSER.bnbmergeddata nmiss;
run;

/* Month, Year and date of first booking have too mamy missing values(3137212) and also action_detail is not important, hence dropping those variables*/
data SASUSER.bnbmergeddata(drop = Month_first_Booking Date_first_Booking Year_first_Booking action_detail);
set SASUSER.bnbmergeddata;
run;
data mergeairbnb;
set SASUSER.bnbmergeddata;
run;

proc print data = mergeairbnb(obs = 30);
run;
proc means data = mergeairbnb mean var nmiss n;
run;
proc sort data  = mergeairbnb;by age1;
proc freq data = mergeairbnb;
table country_destination;
by age1;
run; 
data mergenonnull;
set mergeairbnb;
if age1 <> '';
RUN;

