libname long1 "C:/Users/OODOOE/Downloads/Video/Second Year/First Semester/Longitudnal Data Analysis/LongitudnalReport1/Data";
proc transpose data=long1.renal name=Time out=long1.renal2;
  by id age male cardio reject;
run;
*creating the class version of time;
data long1.renal3(drop=Time);
set long1.renal2(rename=(col1=Haematocrit));
if Time='HC06' THEN Time='HC0.5';
year=input(substr(Time, 3), 5.);
  logHc=log(Haematocrit);
  *class version of year;
yearcss = year;
*year squared;
year2 = year*year;
run;
*stupid full model;
proc mixed data=long1.Renal3 method=ml;
class id male cardio reject year year2;
model Haematocrit = year*age year*male year*cardio year*reject 
year2*age year2*male year2*cardio year2*reject/ noint s ;
repeated year/type=un subject=id rcorr;
run; 
*stupid full modelb;
proc mixed data=long1.Renal3 method=ml;
class id male cardio reject year year2;
model Haematocrit = age cardio reject year year2 year*age year*male year*cardio year*reject 
year2*age year2*male year2*cardio year2*reject/ s ;
repeated year/type=un subject=id rcorr;
run;
*Here the real work begins;
*full model1a;
proc mixed data=long1.Renal3 method=ml;
class id male cardio reject yearcss;
model Haematocrit = age male cardio reject year year2 year*age year*male  
year*cardio year*reject year2*age year2*male year2*reject year2*cardio/ s;
repeated yearcss/type=un subject=id rcorr;
run; 
*full model1b;
proc mixed data=long1.Renal3 method=ml;
class id male cardio reject yearcss;
model Haematocrit =  age male cardio reject year*age year*male  
year*cardio year*reject year2*age year2*male year2*reject year2*cardio/ s;
repeated yearcss/type=un subject=id rcorr;
run; 
*reducing the mean structure;
*kicking out quadratic cardio time;
proc mixed data=long1.Renal3 method=ml;
class id male cardio reject yearcss;
model Haematocrit = age male cardio reject year year2 year*age year*male  
year*cardio year*reject year2*age year2*male year2*reject/ s;
repeated yearcss/type=un subject=id;
run; 
*kicking out quadratic reject time;
proc mixed data=long1.Renal3 method=ml;
class id male cardio reject yearcss;
model Haematocrit = age male cardio reject year year2 year*age year*male  
year*cardio year*reject year2*age year2*male/ s;
repeated yearcss/type=un subject=id;
run; 
*kicking out quadratic age time;
proc mixed data=long1.Renal3 method=ml;
class id male cardio reject yearcss;
model Haematocrit = age male cardio reject year year2 year*age year*male  
year*cardio year*reject year2*male/ s;
repeated yearcss/type=un subject=id;
run; 
*kicking out quadratic male time cannot be removed;
proc mixed data=long1.Renal3 method=ml;
class id male cardio reject yearcss;
model Haematocrit = age male cardio reject year year2 year*age year*male year*cardio year*reject/ s;
repeated yearcss/type=un subject=id;
run; 
*kicking out cardio time;
proc mixed data=long1.Renal3 method=ml;
class id male cardio reject yearcss;
model Haematocrit = age male cardio reject year year2 year*age year*male year*reject year2*male/ s;
repeated yearcss/type=un subject=id;
run;
*kicking out reject time;
proc mixed data=long1.Renal3 method=ml;
class id male cardio reject yearcss;
model Haematocrit = age male cardio reject year year2 year*age year*male year2*male/ s;
repeated yearcss/type=un subject=id;
run;
*kicking out age time;
proc mixed data=long1.Renal3 method=ml;
class id male cardio reject yearcss;
model Haematocrit = age male cardio reject year year2 year*male year2*male/ s;
repeated yearcss/type=un subject=id;
run;
*kicking out male time (not kicked out);
proc mixed data=long1.Renal3 method=ml;
class id male cardio reject yearcss;
model Haematocrit = age male cardio reject year year2 year2*male/ s;
repeated yearcss/type=un subject=id;
run;
*kicking out cardio;
proc mixed data=long1.Renal3 method=ml;
class id male cardio reject yearcss;
model Haematocrit = year year2 age male reject/ s;
repeated yearcss/type=un subject=id;
run;
*kicking out cardio;
proc mixed data=long1.Renal3 method=ml;
class id male cardio reject yearcss;
model Haematocrit = year year2 age male/ s;
repeated yearcss/type=un subject=id;
run;
*kicking out male;
proc mixed data=long1.Renal3 method=ml;
class id male cardio reject yearcss;
model Haematocrit = year year2 age/ s;
repeated yearcss/type=un subject=id;
run;
*try out growth curves;
libname diss6 v6 "C:/Users/OODOOE/Downloads/Video/Second Year/First Semester/Longitudnal Data Analysis/LongitudnalReport1/Data";
data trial;
set diss6.Growthax;
agecss = age;
sex2 = sex - 1;
run;

proc mixed data=trial method=ml;
class idnr agecss sex2;
model measure = sex2 age*sex2 /s;
repeated agecss /type=un subject=idnr;
run;

proc mixed data=trial method=ml;
class idnr age sex2;
model measure = age*sex2 / noint s;
repeated age /type=un subject=idnr;
run;

*Final Multivariate Regression Model for Question 2 Mean Structure;
proc mixed data=long1.Renal3 method=ml;
class id male cardio reject yearcss;
model Haematocrit = age male cardio reject year year2 year*age year*male year*cardio reject*year male*year2/ s;
repeated yearcss/type=un subject=id;
run;

*Reducing the covariance structure not useable;
proc mixed data=long1.Renal3 method=ml;
class id male cardio reject yearcss;
model Haematocrit = year year2 age male cardio reject age*year male*year cardio*year reject*year male*year2/ s;
repeated yearcss/type=toep subject=id rcorr;
run;

*Reducing the covariance structure;
proc mixed data=long1.Renal3 method=ml;
class id male cardio reject yearcss;
model Haematocrit = year year2 age male cardio reject age*year male*year cardio*year reject*year male*year2/ s;
repeated yearcss/type=AR(1) subject=id rcorr;
run;

*Reducing the covariance structure;
proc mixed data=long1.Renal3 method=ml;
class id male cardio reject yearcss;
model Haematocrit = year year2 age male cardio reject age*year male*year cardio*year reject*year male*year2/ s;
repeated yearcss/type=ARH(1) subject=id rcorr;
run;

*Reducing the covariance structure;
proc mixed data=long1.Renal3 method=ml;
class id male cardio reject yearcss;
model Haematocrit = year year2 age male cardio reject age*year male*year cardio*year reject*year male*year2/ s;
repeated yearcss/type=TOEPH subject=id rcorr;
run;

*Reducing the covariance structure;
proc mixed data=long1.Renal3 method=ml;
class id male cardio reject yearcss;
model Haematocrit = year year2 age male cardio reject age*year male*year cardio*year reject*year male*year2/ s;
repeated yearcss/type=TOEPH subject=id rcorr;
run;

*Reducing the covariance structure;
proc mixed data=long1.Renal3 method=ml;
class id male cardio reject yearcss;
model Haematocrit = year year2 age male cardio reject age*year male*year cardio*year reject*year male*year2/ s;
repeated yearcss/type=cs subject=id rcorr;
run;

*Reducing the covariance structure;
proc mixed data=long1.Renal3 method=ml;
class id male cardio reject yearcss;
model Haematocrit = year year2 age male cardio reject age*year male*year cardio*year reject*year male*year2/ s;
repeated yearcss/type=csh subject=id rcorr;
run;

*Reducing the covariance structure;
proc mixed data=long1.Renal3 method=ml;
class id male cardio reject yearcss;
model Haematocrit = year year2 age male cardio reject age*year male*year cardio*year reject*year male*year2/ s;
repeated yearcss/type=un(1) subject=id rcorr;
run;

proc means data=long1.Renal3;
var Haematocrit;
run;















