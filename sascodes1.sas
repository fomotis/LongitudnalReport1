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
model Haematocrit = age male cardio reject year*age year*male  
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
repeated yearcss/type=ARH(1) subject=id rcorr;
run;

*Reducing the covariance structure not useable;
proc mixed data=long1.Renal3 method=ml;
class id male cardio reject yearcss;
model Haematocrit = year year2 age male cardio reject age*year male*year cardio*year reject*year male*year2/ s;
repeated yearcss/type=AR(1) subject=id rcorr;
run;

*Reducing the covariance structure not useable;
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
repeated yearcss/type=ar(1) subject=id rcorr;
run;
*contrast for male vs female;
proc mixed data=long1.Renal3 method=ml;
class id male cardio reject yearcss;
model Haematocrit = age male cardio reject age*year male*year cardio*year reject*year male*year2/ ddfm=satterth noint s;
contrast 'Evolution of Males vs Females' male 1 -1,
										 male*year 1 -1,
										 male*year2 1 -1 /chisq;
repeated yearcss/type=un subject=id;
run;
*contrast for cardio0 vs cardio0;
proc mixed data=long1.Renal3 method=ml;
class id male cardio reject yearcss;
model Haematocrit = age male cardio reject age*year male*year cardio*year reject*year male*year2/ ddfm=satterth noint s;
contrast 'Evolution of Cardio vs No Cardio' cardio 1 -1,
								            cardio*year 1 -1 /chisq;
repeated yearcss/type=un subject=id;
run;
*contrast for reject0 vs reject1;
proc mixed data=long1.Renal3 method=ml;
class id male cardio reject yearcss;
model Haematocrit = age male cardio reject age*year male*year cardio*year reject*year male*year2/ ddfm=satterth noint s;
contrast 'Evolution of Reject vs No Reject' reject 1 -1,
											reject*year 1 -1 /chisq;
repeated yearcss/type=un subject=id;
run;


******* Some Codes I can't Deduce what I was thinking of Again;
proc means data=long1.Renal3;
var Haematocrit;
run;
proc sgplot data=long1.Renal3;
loess x=year y=Haematocrit;
run;
/* Calculation of residuals, linear average trend */
ods graphics on;
/*proc loess data=long1.Renal3;
model Haematocrit=year;
output out=out r=residual;
run;
ods graphics off;*/
proc glm data=long1.Renal3;
model Haematocrit=year year2;
output out=out r=residual;
run;
data out2;
set out;
residual2 = residual**2;
run;
proc sgplot data=out2;
loess x=year y=residual2;
run;
/* Calculation of the variogram */
proc variogram data=out outpair=out;
coordinates xc=year yc=id;
compute robust novariogram;
var residual;
run;
data variogram;set out;
if y1=y2;vario=(v1-v2)**2/2; run;
data variance;set out;
if y1<y2; vario=(v1-v2)**2/2; run;
/* Calculation of the total variance (=34.8944281) */
proc means data=variance mean;
var vario;
run;

proc loess data=variogram;
ods output scoreresults=out;
model vario=distance;
score data=variogram;
run;

goptions reset=all ftext=swiss device=psepsf
gsfmode=replace rotate=landscape;
proc gplot data=out;
plot vario*distance=1 p_vario*distance=2
/ overlay haxis=axis1 vaxis=axis2 vref=34.8944281 lvref=3;
symbol1 c=red v=dot h=0.2 mode=include;
symbol2 c=black i=join w=2 mode=include;
axis1 label=(h=2 'Time lag') value=(h=1.5)
order=(0 to 20 by 5) minor=none;
axis2 label=(h=2 A=90 'v(u)') value=(h=1.5)
order=(0 to 0.4 by 0.1) minor=none;
title h=3 'Semi-variogram';
run;quit;

*Mixed Model Full;
proc mixed data=long1.Renal3 method=ml order=data;
class male cardio reject yearcss;
model haematocrit = year year2 age male cardio reject 
age*year male*year cardio*year reject*year age*year2 male*year2 cardio*year2 reject*year2/ s;
random intercept year year2 /type=un subject=id g gcorr v vcorr;
repeated yearcss /type=simple subject=id;
run;

*removing cardio*year2;
proc mixed data=long1.Renal3 method=ml order=data;
class male cardio reject yearcss;
model haematocrit = year year2 age male cardio reject 
age*year male*year cardio*year reject*year age*year2 male*year2 reject*year2/ s;
random intercept year year2 /type=un subject=id g gcorr v vcorr;
repeated yearcss /type=simple subject=id;
run;
*remove reject by year2 not removed;
proc mixed data=long1.Renal3 method=ml order=data;
class male cardio reject yearcss;
model haematocrit = year year2 age male cardio reject 
					age*year male*year cardio*year reject*year 
					age*year2 male*year2/ s;
random intercept year year2 /type=un subject=id g gcorr v vcorr;
repeated yearcss /type=simple subject=id;
run;
*remove age by year2 (not removed);
proc mixed data=long1.Renal3 method=ml order=data;
class male cardio reject yearcss;
model haematocrit = year year2 age male cardio reject 
					age*year male*year cardio*year reject*year 
					male*year2 reject*year2/ s;
random intercept year year2 /type=un subject=id g gcorr v vcorr;
repeated yearcss /type=simple subject=id;
run;
*remove male by year2 (not removed);
proc mixed data=long1.Renal3 method=ml order=data;
class male cardio reject yearcss;
model haematocrit = year year2 age male cardio reject 
					age*year male*year cardio*year reject*year 
					reject*year2/ s;
random intercept year year2 /type=un subject=id g gcorr v vcorr;
repeated yearcss /type=simple subject=id;
run;
*remove cardio by year;
proc mixed data=long1.Renal3 method=ml order=data;
class male cardio reject yearcss;
model haematocrit = year year2 age male cardio reject 
					age*year male*year reject*year 
					male*year2 reject*year2/ s;
random intercept year year2 /type=un subject=id g gcorr v vcorr;
repeated yearcss /type=simple subject=id;
run;
*final model;
*remove age by year2 (not removed);
proc mixed data=long1.Renal3 method=reml order=data empirical;
class male cardio reject yearcss;
model haematocrit = year year2 age male cardio reject 
					age*year male*year cardio*year reject*year 
					male*year2 reject*year2/ s;
random intercept year year2 /type=un subject=id g gcorr v vcorr;
*ods listing exclude solutionr;
*ods output solutionr=out;
repeated yearcss /type=simple subject=id;
run;

*final model mixed model;
proc mixed data=long1.Renal3 method=reml order=data empirical;
class male cardio reject yearcss;
model haematocrit = age male cardio reject 
					age*year male*year cardio*year reject*year 
					male*year2 reject*year2/ noint s;
random intercept year year2 /type=un subject=id g gcorr v vcorr;
*ods listing exclude solutionr;
*ods output solutionr=out;
repeated yearcss /type=simple subject=id;
contrast 'Evolution of Males vs Females' male 1 -1,
										 male*year 1 -1,
										 male*year2 1 -1 /chisq;
run;
*contrasts for evolution cardio;
proc mixed data=long1.Renal3 method=reml order=data empirical;
class male cardio reject yearcss;
model haematocrit = age male cardio reject 
					age*year male*year cardio*year reject*year 
					male*year2 reject*year2/ noint s;
random intercept year year2 /type=un subject=id g gcorr v vcorr;
*ods listing exclude solutionr;
*ods output solutionr=out;
repeated yearcss /type=simple subject=id;
contrast 'Evolution of Cardio' cardio 1 -1,
							   cardio*year 1 -1 /chisq;
run;
*contrasts for evolution cardio;
proc mixed data=long1.Renal3 method=reml order=data empirical;
class male cardio reject yearcss;
model haematocrit = age male cardio reject 
					age*year male*year cardio*year reject*year 
					male*year2 reject*year2/ noint s;
random intercept year year2 /type=un subject=id g gcorr v vcorr;
*ods listing exclude solutionr;
*ods output solutionr=out;
repeated yearcss /type=simple subject=id;
contrast 'Evolution of Cardio' reject 1 -1,
							   reject*year 1 -1 ,
							   reject*year2 1 -1/chisq;
run;
*no vs 1 random effect;
proc mixed data=long1.Renal3 method=reml order=data empirical;
class male cardio reject yearcss;
model haematocrit = age male cardio reject 
					age*year male*year cardio*year reject*year 
					male*year2 reject*year2/ noint s;
*random intercept year year2 /type=un subject=id g gcorr v vcorr;
*ods listing exclude solutionr;
*ods output solutionr=out;
repeated yearcss /type=simple subject=id;
*contrast 'Evolution of Cardio' reject 1 -1,
							   reject*year 1 -1 ,
							   reject*year2 1 -1/chisq;
run;
*1 random effect;
proc mixed data=long1.Renal3 method=reml order=data empirical;
class male cardio reject yearcss;
model haematocrit = age male cardio reject 
					age*year male*year cardio*year reject*year 
					male*year2 reject*year2/ noint s;
random intercept/type=un subject=id g gcorr v vcorr;
*ods listing exclude solutionr;
*ods output solutionr=out;
repeated yearcss /type=simple subject=id;
*contrast 'Evolution of Cardio' reject 1 -1,
							   reject*year 1 -1 ,
							   reject*year2 1 -1/chisq;
run;





data int slope slope2;
set out(keep=Effect Subject Estimate);
if Effect='Intercept' THEN output int;
else if Effect='year' THEN output slope;
else if Effect='year2' THEN output slope2;
run;

proc sort data=int;
by Subject;
run;
proc sort data=slope;
by Subject;
run;
proc sort data=slope2;
by Subject;
run;



data merged;
merge int slope slope2;
by Subject;
run;
*plot of two stage analysis;

data twostage;
infile "C:/Users/OODOOE/Downloads/Video/Second Year/First Semester/Longitudnal Data Analysis/LongitudnalReport1/randomintslope.csv" dlm=',' firstobs=2;
input intercept time time2;
run;

proc sgscatter data=twostage;
matrix _numeric_ /diagonal=(kernel histogram);
run;


















