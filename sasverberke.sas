/* Macro for the informal check whether or not observed longitudinal profiles can be well
   described by a specific linear regression function

   Reference: Verbeke and Molenberghs (2000), New-York: Springer-Verlag, Section 4.3

   Written by : Geert Verbeke, Biostatistical Centre, K.U.Leuven
   Date : December 1, 1999
   Changed : November 2, 2000 : Allow subjects with constant response. Their R^2 measure
                                is set equal to 1

   Input : INDATA : input data set, which does not contain any missing values in the
                    variables to be used in this macro
           OUTDATA : output data set, which contains 3 variables. The first variable contains
                     the identification number of each subject. The second variable
                     is the coefficient of multiple determination for each subject.
                     The third variable contains the number of
                     repeated measurements on which the calculation was based
           Y : response variable
           ID : numeric identification variable for the subjects in the input data set
           X1 : covariates needed to model each profile. Note that no intercept term will
                be included by default. If an intercept term is required, it should be
                explicitly included via a variable which contains only ones
           X2 : a set of variables not included in X1, for which it will be tested using
                an overall F-test whether or not these variables are needed for describing
                the observed longitudinal profiles

   Output : (1) an overall F-test is performed to test whether the covariates in X2 are
                needed to describe the observed longitudinal profiles, in addition to the
                covariates already in X1
            (2) an overall coefficient of multiple determination is calculated
            (3) the output data set OUTDATA is created */


%macro gof(INDATA =  ,
           OUTDATA = ,
           Y =  ,
           ID = ,
           X1 =  ,
           X2 =  );

proc sort data = &indata;
by &id;
run;
proc freq data = &indata;
tables &id / out=uit  noprint ;
run;
proc iml;
use uit;
read all var {count} into aantal;
close uit;
use &indata;
labelx1 = {&X1};
labelx2 = {&X2};
labely = {&Y};
labelid = {&id};
read all var labelid into id;
read all var labely into y;
read all var labelx1 into x1;
read all var labelx2 into x2;
close &indata;

p=ncol(x1)+ncol(x2);
ftel1 = 0;
fnoem1 = 0;
dftel1 = 0;
dfnoem1 = 0;
begin = 1;
do i = 1 to nrow(aantal);
   ni = aantal[i,];
   einde = begin + ni - 1;
   if ni >= p then do;
      x1i = x1[(begin:einde),];
      x2i = x2[(begin:einde),];
      yi = y[(begin:einde),];
      xi = x1i||x2i;
      ri = yi-xi*(inv(xi`*xi))*xi`*yi;
      rHi = yi - x1i*(inv(x1i`*x1i))*x1i`*yi;
      rssi = ri`*ri;
      rssHi = rHi`*rHi;
      ftel1 = ftel1 + (rssHi - rssi);
      fnoem1 = fnoem1 + rssi;
      dftel1 = dftel1 + ncol(x2);
      dfnoem1 = dfnoem1 + (ni-p);
   end;
   begin = einde + 1;
end;
f = (ftel1/dftel1)/(fnoem1/dfnoem1);
c = {"F"  "ndf" "ddf" "p-value"};
F_test = (F||dftel1||dfnoem1||(1-probf(f,dftel1,dfnoem1)));
print F_test[colname=c format=10.4];


p=ncol(x1);
R = 0||0||0;
ssto = 0;
ssr = 0;
begin = 1;
do i = 1 to nrow(aantal);
   ni = aantal[i,];
   einde = begin + ni - 1;
   if ni >= p then do;
      idi = id[begin,];
      x1i = x1[(begin:einde),];
      yi = y[(begin:einde),];
      ri = yi - x1i*(inv(x1i`*x1i))*x1i`*yi;
      ssei = ri`*ri;
      sstoi = (yi-(j(1,ni)*yi/ni))`*(yi-(j(1,ni)*yi/ni));
      ssri = sstoi-ssei;
      ssto = ssto+sstoi;
      ssr = ssr+ssri;
      if sstoi > 0 then do;
         r = r//(idi||(ssri/sstoi)||ni);
      end;
      else do;
         r = r//(idi||1||ni);
      end;
   end;
   else do;
      idi = id[begin,];
      yi = y[(begin:einde),];
      sstoi = (yi-(j(1,ni)*yi/ni))`*(yi-(j(1,ni)*yi/ni));
      ssto = ssto+sstoi;
      ssr = ssr+sstoi;
      r = r//(idi||1||ni);
   end;
   begin = einde + 1;
end;
Tot_R2=ssr/ssto;
c = {"R2"};
print Tot_R2[colname=c format=10.4];
r=r[2:nrow(r),];
naam = labelid||"R2"||"ni";
create &outdata var naam;
append from r;
quit;
run;
%mend;
