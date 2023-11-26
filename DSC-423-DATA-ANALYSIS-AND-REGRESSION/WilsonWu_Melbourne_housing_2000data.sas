title "Melbourne_housing_2000data";
data Melbourne_housing_2000data;
infile "Melbourne_housing_2000data_WilsonWU.csv" firstobs=2 delimiter=',';
input Suburb$ Address$ Rooms Type$ Price Method$ SellerG$ Date$ Distance Postcode 
Bedroom2 Bathroom Car Landsize BuildingArea YearBuilt CouncilArea$ Lattitude Longtitude Regionname$ Propertycount;
ln_Price=log(Price);
ln_Landsize =log(Landsize);
ln_BuildingArea =log(BuildingArea);
ln_Propertycount =log(Propertycount);
ln_Distance = log(Distance);
d_Type_h =(Type="h");
d_Type_u =(Type="u");
d_Type_t =(Type="t");
d_Type_devsite =(Type="dev site");
d_Type_ores =(Type="o res");
d_Method_SP =(Method="SP");
d_Method_PI =(Method="PI");
d_Method_PN =(Method="PN");
d_Method_SN =(Method="SN");
d_Method_NB =(Method="NB");
d_Method_VB  =(Method="VB");
d_Method_W =(Method="W");
d_Method_SA =(Method="SA");
d_Method_SS =(Method="SS");
d_Method_NA =(Method="N/A");
Bedroom2_Bathroom =Bedroom2*Bathroom;
run;
proc print;
run;

title"Historgram w/Price data=Melbourne_housing2200smaple"; 
proc univariate;
var Price; 
histogram/normal;
run;

title"Historgram w/Rooms data=Melbourne_housing2200smaple"; 
proc univariate;
var Rooms; 
histogram/normal;
run;

title"Historgram w/Bathroom data=Melbourne_housing2200smaple"; 
proc univariate;
var Bathroom; 
histogram/normal;
run;

title"Historgram w/car data=Melbourne_housing2200smaple"; 
proc univariate;
var car; 
histogram/normal;
run;

title"Historgram w/Landsize data=Melbourne_housing2200smaple"; 
proc univariate;
var Landsize; 
histogram/normal;
run;

title"Historgram w/BuildingArea data=Melbourne_housing2200smaple"; 
proc univariate;
var BuildingArea; 
histogram/normal;
run;
title"Historgram w/Propertycount data=Melbourne_housing2200smaple"; 
proc univariate;
var Propertycount; 
histogram/normal;
run;

title"Historgram w/distance  data=Melbourne_housing2200smaple"; 
proc univariate;
var Distance; 
histogram/normal;
run;

title"Historgram w/ln_Price data=Melbourne_housing2200smaple"; 
proc univariate;
var ln_Price; 
histogram/normal;
run;

title"Historgram w/ln_Landsize data=Melbourne_housing2200smaple"; 
proc univariate;
var ln_Landsize; 
histogram/normal;
run;

title"Historgram w/ln_BuildingArea data=Melbourne_housing2200smaple"; 
proc univariate;
var ln_BuildingArea; 
histogram/normal;
run;

title"Historgram w/ln_Propertycount data=Melbourne_housing2200smaple"; 
proc univariate;
var ln_Propertycount; 
histogram/normal;
run;

title"Historgram w/ln_distance  data=Melbourne_housing2200smaple"; 
proc univariate;
var ln_Distance; 
histogram/normal;
run;

title "GPLOTS for Y and X-variables data=Melbourne_housing2200smaple";
proc gplot data=Melbourne_housing_2000data;
plot ln_Price*(Rooms Distance Bedroom2 Bathroom Car ln_Landsize BuildingArea Propertycount 
d_Type_h d_Type_u d_Type_t d_Type_devsite d_Type_ores d_Method_SP d_Method_PI d_Method_PN 
d_Method_SN d_Method_NB d_Method_VB d_Method_W d_Method_SA d_Method_SS d_Method_NA Bedroom2_Bathroom) ;
run;

title "Scatterplot Matrix data=Melbourne_housing2200smaple";
proc sgscatter data=Melbourne_housing_2000data;
matrix ln_Price Rooms Distance Bedroom2 Bathroom Car ln_Landsize BuildingArea Propertycount 
d_Type_h d_Type_u d_Type_t d_Type_devsite d_Type_ores d_Method_SP d_Method_PI d_Method_PN 
d_Method_SN d_Method_NB d_Method_VB d_Method_W d_Method_SA d_Method_SS d_Method_NA Bedroom2_Bathroom;
run;

title "Pearson Correlation Matrix data=Melbourne_housing2200smaple";
proc corr data=Melbourne_housing_2000data;
var ln_Price Rooms Distance Bedroom2 Bathroom Car ln_Landsize BuildingArea Propertycount 
d_Type_h d_Type_u d_Type_t d_Type_devsite d_Type_ores d_Method_SP d_Method_PI d_Method_PN 
d_Method_SN d_Method_NB d_Method_VB d_Method_W d_Method_SA d_Method_SS d_Method_NA Bedroom2_Bathroom;
run;

title "Full Regression Model for ln_Price with VIF data=Melbourne_housing2200smaple";
proc reg data=Melbourne_housing_2000data;
model ln_Price = Rooms Distance Bedroom2 Bathroom Car ln_Landsize BuildingArea Propertycount 
d_Type_h d_Type_u d_Type_t d_Type_devsite d_Type_ores d_Method_SP d_Method_PI d_Method_PN 
d_Method_SN d_Method_NB d_Method_VB d_Method_W d_Method_SA d_Method_SS d_Method_NA Bedroom2_Bathroom /vif tol;
plot student.*predicted.;
plot npp.*student.;
run;

title "New Pearson Correlation Matrix data=Melbourne_housing2200smaple";
proc corr data=Melbourne_housing_2000data;
var ln_Price Distance Bedroom2 Bathroom Car ln_Landsize BuildingArea Propertycount 
d_Type_h d_Type_u d_Type_t d_Type_devsite d_Type_ores d_Method_SP d_Method_PI d_Method_PN 
d_Method_SN d_Method_NB d_Method_VB d_Method_W d_Method_SA d_Method_SS d_Method_NA Bedroom2_Bathroom;
run;

title "New Full Regression Model for ln_Price with VIF data=Melbourne_housing2200smaple";
proc reg data=Melbourne_housing_2000data;
model ln_Price = Distance Bedroom2 Bathroom Car ln_Landsize BuildingArea Propertycount 
d_Type_h d_Type_u d_Type_t d_Type_devsite d_Type_ores d_Method_SP d_Method_PI d_Method_PN 
d_Method_SN d_Method_NB d_Method_VB d_Method_W d_Method_SA d_Method_SS d_Method_NA Bedroom2_Bathroom /vif tol;
plot student.*predicted.;
plot npp.*student.;
run;

title "New1 Full Regression Model for ln_Price with VIF data=Melbourne_housing2200smaple";
proc reg data=Melbourne_housing_2000data;
model ln_Price = Distance Bedroom2 Bathroom Car ln_Landsize BuildingArea Propertycount 
d_Type_h d_Type_u d_Type_t d_Type_devsite d_Type_ores d_Method_SP d_Method_PI d_Method_PN 
d_Method_SN d_Method_NB d_Method_VB d_Method_W d_Method_SA d_Method_SS d_Method_NA  /vif tol;
plot student.*predicted.;
plot npp.*student.;
run;

title "New2 Full Regression Model for ln_Price with VIF data=Melbourne_housing2200smaple";
proc reg data=Melbourne_housing_2000data;
model ln_Price = Distance Bedroom2 Car ln_Landsize BuildingArea Propertycount 
d_Type_h d_Type_u d_Type_t d_Type_devsite d_Type_ores d_Method_SP d_Method_PI d_Method_PN 
d_Method_SN d_Method_NB d_Method_VB d_Method_W d_Method_SA d_Method_SS d_Method_NA Bedroom2_Bathroom /vif tol;
plot student.*predicted.;
plot npp.*student.;
run;

TITLE "Training and test sets";
proc surveyselect data= Melbourne_housing_2000data out= MH_TT seed =4378
samprate = 0.8 outall;
run;
proc print data=MH_TT;
run;

TITLE "Training and test sets in selected";
data MH_TT;
set MH_TT;
if selected then new_y = ln_Price;
run;
proc print;
run;

title"Historgram w/logp "; 
proc univariate;
var new_y; 
histogram/normal;
run;


title "Pearson Correlation Matrix data=Melbourne_housing2200smaple";
proc corr data=MH_TT;
var new_y Distance Bedroom2 Bathroom Car ln_Landsize BuildingArea Propertycount 
d_Type_h d_Type_u d_Type_t d_Type_devsite d_Type_ores d_Method_SP d_Method_PI d_Method_PN 
d_Method_SN d_Method_NB d_Method_VB d_Method_W d_Method_SA d_Method_SS d_Method_NA Bedroom2_Bathroom;
run;

title "Full Regression Model for ln_Price with VIF data=Melbourne_housing2200smaple";
proc reg data=MH_TT;
model new_y = Distance Bedroom2 Bathroom Car ln_Landsize BuildingArea Propertycount 
d_Type_h d_Type_u d_Type_t d_Type_devsite d_Type_ores d_Method_SP d_Method_PI d_Method_PN 
d_Method_SN d_Method_NB d_Method_VB d_Method_W d_Method_SA d_Method_SS d_Method_NA Bedroom2_Bathroom/vif stb;;
run;

title "Selection Method: backward Selection Method md1";
proc reg data=MH_TT;
model new_y = Distance Bedroom2 Bathroom Car ln_Landsize BuildingArea Propertycount 
d_Type_h d_Type_u d_Type_t d_Type_devsite d_Type_ores d_Method_SP d_Method_PI d_Method_PN 
d_Method_SN d_Method_NB d_Method_VB d_Method_W d_Method_SA d_Method_SS d_Method_NA Bedroom2_Bathroom/selection=backward;
run;

title "Selection Method: Adj-R2 Selection Method md1"; * Selection Method-3: stepwise Selection Method;
proc reg data=MH_TT;
model new_y = Distance Bedroom2 Bathroom Car ln_Landsize BuildingArea Propertycount 
d_Type_h d_Type_u d_Type_t d_Type_devsite d_Type_ores d_Method_SP d_Method_PI d_Method_PN 
d_Method_SN d_Method_NB d_Method_VB d_Method_W d_Method_SA d_Method_SS d_Method_NA Bedroom2_Bathroom/selection=adjrsq;
run;

title "Selection Method: backward Selection Method md2";
proc reg data=MH_TT;
model new_y = Distance Bedroom2 Bathroom Car ln_Landsize BuildingArea Propertycount 
d_Type_h d_Type_u d_Type_t d_Type_devsite d_Type_ores d_Method_SP d_Method_PI d_Method_PN 
d_Method_SN d_Method_NB d_Method_VB d_Method_W d_Method_SA d_Method_SS d_Method_NA/selection=backward;
run;

title "Selection Method: Adj-R2 Selection Method md2"; * Selection Method-3: stepwise Selection Method;
proc reg data=MH_TT;
model new_y = Distance Bedroom2 Bathroom Car ln_Landsize BuildingArea Propertycount 
d_Type_h d_Type_u d_Type_t d_Type_devsite d_Type_ores d_Method_SP d_Method_PI d_Method_PN 
d_Method_SN d_Method_NB d_Method_VB d_Method_W d_Method_SA d_Method_SS d_Method_NA/selection=adjrsq;
run;

title "Selection Method: backward Selection Method md3";
proc reg data=MH_TT;
model new_y = Distance Bedroom2 Car ln_Landsize BuildingArea Propertycount 
d_Type_h d_Type_u d_Type_t d_Type_devsite d_Type_ores d_Method_SP d_Method_PI d_Method_PN 
d_Method_SN d_Method_NB d_Method_VB d_Method_W d_Method_SA d_Method_SS d_Method_NA Bedroom2_Bathroom/selection=backward;
run;

title "Selection Method: Adj-R2 Selection Method md3"; * Selection Method-3: stepwise Selection Method;
proc reg data=MH_TT;
model new_y = Distance Bedroom2 Car ln_Landsize BuildingArea Propertycount 
d_Type_h d_Type_u d_Type_t d_Type_devsite d_Type_ores d_Method_SP d_Method_PI d_Method_PN 
d_Method_SN d_Method_NB d_Method_VB d_Method_W d_Method_SA d_Method_SS d_Method_NA Bedroom2_Bathroom/selection=adjrsq;
run;

title "modl 1 Full Regression Model for new_y with VIF data=Melbourne_housing2200smaple";
proc reg data=MH_TT;
model new_y = Distance Bedroom2 Bathroom ln_Landsize BuildingArea Propertycount d_Type_h d_Type_u d_Method_SP Bedroom2_Bathroom;
plot student.*(Distance Bedroom2 Bathroom ln_Landsize BuildingArea Propertycount d_Type_h d_Type_u d_Method_SP Bedroom2_Bathroom);
plot student.*predicted.;
plot npp.*student.;
run;

title "modl 2 Full Regression Model for new_y with VIF data=Melbourne_housing2200smaple";
proc reg data=MH_TT;
model new_y = Distance Bedroom2 Bathroom ln_Landsize BuildingArea Propertycount d_Type_h d_Type_u d_Method_SP;
plot student.*(Distance Bedroom2 Bathroom ln_Landsize BuildingArea Propertycount d_Type_h d_Type_u d_Method_SP);
plot student.*predicted.;
plot npp.*student.;
run;

title "modl 3 Full Regression Model for new_y with VIF data=Melbourne_housing2200smaple";
proc reg data=MH_TT;
model new_y = Distance Bedroom2 ln_Landsize BuildingArea Propertycount d_Type_h d_Type_u d_Method_SP Bedroom2_Bathroom;
plot student.*(Distance Bedroom2 ln_Landsize BuildingArea Propertycount d_Type_h d_Type_u d_Method_SP Bedroom2_Bathroom);
plot student.*predicted.;
plot npp.*student.;
run;

title "Final Regression Model for new_y with VIF data=Melbourne_housing2200smaple";
proc reg data=MH_TT;
model new_y = Distance Bedroom2 Bathroom ln_Landsize BuildingArea Propertycount d_Type_h d_Type_u d_Method_SP Bedroom2_Bathroom/r influence vif stb;;
plot student.*(Distance Bedroom2 Bathroom ln_Landsize BuildingArea Propertycount d_Type_h d_Type_u d_Method_SP Bedroom2_Bathroom);
plot student.*predicted.;
plot npp.*student.;
run;

title "Remove Influencial Points and Outliers"; 
data MH_TT_new;
set MH_TT;
if _n_ in (103,756,982,1232,1385,1844) then delete;
run;
proc print;
run;


title "Final Regression Model for new_y with VIF data=Melbourne_housing2200smaple";
proc reg data=MH_TT_new;
model new_y = Distance Bedroom2 Bathroom ln_Landsize BuildingArea Propertycount d_Type_h d_Type_u d_Method_SP Bedroom2_Bathroom/r influence vif stb;;
plot student.*(Distance Bedroom2 Bathroom ln_Landsize BuildingArea Propertycount d_Type_h d_Type_u d_Method_SP Bedroom2_Bathroom);
plot student.*predicted.;
plot npp.*student.;
run;

title "Remove Influencial Points and Outliers"; 
data MH_TT_new1;
set MH_TT_new;
if _n_ in (452,1617) then delete;
run;
proc print;
run;

title "Final Regression Model for new_y with VIF data=Melbourne_housing2200smaple";
proc reg data=MH_TT_new1;
model new_y = Distance Bedroom2 Bathroom ln_Landsize BuildingArea Propertycount d_Type_h d_Type_u d_Method_SP Bedroom2_Bathroom/r influence vif stb;;
plot student.*(Distance Bedroom2 Bathroom ln_Landsize BuildingArea Propertycount d_Type_h d_Type_u d_Method_SP Bedroom2_Bathroom);
plot student.*predicted.;
plot npp.*student.;
run;

title "Validation - Test Set";
proc reg data=MH_TT_new1;
model new_y = Distance Bedroom2 Bathroom ln_Landsize BuildingArea Propertycount d_Type_h d_Type_u d_Method_SP Bedroom2_Bathroom;
output out=outm1(where=(new_y=.)) p=yhat;
run;
proc print;
run;

title "Difference between Observed and Predicted in Test Set";
data outm1_sum ;
set outm1;
d=ln_Price-yhat; *d is the difference between observed and predicted values in test set; absd=abs(d);
run;
proc print;
run;

title "Descriptives";
proc means mean std stderr clm min p25 p50 p75 max; 
var  absd;
run;

proc summary data=outm1_sum;
var d absd;
output out=outm1_stats std(d)=rmse mean(absd)=mae ;
proc print data=outm1_stats;
run;



title 'Validation statistics for Model';
proc corr data=outm1;
var ln_Price yhat;
run;
proc print;
run;


title "5-fold crossvalidation + 25% testing set";
proc glmselect data=MH_TT
plots=(asePlot Criteria);
*partition defines a test set (25% of data) to validate model on new data;
partition fraction(test=0.25);
* selection=stepwise uses stepwise selection method;
* stop=cv: minimizes prediction residual sum of squares for variable selection;
model new_y = Distance Bedroom2 Bathroom Car ln_Landsize BuildingArea Propertycount d_Type_h d_Type_u d_Type_t d_Type_devsite d_Type_ores d_Method_SP d_Method_PI d_Method_PN 
d_Method_SN d_Method_NB d_Method_VB d_Method_W d_Method_SA d_Method_SS d_Method_NA Bedroom2_Bathroom/selection=backward(stop=cv)cvMethod=split(5) cvDetails=all;
run;


title "input two predictions";
data new;
input Distance Bedroom2 Bathroom ln_Landsize BuildingArea Propertycount d_Type_h d_Type_u d_Method_SP Bedroom2_Bathroom;;
datalines;
10 3 1 5.34 209 5682 1 0 1 3
20 5 2 6.87 280 8888 0 1 0 10
;
run;
proc print;
run;

title "Merge prediction dataset with original datase";
data pred;
set new Melbourne_housing_2000data;
run;
proc print;
run;

title "Final Regression Model for new_y with VIF data=Melbourne_housing2200smaple";
proc reg data=pred;
model ln_price = Distance Bedroom2 Bathroom ln_Landsize BuildingArea Propertycount d_Type_h d_Type_u d_Method_SP Bedroom2_Bathroom/p cli clm ;
run;  
