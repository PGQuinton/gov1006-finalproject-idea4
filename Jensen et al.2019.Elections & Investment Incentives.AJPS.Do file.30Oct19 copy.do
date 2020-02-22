
# The data analysis for this paper was conducted in Stata.
# I have loaded in the Stata do-file as text and am commenting over it.
# I have a limited understanding of Stata but will do my best to parse through it.

********************************************************************************
* REPLICATION .DO FILE - Jensen, Findley, & Nielson
* AJPS 2019 - Electoral Institutions & Electoral Cycles in Investment Incentives
* 29 October 2019
********************************************************************************

* Verify that parmest package is installed; ssc install parmest
* Verify that outreg2 is installed; ssc install outreg2
* Verify that estout is installed; ssc install estout
* Verify that sartsel ado file is installed; located at https://asartori.mit.edu/research
* Verify that spost9 is installed; located at http://www.indiana.edu/~jslsoc/stata

# These first lines of code clear the workspace of any prior work.

clear
set more off
eststo clear

# Here the authors are providing a space for the person replicating the data to 
# change the working director and create a new folder for extra files.

* Use cd to set directory for the using and output files
*{CHANGE DIRECTORY HERE}
*{CREATE "Accessory" SUBFOLDER FOR ALL THE ACCESSORY FILES}

********************************************************************************
********************************************************************************
* TABLES IN MAIN TEXT
********************************************************************************
********************************************************************************

********************************************************************************
* TABLE 1 - Main Treatment Effects
********************************************************************************

# The authors are specifying which data to use. 
# In this case, it is the incentives dataset.

use "Incentives Experiment Replication Data.dta"

# Here the authors are running T-Tests on the incentives offered prior to the elections to 
# test for statistical significance.
# They do this for four variables, response, incentoffered, lndollars2, and dollars.

* T-TESTS FOR BEFOREELECTION TREATMENT
ttest response, by(beforeelection)
ttest incentoffered, by(beforeelection)
ttest lndollars2, by(beforeelection)
ttest dollars, by(beforeelection)


# Next, the authors generate descriptive data to display the behavior of government actors
# pre-election. 

* Generate descriptive data using collapse for table and graphs

# The next set of code creates five new variables.
# They are creating binary variables to indicate whether a treatment/action occured
# where it is 1 if it id and 0 if it didn't.

gen response_count=.
replace response_count=1 if response==1

gen incent_count=.
replace incent_count=1 if incentoffered==1

gen dollars_count=.
replace dollars_count=1 if lndollars~=.

gen treat_n =.
replace treat_n=1 if beforeelection==1

gen control_n =.
replace control_n=1 if beforeelection==0

# I am somewhat uncertain about this command but I think it basically functions like summarize
# does in R.
# The data is being collapsed to just show the mean, standard deviation, and count of a number of variables.

collapse (mean) mean1=response (mean) mean2=incentoffered (mean) mean3=lndollars2 (count) count1=response_count ///
	(count) count2=incent_count (count) count3=dollars_count (sd) sd1=response (sd) sd2=incentoffered ///
	(sd) sd3=lndollars (count) control_n1=control_n (count) control_n2=control_n (count) control_n3=control_n ///
	(count) treat_n1=treat_n (count) treat_n2=treat_n (count) treat_n3=treat_n, by(beforeelection)
	
# Filtering out observations not before the election.

drop if beforeelection==.

reshape long mean count control_n treat_n sd, i(beforeelection) j(outcome)

# Renaming a the beforeelection variable to treat.

rename beforeelection treat

# They are generating a new variable using essentially a bunch of ifelse commands.
# The new values range from 0 to 3.

gen idstr=.
tostring ids, replace
replace idstr="Response" if outcome==1
replace idstr="Incentive" if outcome==2
replace idstr="Ln Dollars" if outcome==3
order idstr treat mean outcome
reshape wide mean count sd control_n treat_n, i(outcome) j(treat)
order idstr outcome mean0 mean1 count0 count1 control_n0 control_n1 treat_n0 treat_n1 sd0 sd1

# They are sorting or arranging the data by the new variable, idstr.

sort idstr

# The authors are saving the modified dataset as a new dataset.

save "Accessory/Outcome Means for B4Election.dta", replace

* Generate analysis data for tables and use in graphs

# The authors switch back to the original dataset.

use "Incentives Experiment Replication Data.dta", replace

# The are creating a set of new variable conditional upon whether an action occured by the 
# election.
# A treatment and a control variable is created. Both are binary.

gen response_b4elec_treat = .
replace response_b4elec_treat = 1 if response==1 & beforeelection==1
replace response_b4elec_treat = 0 if response==1 & beforeelection==0

gen response_b4elec_control = .
replace response_b4elec_control = 1 if response==1 & beforeelection==0
replace response_b4elec_control = 1 if response==1 & beforeelection==1

# Not really sure what is going on here. 
# I think they may be specifying how they want the outputs to be saved.

tempfile tf1 tf2 tf3
parmby "regress response beforeelection", lab saving(`"`tf1'"',replace) stars (0.1 0.05 0.01) idn(1) ids(Response)
parmby "regress incentoffered beforeelection", lab saving(`"`tf2'"',replace) stars (0.1 0.05 0.01) idn(2) ids(Incentive)
parmby "regress lndollars2 beforeelection", lab saving(`"`tf3'"',replace) stars (0.1 0.05 0.01) idn(3) ids(Ln Dollars)
drop _all
append using `"`tf1'"' `"`tf2'"' `"`tf3'"'
list idnum idstr parm estimate min95 max95 p, clean 
drop if label=="Constant"

# Arranging the data by idstr.

sort idstr

# Merging the idstr variable to the current dataset using the modified dataset.
# Then, sorting by idnum.

merge 1:1 idstr using "Accessory/Outcome Means for B4Election.dta"
sort idnum

# Here they are modifying and creating several variables.

reshape long mean count sd, i(idnum) j(treat)
rename treat control_treat
rename count n
gen condition=.
tostring condition, replace
replace condition="Before Election" if control_treat==1
replace condition="After Election" if control_treat==0

# As it says below, here they are generating confidence intervals.

* Make upper and lower values of the confidence intervals

generate hivalue = mean + invttail(n-1,0.025)*(sd / sqrt(n))
generate lovalue = mean - invttail(n-1,0.025)*(sd / sqrt(n))

# Again, generating a new variable based on idnum values.

gen str outcome_name="."
replace outcome_name="Response" if idnum==1
replace outcome_name="Incentive" if idnum==2
replace outcome_name="Dollars" if idnum==3

# Generating a binary variable to indicate if an observation is treatment or control.

gen str treatment="."
replace treatment="Before Election" if control_treat==1
replace treatment="After Election" if control_treat==0

# Renaming n to count.

rename n count

# Creating the variable N.
# Another binary variable for control vs. treatment.

gen N=.
replace N=control_n0 if control_treat==0
replace N=treat_n1 if control_treat==1

# Sorting by outcome name and then by treatment.

gsort -outcome_name -treatment

order outcome_name treatment N count mean estimate stars p min95 max95

# Saving this data as a new dataset.

save "Accessory/Table1.B4Election Effects.dta", replace

# Clearing workspace.

clear

********************************************************************************
* END TABLE 1 
********************************************************************************


********************************************************************************
* TABLE 2: Effects of Before-Election Treatment in Subgroup of Municipalities with 
* 	Self-Identified Manufacturing Focus
********************************************************************************
clear

# Specifying the dataset to be used for this section of the analysis.

use "Incentives Experiment Replication Data.dta"

# Examing the data when manufacturingfocus==1.
# Gives you a general overview of what's going on in the data.

tab beforeelection incentoffered if manufacturingfocus==1

# Running ttests on three variables.

ttest response if manufacturingfocus==1, by(beforeelection)
ttest incentoffered if manufacturingfocus==1, by(beforeelection)
ttest lndollars2 if manufacturingfocus==1, by(beforeelection)

# Running a probit regression.
# Pretty sure this is a type of logistic regression.

probit response beforeelection if manufacturingfocus==1
estimates store m1, title(Model 1)

probit response beforeelection Japan China lnpop quart_1 quart_2 quart_4 regi_2 ///
	regi_3 regi_4 if manufacturingfocus==1
estimates store m2, title(Model 2)

probit incentoffered beforeelection if manufacturingfocus==1
estimates store m3, title(Model 3)

probit incentoffered beforeelection Japan China lnpop quart_1 quart_2 quart_4 regi_2 ///
	regi_3 regi_4 if manufacturingfocus==1
estimates store m4, title(Model 4)

# Here they switch to just regular OLS regressions.

regress lndollars2 beforeelection if manufacturingfocus==1
estimates store m5, title(Model 5)

regress lndollars2 beforeelection Japan China lnpop quart_1 quart_2 quart_4 regi_2 ///
	regi_3 regi_4 if manufacturingfocus==1
estimates store m6, title(Model 6)
	
estout * using "Accessory/Table 2.MANUF SUBGROUP.txt", ///
	cells(b(star fmt(3)) se(par fmt(3))) starl(* 0.1 ** 0.05 *** 0.01) stats(N r2_p r2) style(tab) replace

********************************************************************************
* END TABLE 2 
********************************************************************************



********************************************************************************
* TABLE 3 - Response Rate with Dummies for Elected, Quarter, Region, and State
********************************************************************************
clear

# New section so the workspace is cleared and a new dataset is specified.

use "Incentives Experiment Replication Data.dta"

# More logistic regression run.
# Each time an additional covariate is added to the model.
# Quarter and Region fixed effects are added in.

probit response Japan China elected
estimates store m1, title(Model 1)

probit response Japan China elected lnpop
estimates store m2, title(Model 2)

probit response Japan China elected lnpop quart_1 quart_3 quart_4
estimates store m3, title(Model 3)

probit response Japan China elected lnpop regi_1 regi_2 regi_3
estimates store m4, title(Model 4)
	
probit response Japan China elected lnpop quart_1 quart_3 quart_4 regi_1 regi_2 regi_3
estimates store m5, title(Model 5)	

probit response Japan China elected lnpop quart_1 quart_3 quart_4 estado_*
estimates store m6, title(Model 6)		

estout * using "Accessory/Table 3.RESPONSE RATES.txt", ///
	cells(b(star fmt(3)) se(par fmt(3))) starl(* 0.1 ** 0.05 *** 0.01) stats(N r2_p r2) style(tab) replace
	
********************************************************************************
* END TABLE 3
********************************************************************************



********************************************************************************
* TABLE 4 - Incentives Offered with Dummies for Elected, Quarter, Region, and State
********************************************************************************
clear
use "Incentives Experiment Replication Data.dta"

# Similar analysis but the dependent variable is switch from response rate to 
# incentive offered.
# Again models are expanded to include Quarter and Region fixed effects.

probit incentoffered Japan China elected
estimates store m1, title(Model 1)

probit incentoffered Japan China elected lnpop
estimates store m2, title(Model 2)

probit incentoffered Japan China elected lnpop quart_1 quart_3 quart_4
estimates store m3, title(Model 3)

probit incentoffered Japan China elected lnpop regi_1 regi_2 regi_3
estimates store m4, title(Model 4)
	
probit incentoffered Japan China elected lnpop quart_1 quart_3 quart_4 regi_1 regi_2 regi_3
estimates store m5, title(Model 5)	

probit incentoffered Japan China elected lnpop quart_1 quart_3 quart_4 estado_*
estimates store m6, title(Model 6)		

estout * using "Accessory/Table 4.RESPONSE.txt", ///
	cells(b(star fmt(3)) se(par fmt(3))) starl(* 0.1 ** 0.05 *** 0.01) stats(N r2_p r2) style(tab) replace

********************************************************************************
* END TABLE 4
********************************************************************************



********************************************************************************
* TABLE 5 - Logged Dollars with Dummies for Elected, Quarter, Region, and State
********************************************************************************
clear
use "Incentives Experiment Replication Data.dta"

# This time it's an OLS regression with lndollars (logged dollars) as the independent
# variable.
# lndollars is continuous unlike response and incentive which are binary 
# so an OLS regression works.
# Same progression with Quarter and Regional fixed effects added.

regress lndollars Japan China elected
estimates store m1, title(Model 1)

regress lndollars Japan China elected lnpop
estimates store m2, title(Model 2)

regress lndollars Japan China elected lnpop quart_1 quart_3 quart_4
estimates store m3, title(Model 3)

regress lndollars Japan China elected lnpop regi_1 regi_2 regi_3
estimates store m4, title(Model 4)
	
regress lndollars Japan China elected lnpop quart_1 quart_3 quart_4 regi_1 regi_2 regi_3
estimates store m5, title(Model 5)	

regress lndollars Japan China elected lnpop quart_1 quart_3 quart_4 estado_*
estimates store m6, title(Model 6)		

estout * using "Accessory/Table 5.DOLLARS.txt", ///
	cells(b(star fmt(3)) se(par fmt(3))) starl(* 0.1 ** 0.05 *** 0.01) stats(N r2_p r2) style(tab) replace

********************************************************************************
* END TABLE 5
********************************************************************************


********************************************************************************
* APPENDIX MATERIAL
********************************************************************************


********************************************************************************
* FIGURE D1: Treatment Effects for Before Election on Incentives Offered
********************************************************************************

# Clearing workspace and switching datasets.

clear
use "Accessory/Table1.B4Election Effects.dta"

* graph twoway (bar mean control_treat) (rcap hivalue lovalue control_treat) if outcome==1 | outcome==2, by(outcome)

# Renaming three variables.

rename estimate testdiff
rename min95 lowci
rename max95 highci

# Filtering out obserservation by idnum.

drop if idnum==3

		// this will help us space out the graph the way we want it.

# Generating new variables conditional upon idum and control_treat.

		generate treatout = 1 if idnum==1 & control_treat==0
		replace treatout = 2 if idnum==1 & control_treat==1
		replace treatout = 3.5 if idnum==2 & control_treat==0
		replace treatout = 4.5 if idnum==2 & control_treat==1

# Generating a new variable based on treatout.

		generate treatoutb = .5 if treatout==1
		replace treatoutb = 1 if treatout==2
		replace treatoutb = 3 if treatout==3.5
		replace treatoutb = 3.5 if treatout==4.5

# Sorting by the new variable treatout.

		sort treatout

# Creating a twoway bar chart.

	twoway (bar mean treatout if treatout==1) /// 
		   (bar mean treatout if treatout==2) /// 
		   (bar mean treatout if treatout==3.5) /// 
		   (bar mean treatout if treatout==4.5) ///
		   (rcap hivalue lovalue treatout, lw(none)), ///
		   legend(row(1) order(1 "Resp Control" 2 "Resp Treat" 3 "Incent Control" 4 "Incent Treat") ) ///
		   xlabel( 1 "Response Control" 2 "Response Treatment" 3.5 "Incentive Control" 4.5 "Incentive Treatment", noticks) ///
		   xtitle("Values Across Experimental Conditions") ytitle("Proportion") ///
		   name(panela, replace) nodraw legend(off)
		   

# Another twoway bar chart.

	twoway (dot testdiff treatoutb if treatout==2, dotextend(no)) /// 
		   (dot testdiff treatoutb if treatout==4.5, dotextend(no)) /// 
		   (rcap highci lowci treatoutb if treatout==2 | treatout==4.5), ///
		   ysc(r(-.1  .1)) ///
		   xlabel(0 " " .5 " " 1 " " 1.5 " " 2 " " 2.5 " " 3 " " 3.5 " " 4 " " 4.5 " ", noticks) ///
		   xtitle("Treatment Effects and Confidence Intervals") ytitle("Treatment - Control") ///
		   name(panelb, replace) nodraw yline(0, lcolor(black)) legend(off) ylabel(-.1(.05).1) ///

# Graphing both of the twoway plots into a combined graph.

	graph combine panela panelb, cols(1)

# Saving the new graph.

	graph save "Accessory/Figure D1.Main Means Graph.gph", replace

********************************************************************************
* END FIGURE D1 
********************************************************************************



********************************************************************************
* TABLE D1 - Response Rate, Incentive Offered, and Logged Dollars with Treatments 
*	and Main Control Variables
********************************************************************************
# New section, new dataset

clear
use "Incentives Experiment Replication Data.dta"

# Logistic regressions with response as the independent variable.
# Quarter and Region effects added to some regression models.

probit response beforeelection Japan China lnpop quart_1 quart_3 quart_4 regi_1 regi_2 regi_3
estimates store m1, title(Model 1)

probit response beforeelection Japan China lnpop quart_1 quart_3 quart_4 estado_*
estimates store m2, title(Model 2)

# Changed independent variable to incentoffered.
# Otherwise the same.

probit incentoffered beforeelection Japan China lnpop quart_1 quart_3 quart_4 regi_1 regi_2 regi_3
estimates store m3, title(Model 3)

probit incentoffered beforeelection Japan China lnpop quart_1 quart_3 quart_4 estado_*
estimates store m4, title(Model 4)
	
# Changed independent variable to lndollars.
# So, didn't need probit and could run a linear regression.
# Same use of fixed effects.

regress lndollars beforeelection Japan China lnpop quart_1 quart_3 quart_4 regi_1 regi_2 regi_3
estimates store m5, title(Model 5)	

regress lndollars beforeelection Japan China lnpop quart_1 quart_3 quart_4 estado_*
estimates store m6, title(Model 6)		

estout * using "Accessory/Table D1.TREATMENT EFFECTS.txt", ///
	cells(b(star fmt(3)) se(par fmt(3))) starl(* 0.1 ** 0.05 *** 0.01) stats(N r2_p r2) style(tab) replace

********************************************************************************
* END TABLE D1 
********************************************************************************


********************************************************************************
* TABLE D1A - Response Rate, Incentive Offered, and Logged Dollars with Treatments 
*	and Controls on Subgroup with Elections in 2013
********************************************************************************

# New section, new dataset

clear
use "Incentives Experiment Replication Data.dta"

# Logistic regressions with response as the independent variable.
# Quarter and region fixed effects.

# Restricted to data before 2014

probit response beforeelection Japan China lnpop quart_1 quart_3 quart_4 regi_1 regi_2 regi_3 if elec_year < 2014
estimates store m1, title(Model 1)

probit response beforeelection Japan China lnpop quart_1 quart_3 quart_4 estado_*  if elec_year < 2014
estimates store m2, title(Model 2)

# Switched indepndent variable to incentoffered.
# Otherwise the same set of regressions.

probit incentoffered beforeelection Japan China lnpop quart_1 quart_3 quart_4 regi_1 regi_2 regi_3  if elec_year < 2014
estimates store m3, title(Model 3)

probit incentoffered beforeelection Japan China lnpop quart_1 quart_3 quart_4 estado_*  if elec_year < 2014
estimates store m4, title(Model 4)

# Switched the indendent variable to ln and the regression to linear.
# Same use of fixed effects and filtering to pre-2014.

regress lndollars beforeelection Japan China lnpop quart_1 quart_3 quart_4 regi_1 regi_2 regi_3  if elec_year < 2014
estimates store m5, title(Model 5)	

regress lndollars beforeelection Japan China lnpop quart_1 quart_3 quart_4 estado_*  if elec_year < 2014
estimates store m6, title(Model 6)		

estout * using "Accessory/Table D1A.Subgroup Effects 2013 Elections.txt", ///
	cells(b(star fmt(3)) se(par fmt(3))) starl(* 0.1 ** 0.05 *** 0.01) stats(N r2_p r2) style(tab) replace

********************************************************************************
* END TABLE D1A 
********************************************************************************
	
	
********************************************************************************
* TABLE D1B - Response Rate, Incentive Offered, and Logged Dollars with Treatments 
*	and Controls on Subgroup with Elections in 2013-2014 
********************************************************************************

# New section, new dataset.

# This section is the same as the last except the data is filtered to pre-2015 and
# not pre-2014.

clear
use "Incentives Experiment Replication Data.dta"

probit response beforeelection Japan China lnpop quart_1 quart_3 quart_4 regi_1 regi_2 regi_3 if elec_year < 2015
estimates store m1, title(Model 1)

probit response beforeelection Japan China lnpop quart_1 quart_3 quart_4 estado_*  if elec_year < 2015
estimates store m2, title(Model 2)

probit incentoffered beforeelection Japan China lnpop quart_1 quart_3 quart_4 regi_1 regi_2 regi_3  if elec_year < 2015
estimates store m3, title(Model 3)

probit incentoffered beforeelection Japan China lnpop quart_1 quart_3 quart_4 estado_*  if elec_year < 2015
estimates store m4, title(Model 4)
	
regress lndollars beforeelection Japan China lnpop quart_1 quart_3 quart_4 regi_1 regi_2 regi_3  if elec_year < 2015
estimates store m5, title(Model 5)	

regress lndollars beforeelection Japan China lnpop quart_1 quart_3 quart_4 estado_*  if elec_year < 2015
estimates store m6, title(Model 6)		

estout * using "Accessory/Table D1B.Subgroup Effects 2013-14 Elections.txt", ///
	cells(b(star fmt(3)) se(par fmt(3))) starl(* 0.1 ** 0.05 *** 0.01) stats(N r2_p r2) style(tab) replace

********************************************************************************
* END TABLE D1B 
********************************************************************************
	
	
	
********************************************************************************
* TABLE D1C - Response Rate, Incentive Offered, and Logged Dollars with Treatments 
*	and Controls on Subgroup with Elections in 2013-2015 
********************************************************************************

# Same exact section as above except pre-2016.

clear
use "Incentives Experiment Replication Data.dta"

probit response beforeelection Japan China lnpop quart_1 quart_3 quart_4 regi_1 regi_2 regi_3 if elec_year < 2016
estimates store m1, title(Model 1)

probit response beforeelection Japan China lnpop quart_1 quart_3 quart_4 estado_* if elec_year < 2016
estimates store m2, title(Model 2)

probit incentoffered beforeelection Japan China lnpop quart_1 quart_3 quart_4 regi_1 regi_2 regi_3 if elec_year < 2016
estimates store m3, title(Model 3)

probit incentoffered beforeelection Japan China lnpop quart_1 quart_3 quart_4 estado_* if elec_year < 2016
estimates store m4, title(Model 4)
	
regress lndollars beforeelection Japan China lnpop quart_1 quart_3 quart_4 regi_1 regi_2 regi_3 if elec_year < 2016
estimates store m5, title(Model 5)	

regress lndollars beforeelection Japan China lnpop quart_1 quart_3 quart_4 estado_* if elec_year < 2016
estimates store m6, title(Model 6)		

estout * using "Accessory/Table D1C.Subgroup Effects 2013-15 Elections.txt", ///
	cells(b(star fmt(3)) se(par fmt(3))) starl(* 0.1 ** 0.05 *** 0.01) stats(N r2_p r2) style(tab) replace

********************************************************************************
* END TABLE D1C 
********************************************************************************


********************************************************************************
* TABLE D1D - Response Rate, Incentive Offered, and Logged Dollars with Treatments 
*	and Controls on Subgroup of Smaller Cities (Below Median Population) 
********************************************************************************

# Same regressions as above except the filter is for population_block equal to 0
# No restriction by year
# Otherwise exact same section as the above ones

clear
use "Incentives Experiment Replication Data.dta"

probit response beforeelection Japan China lnpop quart_1 quart_3 quart_4 regi_1 regi_2 regi_3 if population_block==0
estimates store m1, title(Model 1)

probit response beforeelection Japan China lnpop quart_1 quart_3 quart_4 estado_* if population_block==0
estimates store m2, title(Model 2)

probit incentoffered beforeelection Japan China lnpop quart_1 quart_3 quart_4 regi_1 regi_2 regi_3 if population_block==0
estimates store m3, title(Model 3)

probit incentoffered beforeelection Japan China lnpop quart_1 quart_3 quart_4 estado_* if population_block==0
estimates store m4, title(Model 4)
	
regress lndollars beforeelection Japan China lnpop quart_1 quart_3 quart_4 regi_1 regi_2 regi_3 if population_block==0
estimates store m5, title(Model 5)	

regress lndollars beforeelection Japan China lnpop quart_1 quart_3 quart_4 estado_* if population_block==0
estimates store m6, title(Model 6)		

estout * using "Accessory/Table D1D.Subgroup Effects Small Cities.txt", ///
	cells(b(star fmt(3)) se(par fmt(3))) starl(* 0.1 ** 0.05 *** 0.01) stats(N r2_p r2) style(tab) replace

********************************************************************************
* END TABLE D1D 
********************************************************************************
	
	
********************************************************************************
* TABLE D1E - Response Rate, Incentive Offered, and Logged Dollars with Treatments 
*	and Controls on Subgroup of Larger Cities (Above Median Population) 
********************************************************************************

# Repeating the above regressions but for data when population_block equals 1.

clear
use "Incentives Experiment Replication Data.dta"

probit response beforeelection Japan China lnpop quart_1 quart_3 quart_4 regi_1 regi_2 regi_3 if population_block==1
estimates store m1, title(Model 1)

probit response beforeelection Japan China lnpop quart_1 quart_3 quart_4 estado_* if population_block==1
estimates store m2, title(Model 2)

probit incentoffered beforeelection Japan China lnpop quart_1 quart_3 quart_4 regi_1 regi_2 regi_3 if population_block==1
estimates store m3, title(Model 3)

probit incentoffered beforeelection Japan China lnpop quart_1 quart_3 quart_4 estado_* if population_block==1
estimates store m4, title(Model 4)
	
regress lndollars beforeelection Japan China lnpop quart_1 quart_3 quart_4 regi_1 regi_2 regi_3 if population_block==1
estimates store m5, title(Model 5)	

regress lndollars beforeelection Japan China lnpop quart_1 quart_3 quart_4 estado_* if population_block==1
estimates store m6, title(Model 6)		

estout * using "Accessory/Table D1E.Subgroup Effects Large Cities.txt", ///
	cells(b(star fmt(3)) se(par fmt(3))) starl(* 0.1 ** 0.05 *** 0.01) stats(N r2_p r2) style(tab) replace

********************************************************************************
* END TABLE D1E
********************************************************************************
	

********************************************************************************
* TABLE D2: Sartori Selection Model of Treatment Conditions and Covariates on Response 
*	and Incentives Offered for Cities with Manufacturing Focus
********************************************************************************

# Clearing workspace and specifying dataset.

clear
use "Incentives Experiment Replication Data.dta"


* Generate needed variables

# New variables created.

gen sartincent=.
replace sartincent=0 if response==0
replace sartincent=1 if response==1 & incentoffered==0
replace sartincent=2 if response==1 & incentoffered==1

# Comparative binary variable to show if Japan and US are the same.

gen compare_japan_us =.
replace compare_japan_us=1 if Japan==1 | US==1

# Comparative binary variable to show if China and US are the same.

gen compare_china_us=.
replace compare_china_us=1 if China==1 | US==1

# Sartori variables created for Japan/US and China/US respectively.

gen sartincent_japan_us=.
replace sartincent_japan_us=0 if response==0 & incentoffered==0 & compare_japan_us==1 
replace sartincent_japan_us=1 if response==1 & incentoffered==0 & compare_japan_us==1 
replace sartincent_japan_us=2 if response==1 & incentoffered==1 & compare_japan_us==1 

gen sartincent_china_us=.
replace sartincent_china_us=0 if response==0 & incentoffered==0 & compare_china_us==1 
replace sartincent_china_us=1 if response==1 & incentoffered==0 & compare_china_us==1 
replace sartincent_china_us=2 if response==1 & incentoffered==1 & compare_china_us==1

* Run Sartori Selection basic models for main effects

gen mf_sartincent=.
replace mf_sartincent=sartincent if manufacturingfocus==1

# Here they are running Sartori models.
# Not sure what these are but they appear to be somewhat like a logistic regression.
# Documentation is limited.

sartsel mf_sartincent beforeelection
outreg2 using "Accessory/Table D2.SartSel.txt", replace

* Sartori Selection models with covariates 

sartsel mf_sartincent beforeelection Japan China lnpop regi_1 regi_2  regi_3 
outreg2 using "Accessory/Table D2.SartSel.txt", append
* Note that SartSel specifications with more than one quarter fail to converge.

* Before Election X Manufacturing Focus/Base
sartsel mf_sartincent beforeelection Japan China lnpop quart_3 ///
	regi_1 regi_2 regi_3
* Note that SartSel specifications with more than one quarter fail to converge.

outreg2 using "Accessory/Table D2.SartSel.txt", append


********************************************************************************
* END TABLE D2
********************************************************************************



********************************************************************************
* TABLE D3: Multinomial Probit Model of Treatment Conditions on Response and 
*	Incentives Offered for Cities with Manufacturing Focus
********************************************************************************

gen multioutcome=sartincent

# Running mprobit regressions.
# These appear to be multinomial regression models.
# Restricted to just if manufacturingfocus == 1.

mprobit multioutcome beforeelection if manufacturingfocus==1
outreg2 using "Accessory/Table D3.Multinomial.xls", replace

mprobit multioutcome beforeelection Japan China lnpop regi_1 regi_2  regi_3 ///
	if manufacturingfocus==1
outreg2 using "Accessory/Table D3.Multinomial.xls", append

mprobit multioutcome beforeelection Japan China lnpop quart_4 ///
	regi_1 regi_2 regi_3 if manufacturingfocus==1
outreg2 using "Accessory/Table D3.Multinomial.xls", append word excel

* Multinomial probit models with subgroup interactions
* Before Election X Region

# Removed the manufacturingfocus filter

mprobit multioutcome beforeelection Japan China lnpop regi_1 regi_2 regi_3 beforeelect_X_regi_1 beforeelect_X_regi_2 beforeelect_X_regi_3
mprobit multioutcome beforeelection Japan China lnpop regi_1 regi_2 regi_3 beforeelect_X_regi_1 beforeelect_X_regi_2 beforeelect_X_regi_3
mprobit multioutcome beforeelection Japan China lnpop regi_1 regi_2 regi_3 beforeelect_X_regi_1 beforeelect_X_regi_2 beforeelect_X_regi_3

* Before Election X Manufacturing Focus/Base

mprobit multioutcome beforeelection Japan China lnpop manufacturingfocus beforeelect_X_manufocus 
mprobit multioutcome beforeelection Japan China lnpop manufacturingbase beforeelect_X_manubase


********************************************************************************
* END TABLE D3
********************************************************************************



********************************************************************************
* Table D4: Response rate, incentive offered, and logged dollars with 
*	observational tests
********************************************************************************

clear
use "Incentives Experiment Replication Data.dta"

# Logistic regression of response on population, manufacturing, grouth and mayor.

probit response lnpop manufacturingfocus growth mayor2 regi_1 regi_2 regi_3
estimates store m1, title(Model 1)

# Same regression but for incentive

probit incentoffered lnpop manufacturingfocus growth mayor2 regi_1 regi_2 regi_3
estimates store m2, title(Model 2)

# Linear regression with dollars.

regress lndollars2 lnpop manufacturingfocus growth mayor2 regi_1 regi_2 regi_3
estimates store m3, title(Model 3)

probit response presdem_2008 lnpop manufacturingfocus growth mayor2 regi_1 regi_2 regi_3
estimates store m4, title(Model 4)
prchange, fromto
*when changing from min-->max, probability change goes from: 0.4548 --> 0.0658 =  -0.3889  
disp (((0.0658-0.4548) / 0.4548 ) * 100)
*(% change: -86%)
*when changing from 0.5SD below to 0.5SD above, you get: 0.2653 -->   0.1851 =  -0.0802 
disp (((0.1851-0.2653) / 0.2653) * 100)
*(% change: -30%)

probit incentoffered presdem_2008 lnpop manufacturingfocus growth mayor2 regi_1 regi_2 regi_3
estimates store m5, title(Model 5)	

regress lndollars2 presdem_2008 lnpop manufacturingfocus growth mayor2 regi_1 regi_2 regi_3
estimates store m6, title(Model 6)	

estout * using "Accessory/Table D4.OBSERVATIONAL.txt", ///
	cells(b(star fmt(3)) se(par fmt(3))) starl(* 0.1 ** 0.05 *** 0.01) stats(N r2_p r2) style(tab) replace

********************************************************************************
* END TABLE D4
********************************************************************************



********************************************************************************
* Table D5: Partisanship on Response Rate, Incentive Offered, and Logged Dollars 
*	Robustness Checks
********************************************************************************

clear
use "Incentives Experiment Replication Data.dta"

# Testing to see the effects of a democratic president in 2008
# on response, investment incentives, and dollars spent.

probit response presdem_2008 elected lnpop estado_*
estimates store m1, title(Model 1)
 
probit response mrp_estimate elected lnpop estado_*
estimates store m2, title(Model 2)

probit incentoffered presdem_2008 elected lnpop estado_*
estimates store m3, title(Model 3)

probit incentoffered mrp_estimate elected lnpop estado_*
estimates store m4, title(Model 4)

regress lndollars2 presdem_2008 elected lnpop estado_*
estimates store m5, title(Model 5)

regress lndollars2 mrp_estimate elected lnpop estado_*
estimates store m6, title(Model 6)

estout * using "Accessory/Table D5.PARTISANSHIP.txt", ///
	cells(b(star fmt(3)) se(par fmt(3))) starl(* 0.1 ** 0.05 *** 0.01) stats(N r2_p r2) style(tab) replace

********************************************************************************
* END TABLE D5
********************************************************************************



********************************************************************************
* Table D6: Response rate, incentive offered, and logged dollars with alternate 
*	measure of partisanship
********************************************************************************

clear
use "Incentives Experiment Replication Data.dta"

# Different estimation of partisanship, mrp_estimate.
# But testing its effects on response, incentive, and dollars spent.

probit response mrp_estimate afterelection population_block Japan China quart_2 quart_3 quart_4 regi_1 regi_2 regi_3
estimates store m1, title(Model 1)

probit response mrp_estimate afterelection population_block Japan China quart_2 quart_3 quart_4 estado_*
estimates store m2, title(Model 2)

probit incentoffered mrp_estimate afterelection population_block Japan China quart_2 quart_3 quart_4 regi_1 regi_2 regi_3
estimates store m3, title(Model 3)

probit incentoffered mrp_estimate afterelection population_block Japan China quart_2 quart_3 quart_4 estado_*
estimates store m4, title(Model 4)

regress lndollars2 mrp_estimate afterelection population_block Japan China quart_2 quart_3 quart_4 regi_1 regi_2 regi_3
estimates store m5, title(Model 5)

regress lndollars2 mrp_estimate afterelection population_block Japan China quart_2 quart_3 quart_4 estado_*
estimates store m6, title(Model 6)

estout * using "Accessory/Table D6.ALTERNATIVE PARTISANSHIP.txt", ///
	cells(b(star fmt(3)) se(par fmt(3))) starl(* 0.1 ** 0.05 *** 0.01) stats(N r2_p r2) style(tab) replace

********************************************************************************
* END TABLE D6
********************************************************************************

	
	
*************************************************************
* TABLE D7 - Randomization Balance Checks
*************************************************************

clear
use "Incentives Experiment Replication Data.dta"

sort beforeelection
by beforeelection: sum lnpop quart_1 quart_2 quart_3 quart_4 regi_1 regi_2 regi_3 regi_4 Japan China dailypaper

# Not really sure what is going on here.
# Some sort of logistic regression but need to explore further to 
# figure out what is exactly being tested.

logit beforeelection lnpop quart_1 quart_2 quart_3 regi_1 regi_2 regi_3 Japan China
outreg2 using "Accessory/Table D7.RandomizationCheck.doc", replace

*****
*add dailypaper, but that decreases number of observations a lot. prob to those with larger population as lnpop becomes significant
logit beforeelection lnpop quart_1 quart_2 quart_3 regi_1 regi_2 regi_3 Japan China dailypaper
outreg2 using "Accessory/Table D7.RandomizationCheck.doc", append

*recode dailypaper (.=0) to see what happens when assuming the missings are zeros. not sure if that's tenable??
gen dailypaper1 = dailypaper
recode dailypaper1 (.=0)
logit beforeelection lnpop quart_1 quart_2 quart_3 regi_1 regi_2 regi_3 Japan China dailypaper1
outreg2 using "Accessory/Table D7.RandomizationCheck.doc", append


*****
*add partisanship measures
*presdem_2008 mrp_estimate
*also drops observations and population again becomes significant. but mostly not anything to worry about here
logit beforeelection lnpop quart_1 quart_2 quart_3 regi_1 regi_2 regi_3 Japan China dailypaper presdem_2008
outreg2 using "Accessory/Table D7.RandomizationCheck.doc", append

logit beforeelection lnpop quart_1 quart_2 quart_3 regi_1 regi_2 regi_3 Japan China dailypaper mrp_estimate
outreg2 using "Accessory/Table D7.RandomizationCheck.doc", append

********************************************************************************
* END TABLE D7
********************************************************************************


********************************************************************************
* TABLE D8: Interaction Effects for the Manufacturing Cities and the Before 
*	Election Treatment
********************************************************************************

clear
use "Incentives Experiment Replication Data.dta"

*this estimate indicates: (before election treamtent given a manufacturing city) minus (after election treatment given non manufacturing city)
*uses single operator #
probit response i.beforeelection#i.manufacturingfocus
*Treatment effect for manufacturing cities is: .5266966-.095204=.4314926. P-value computed as:
test 1.beforeelection#1.manufacturingfocus = 0.beforeelection#1.manufacturingfocus
*Which results in chi2(  1) =    4.13; Prob > chi2 =    0.0422


probit response i.beforeelection#i.manufacturingfocus lnpop quart_1 quart_2 quart_4 regi_2 regi_3 regi_4 Japan China
*Treatment effect for manufacturing cities is: 0.5124004-0.0762859=.4361145. P-value computed as: 
test 1.beforeelection#1.manufacturingfocus = 0.beforeelection#1.manufacturingfocus
*Which results in chi2(  1) =      4.03; Prob > chi2 =    0.0446


probit incentoffered i.beforeelection#i.manufacturingfocus
*Treatment effect for manufacturing cities is: 0.8149803-0.3531292 = .4618511. P-value computed as: 
test 1.beforeelection#1.manufacturingfocus = 0.beforeelection#1.manufacturingfocus
*Which results in chi2(  1) =      4.12; Prob > chi2 =    0.0423

probit incentoffered i.beforeelection#i.manufacturingfocus lnpop quart_1 quart_2 quart_4 regi_2 regi_3 regi_4 Japan China
*Treatment effect for manufacturing cities is: 0.8177595-0.3558607 = .4618988. P-value computed as: 
test 1.beforeelection#1.manufacturingfocus = 0.beforeelection#1.manufacturingfocus
*Which results in chi2(  1) =      3.88; Prob > chi2 =    0.049


regress lndollars2 i.beforeelection#i.manufacturingfocus
*Treatment effect for manufacturing cities is: disp 0.5484879-0.0766626 = .4718253. P-value computed as: 
test 1.beforeelection#1.manufacturingfocus = 0.beforeelection#1.manufacturingfocus
*Which results in chi2(  1) =      5.40; Prob > chi2 =    0.0205

regress lndollars2 i.beforeelection#i.manufacturingfocus lnpop quart_1 quart_2 quart_4 regi_2 regi_3 regi_4 Japan China
*Treatment effect for manufacturing cities is: 0.5665595-0.092624 = .4739355. P-value computed as: 
test 1.beforeelection#1.manufacturingfocus = 0.beforeelection#1.manufacturingfocus
*Which results in chi2(  1) =      5.35; Prob > chi2 =    0.0211

********************************************************************************
* END TABLE D8
********************************************************************************


********************************************************************************
* TABLE D9: EXPLORING ADDITIONAL COVARIATES
********************************************************************************

* Partisan effect results plus additional control variables
clear
use "Incentives Experiment Replication Data.dta"

*From Table D4
probit response presdem_2008 lnpop manufacturingfocus growth mayor2 regi_1 regi_2 regi_3
probit response presdem_2008 lnpop manufacturingfocus growth mayor2 regi_1 regi_2 regi_3 dailypaper unemp_rate
estimates store m1, title(Model 1)

probit incentoffered presdem_2008 lnpop manufacturingfocus growth mayor2 regi_1 regi_2 regi_3
probit incentoffered presdem_2008 lnpop manufacturingfocus growth mayor2 regi_1 regi_2 regi_3 dailypaper unemp_rate
estimates store m2, title(Model 2)	

regress lndollars2 presdem_2008 lnpop manufacturingfocus growth mayor2 regi_1 regi_2 regi_3
regress lndollars2 presdem_2008 lnpop manufacturingfocus growth mayor2 regi_1 regi_2 regi_3 dailypaper unemp_rate
estimates store m3, title(Model 3)	

estout * using "Accessory/Table D9.D4WithMoreControls.txt", ///
	cells(b(star fmt(3)) se(par fmt(3))) starl(* 0.1 ** 0.05 *** 0.01) stats(N r2_p r2) style(tab) replace

********************************************************************************
* END TABLE D9
********************************************************************************
	
	
********************************************************************************
* TABLE D10: PARTISANSHIP WITH STATE FIXED EFFECTS, DAILY PAPER & UNEMPLOYMENT
********************************************************************************
	
clear
use "Incentives Experiment Replication Data.dta"

*From Table D5
probit response presdem_2008 elected lnpop estado_*
probit response presdem_2008 elected lnpop estado_* dailypaper unemp_rate
estimates store m1, title(Model 1)
 
probit response mrp_estimate elected lnpop estado_*
probit response mrp_estimate elected lnpop estado_* dailypaper unemp_rate
estimates store m2, title(Model 2)

probit incentoffered presdem_2008 elected lnpop estado_*
probit incentoffered presdem_2008 elected lnpop estado_* dailypaper unemp_rate
estimates store m3, title(Model 3)

probit incentoffered mrp_estimate elected lnpop estado_*
probit incentoffered mrp_estimate elected lnpop estado_* dailypaper unemp_rate
estimates store m4, title(Model 4)

regress lndollars2 presdem_2008 elected lnpop estado_*
regress lndollars2 presdem_2008 elected lnpop estado_* dailypaper unemp_rate
estimates store m5, title(Model 5)

regress lndollars2 mrp_estimate elected lnpop estado_*
regress lndollars2 mrp_estimate elected lnpop estado_* dailypaper unemp_rate
estimates store m6, title(Model 6)

estout * using "Accessory/Table D10.D5WithMoreControls.txt", ///
	cells(b(star fmt(3)) se(par fmt(3))) starl(* 0.1 ** 0.05 *** 0.01) stats(N r2_p r2) style(tab) replace

********************************************************************************
* END TABLE D10
********************************************************************************

	
********************************************************************************
* TABLE D11: ALL CONDITIONS QUARTERS, REGIONS, STATE FE, DAILY PAPER & UNEMPLOYMENT
********************************************************************************
	
clear
use "Incentives Experiment Replication Data.dta"

*From Table D6
probit response mrp_estimate afterelection population_block Japan China quart_2 quart_3 quart_4 regi_1 regi_2 regi_3
probit response mrp_estimate afterelection population_block Japan China quart_2 quart_3 quart_4 regi_1 regi_2 regi_3 dailypaper unemp_rate
estimates store m1, title(Model 1)

probit response mrp_estimate afterelection population_block Japan China quart_2 quart_3 quart_4 estado_*
probit response mrp_estimate afterelection population_block Japan China quart_2 quart_3 quart_4 estado_* dailypaper unemp_rate
estimates store m2, title(Model 2)

probit incentoffered mrp_estimate afterelection population_block Japan China quart_2 quart_3 quart_4 regi_1 regi_2 regi_3
probit incentoffered mrp_estimate afterelection population_block Japan China quart_2 quart_3 quart_4 regi_1 regi_2 regi_3 dailypaper unemp_rate
estimates store m3, title(Model 3)

probit incentoffered mrp_estimate afterelection population_block Japan China quart_2 quart_3 quart_4 estado_*
probit incentoffered mrp_estimate afterelection population_block Japan China quart_2 quart_3 quart_4 estado_* dailypaper unemp_rate
estimates store m4, title(Model 4)

regress lndollars2 mrp_estimate afterelection population_block Japan China quart_2 quart_3 quart_4 regi_1 regi_2 regi_3
regress lndollars2 mrp_estimate afterelection population_block Japan China quart_2 quart_3 quart_4 regi_1 regi_2 regi_3 dailypaper unemp_rate
estimates store m5, title(Model 5)

regress lndollars2 mrp_estimate afterelection population_block Japan China quart_2 quart_3 quart_4 estado_*
regress lndollars2 mrp_estimate afterelection population_block Japan China quart_2 quart_3 quart_4 estado_* dailypaper unemp_rate
estimates store m6, title(Model 6)

estout * using "Accessory/Table D11.D6WithMoreControls.txt", ///
	cells(b(star fmt(3)) se(par fmt(3))) starl(* 0.1 ** 0.05 *** 0.01) stats(N r2_p r2) style(tab) replace


********************************************************************************
* END TABLE D11
********************************************************************************

