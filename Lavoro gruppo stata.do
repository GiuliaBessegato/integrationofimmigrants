*ssc install
ssc install ciplot, replace
	ssc install catplot, replace
	ssc install blindschemes, replace
	ssc install combomarginsplot, replace
	ssc install outreg2, replace
	ssc install estout, replace
	
cd "C:\Users\giuli\OneDrive\Desktop\1. Corsi\quantitative methods lab\lavoro gruppo stata"
use "ESS7e02_2 (1).dta", clear

*immigrant sample
tab ctzcntr, m
codebook ctzcntr
drop if ctzcntr==.a // refusal
drop if ctzcntr==.b // don't know
drop if ctzcntr==.c //no answer
tab ctzcntr

label list ctzcntr
drop if ctzcntr==1
gen migrants = ctzcntr 
tab migrants

*country in Europe
*country in Europe
tab cntry
encode cntry, gen(country)
tab country
tab country, nol
drop if country == 4
drop if country == 13
drop if country == 14
drop if country == 19
tab country 
recode country (11 12 = 1 "Liberal") (1 2 3 5 10 15= 2 "Conservative") (6 9 16 18 = 3 "Social democratic") (8 17 = 4 "Mediterranean model") (7 = 5 "Hybrid model"), gen(country1)
lab def country1 1 "Liberal" 2 "Conservative" 3 "Social democratic" 4 "Mediterranean model" 5 "Hybrid model", replace
tab country1
codebook country1


*VARIABILE X"

* we wanted to create a new variable merging emplrel and uemp12m, so being able to do a comparison between the working and not working and their social integration but if we merge the to variables we end up with 58 missing out of 99 cases*/ 
gen employue = emplrel + uemp12m 
cap drop empl_x
gen empl_x = . 
replace empl_x = 0 if uemp12m==1 // unemployed 
replace empl_x = 1 if inlist(emplrel,1,2,3) & mi(empl_x) // employee
lab def empl_x 0 "Not Employed" 1 "Employed", replace
lab val empl_x empl_x

*VARIABILE Y*
tab fclcntr
codebook fclcntr
drop if fclcntr==.b
tab fclcntr migrants
recode fclcntr (1 2 = 1 "feel close" ) (3 4 = 0 "not close"), gen(fclcntr1)
lab def fclcntr1 1 "Feel close" 0 "Not close", replace
tab fclcntr1
codebook fclcntr1

*VARIABILE Z*

*gender
codebook gndr

*education 
tab eisced, m
codebook eisced 
drop if eisced ==.a // refusal
drop if eisced ==.b // don't know
drop if eisced ==.c //no answer
recode eisced (1 2 3 = 1 "low" ) (4 5 = 2 "medium") ///
           (6 7 = 3 "high" ) (else = .), gen (edu)
lab def edu 1 "Low" 2 "Medium" 3 "High", replace
tab edu, m
drop if edu==.

*age
drop if agea > 65
tab agea

*OPERAZIONI

*grafici
twoway (scatter fclcntr1 empl_x)(lfitci fclcntr1 empl_x)
twoway (scatter fclcntr1 empl_x) || (lfit fclcntr1 empl_x)
graph dot fclcntr1 empl_x 
catplot fclcntr1 empl_x, percent(empl_x)
catplot fclcntr1 empl_x
catplot fclcntr1 empl_x 
catplot fclcntr1 empl_x, percent(empl_x) 						///
		bar(1, bcolor(black) bfcolor(sea)) 					///
		blabel(bar, pos(outside) format(%3.1f) size(3)) 	///
		ylabel(0(25)100) 									///
		xsize(10) ysize(5) 									///
		ytitle("Percentage", place(e)) 						///
		title("Integration and employment status of immigrants in Europe") 
graph save "output/Graph2.gph", replace
graph save "output/Graph2.jpg", replace

*margini e regressioni
logit fclcntr1 empl_x //X e Y
margins, dydx(*) post 
est store a1

logit fclcntr1 empl_x i.country1 //X(considering country) e Y
margins, dydx(*) post 
est store a2

logit fclcntr1 empl_x i.country1 i.gndr //Z=gender
margins, dydx(*) post 
est store a3

logit fclcntr1 empl_x i.country1 i.gndr i.edu //Z=gender and level of education
margins, dydx(*) post 
est store a4
outreg2 using "output/esttab.doc", replace

margins, dydx(empl_x) over(country1) noestimcheck
marginsplot, yline(0)
marginsplot, recast (bar)
marginsplot, recast (line) recastci(rarea) yline(0)
coefplot a1 a2 a3 a4, xline(0)
coefplot a1 a2 a3 a4, xline(0) keep(empl_x)

esttab a1 a2 a3 a4
esttab a1 a2 a3 a4 using "output/log1.rtf"

outreg2 using "output/esttab.doc", replace

logit fclcntr1 empl_x i.country1 //model 2
logit fclcntr1 empl_x country i.gndr //model 3
logit fclcntr1 empl_x i.country1 i.gndr i.edu //model 4
logit fclcntr1 empl_x i.country1 i.gndr i.edu c.agea //model 5

twoway function y = logistic(x), range(0 1) xtitle("employment") ytitle("feeling close")
twoway (scatter fclcntr1 empl_x)
twoway (scatter fclcntr1 empl_x) || (lfit fclcntr1 empl_x)