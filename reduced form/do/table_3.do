* -------------------------------------------------------------------------------------------------- *
*   Generate Table 3 for 														 					 *
*	THE DAMAGES AND DISTORTIONS FROM DISCRIMINATION IN THE RENTAL HOUSING MARKET                     *
*   STATA Version: 17.0                                                                              *                                                             
*   Date Last Modification: 12/15/2022                                                               *
* -------------------------------------------------------------------------------------------------- *

* Clear workspace

clear all

* Set working directory
capture cd ""

* Load Data
use "data\matchedinquiriesall.dta", clear

*--------------------------------------------------------------------
*Gen race dummies
*--------------------------------------------------------------------
gen Black=(race=="black")
gen Hispanic=(race=="hispanic")
gen White=(race=="white")
gen Minority=(race=="black"|race=="hispanic")

destring days_on_trulia, force replace
rename days_on_trulia days_on_trulia_num

gen days3 = (days_on_trulia_num>=0 & days_on_trulia_num<3)
gen days7 = (days_on_trulia_num>=3 & days_on_trulia_num<=7)
gen days7plus = (days_on_trulia_num>7)

gen Minority1=(Minority==1 & days3==1)
gen Minority2=(Minority==1 & days7==1)
gen Minority3=(Minority==1 & days7plus==1)

gen White1=(Minority==0 & days3==1)
gen White2=(Minority==0 & days7==1)
gen White3=(Minority==0 & days7plus==1)

gen Hispanic1=(Hispanic==1 & days3==1)
gen Hispanic2=(Hispanic==1 & days7==1)
gen Hispanic3=(Hispanic==1 & days7plus==1)

gen Black1=(Black==1 & days3==1)
gen Black2=(Black==1 & days7==1)
gen Black3=(Black==1 & days7plus==1)

label variable Hispanic "Hispanic"
label variable Black "African American"

label variable Minority1 "Minority: 0-3 Days"
label variable Minority2 "Minority: 3-7 Days"
label variable Minority3 "Minority: 7+ Days"

label variable Hispanic1 "Hispanic: 0-3 Days"
label variable Hispanic2 "Hispanic: 3-7 Days"
label variable Hispanic3 "Hispanic: 7+ Days"

label variable Black1 "Black: 0-3 Days"
label variable Black2 "Black: 3-7 Days"
label variable Black3 "Black: 7+ Days"


*--------------------------------------------------------------------
*Difference in Means
*--------------------------------------------------------------------
  qui sum choice if White==1
  loc r_white=r(mean)
  
* run model for All Cities *
reg choice  Minority1 Minority2 Minority3 , vce(cluster address_id)
outreg, ctitle("Difference in Means") ///
summstat(N\N_clust) keep(Minority1 Minority2 Minority3) stats(b ci) dbldiv(,) starlevels(10 5 1)  starloc(1) bdec(4) store(model1_diff) level(90) 

nlcom   (Minority1 :_b[Minority1]  /`r_white' +1) ///
		(Minority2 :_b[Minority2]  /`r_white' +1) ///
		(Minority3 :_b[Minority3]  /`r_white' +1) , post
	test Minority1=1
	test Minority2=1 
	test Minority3=1
outreg,  stats(b ci) dbldiv(,) ///
summstat(N\N_clust) store(model_diff_r)  starlevels(10 5 1) starloc(1) bdec(4) level(90) 

reg choice  Hispanic1 Hispanic2 Hispanic3 Black1 Black2 Black3 , vce(cluster address_id)
outreg, ctitle("Difference in Means") ///
summstat(N\N_clust) keep(Hispanic1 Hispanic2 Hispanic3 Black1 Black2 Black3) stats(b ci) dbldiv(,) starlevels(10 5 1)  starloc(1) bdec(4) store(model2_diff) level(90) 

nlcom   (Hispanic1: _b[Hispanic1]  /`r_white' +1) ///
		(Hispanic2: _b[Hispanic2] /`r_white' +1) ///
		(Hispanic3: _b[Hispanic3] /`r_white' +1) ///
		(Black1: _b[Black1] /`r_white' +1) ///
		(Black2: _b[Black2] /`r_white' +1) ///
		(Black3: _b[Black3] /`r_white' +1), post
	test Hispanic1=1
	test Hispanic2=1 
	test Hispanic3=1
	test Black1=1
	test Black2=1 
	test Black3=1
outreg,   stats(b ci) dbldiv(,) ///
summstat(N\N_clust) store(model_diff_r2)  starlevels(10 5 1) starloc(1) bdec(4) level(90) 

outreg, replay(model1_diff) append(model2_diff) store(model_diff)
outreg, replay(model_diff_r) append(model_diff_r2) store(model_diff_r)

outreg, replay(model_diff_r) merge(model_diff) store(model_diff)

************************************************************************************************
*Summary Statistics
************************************************************************************************
mat sumstat = J(13,2,.)
mat list sumstat
*All cities
    quietly: summarize choice if Minority1==1 
    mat sumstat[1,1] = r(mean)
    quietly: summarize choice if Minority2==1 
    mat sumstat[2,1] = r(mean)
	quietly: summarize choice if Minority3==1 
    mat sumstat[3,1] = r(mean)
    quietly: summarize choice if Hispanic1==1 
    mat sumstat[4,1] = r(mean)
    quietly: summarize choice if Hispanic2==1 
    mat sumstat[5,1] = r(mean)
	quietly: summarize choice if Hispanic3==1 
    mat sumstat[6,1] = r(mean)
    quietly: summarize choice if Black1==1 
    mat sumstat[7,1] = r(mean)
    quietly: summarize choice if Black2==1 
    mat sumstat[8,1] = r(mean)
	quietly: summarize choice if Black3==1 
    mat sumstat[9,1] = r(mean)
    quietly: summarize choice if White1==1 
    mat sumstat[10,1] = r(mean)
    quietly: summarize choice if White2==1 
    mat sumstat[11,1] = r(mean)
	quietly: summarize choice if White3==1 
    mat sumstat[12,1] = r(mean)	
	quietly: summarize choice if White==1 
    mat sumstat[13,1] = r(mean)	
	
		
    quietly: summarize choice if Minority1==1 
    mat sumstat[1,2] = r(mean)
    quietly: summarize choice if Minority2==1 
    mat sumstat[2,2] = r(mean)
	quietly: summarize choice if Minority3==1 
    mat sumstat[3,2] = r(mean)
    quietly: summarize choice if Hispanic1==1 
    mat sumstat[4,2] = r(mean)
    quietly: summarize choice if Hispanic2==1 
    mat sumstat[5,2] = r(mean)
	quietly: summarize choice if Hispanic3==1 
    mat sumstat[6,2] = r(mean)
    quietly: summarize choice if Black1==1 
    mat sumstat[7,2] = r(mean)
    quietly: summarize choice if Black2==1 
    mat sumstat[8,2] = r(mean)
	quietly: summarize choice if Black3==1 
    mat sumstat[9,2] = r(mean)
    quietly: summarize choice if White1==1 
    mat sumstat[10,2] = r(mean)
    quietly: summarize choice if White2==1 
    mat sumstat[11,2] = r(mean)
	quietly: summarize choice if White3==1 
    mat sumstat[12,2] = r(mean)		
	quietly: summarize choice if White==1 
    mat sumstat[13,2] = r(mean)	
	
	
matrix rownames sumstat = "Minority: 0-3 Days"  "Minority: 3-7 Days" "Minority: 7+ Days" "Hispanic: 0-3 Days" ///
"Hispanic: 3-7 Days" "Hispanic: 7+ Days" "Black: 0-3 Days" "Black: 3-7 Days" "Black: 7+ Days" ///
"White: 0-3 Days" "White: 3-7 Days" "White: 7+ Days" "Overall"

frmttable, statmat(sumstat) store(sumstat) sdec(4)

outreg using "tables/table_3.tex", replay(model_diff) append(sumstat) tex nocenter note("") fragment plain replace
