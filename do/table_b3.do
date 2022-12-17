* -------------------------------------------------------------------------------------------------- *
*   Generate Table B3 for 														 					 *
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
gen Black3bed=(race=="black" & bed3bath2==1)
gen Hispanic=(race=="hispanic")
gen Hispanic3bed=(race=="hispanic" & bed3bath2==1)
gen White=(race=="white")
gen Minority=(race=="black"|race=="hispanic")
gen Minority3bed=(Minority==1 & bed3bath2==1)



label variable Hispanic "Hispanic"
label variable Black "African American"


  qui sum choice if White==1
  loc r_white=r(mean)
  
* run model for All Cities *
reghdfe choice  Minority Minority3bed bed3bath2, absorb(gender  education_level inquiry_order  address_id) cl(city) level(90)  nocons res
outreg, ctitle("" "Difference in Means") ///
noautosumm keep(Minority Minority3bed) stats(b ci) dbldiv(,) starlevels(10 5 1)  starloc(1) bdec(4) store(model1_diff) level(90) 

nlcom   (Minority :_b[Minority]  /`r_white' +1) ///
		(Minority3bed :_b[Minority3bed]  /`r_white' +1) , post
test Minority=1
test Minority3bed=1
outreg,  stats(b ci) dbldiv(,) ///
noautosumm store(model_diff_r)  starlevels(10 5 1) starloc(1) bdec(4) level(90) ctitle("" "Conditional Relative Response")

reghdfe choice  Hispanic Hispanic3bed Black  Black3bed , absorb(gender  education_level inquiry_order  address_id) cl(city) level(90)  nocons res
outreg, ctitle("" "Difference in Means") ///
summstat(N\df_a_nested) keep(Hispanic Hispanic3bed Black Black3bed) stats(b ci) dbldiv(,) starlevels(10 5 1)  starloc(1) bdec(4) store(model2_diff) level(90) 

nlcom   (Hispanic: _b[Hispanic]  /`r_white' +1) ///
		(Hispanic3bed: _b[Hispanic3bed] /`r_white' +1) ///
		(Black: _b[Black] /`r_white' +1) ///
		(Black3bed: _b[Black3bed] /`r_white' +1) , post
test Hispanic=1
test Hispanic3bed=1		
test Black=1
test Black3bed=1		
outreg,   stats(b ci) dbldiv(,) ///
summstat(N\df_a_nested) store(model_diff_r2)  starlevels(10 5 1) starloc(1) bdec(4) level(90)  ctitle("" "Conditional Relative Response")

outreg, replay(model1_diff) append(model2_diff) store(model_diff)
outreg, replay(model_diff_r) append(model_diff_r2) store(model_diff_r)
outreg, replay(model_diff_r) merge(model_diff) store(model_diff)

*--------------------------------------------------------------------
*overall means
*--------------------------------------------------------------------
mat overall=J(1,2,.)

mat overall[1,1] = `r_white'
mat overall[1,2] = `r_white'


matrix rownames overall = "Mean White (Overall)" 
frmttable, statmat(overall) store(overall) sdec(4)

outreg using "tables/table_b3.tex", replay(model_diff) append(overall) tex nocenter note("") fragment plain replace

