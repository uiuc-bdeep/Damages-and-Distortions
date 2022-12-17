* -------------------------------------------------------------------------------------------------- *
*   Generate Table B1 for 														 					 *
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


label variable Hispanic "Hispanic"
label variable Black "African American"

*--------------------------------------------------------------------
*Gen other variables
*--------------------------------------------------------------------
gen first=(inquiry_order==1)
gen second=(inquiry_order==2)
gen third=(inquiry_order==3)
gen Male=(gender=="male")
gen Female=(gender=="female")
gen Low=(education_level=="low")
gen Medium=(education_level=="medium")
gen High=(education_level=="high")


*--------------------------------------------------------------------
*Difference in means (balance)
*--------------------------------------------------------------------

reg first Hispanic Black, vce(cluster address_id)	
outreg, ctitles("" "First") rtitle("Hispanic"\""\"African American")  stats(b ci) dbldiv(,) ///
noautosum store(order)  starlevels(10 5 1) starloc(1) bdec(4) level(90) nocons
							
reg second Hispanic Black, vce(cluster address_id)	
outreg, ctitles("" "Second") rtitle("Hispanic"\""\"African American")  stats(b ci) dbldiv(,) ///
noautosum merge(order)  starlevels(10 5 1) starloc(1) bdec(4) level(90) nocons

reg third Hispanic Black, vce(cluster address_id)	
outreg, ctitles("" "Third") rtitle("Hispanic"\""\"African American")  stats(b ci) dbldiv(,) ///
noautosum merge(order)  starlevels(10 5 1) starloc(1) bdec(4) level(90) nocons
							
reg Male Hispanic Black, vce(cluster address_id)	
outreg, ctitles("" "Male") rtitle("Hispanic"\""\"African American")  stats(b ci) dbldiv(,) ///
summstat(N) store(gender)  starlevels(10 5 1) starloc(1) bdec(4) level(90) nocons
							
reg Female Hispanic Black, vce(cluster address_id)	
outreg, ctitles("" "Female") rtitle("Hispanic"\""\"African American")  stats(b ci) dbldiv(,) ///
summstat(N) merge(gender)  starlevels(10 5 1) starloc(1) bdec(4) level(90) nocons

reg Low Hispanic Black, vce(cluster address_id)	
outreg, ctitles("" "Low") rtitle("Hispanic"\""\"African American")  stats(b ci) dbldiv(,) ///
summstat(N) merge(gender)  starlevels(10 5 1) starloc(1) bdec(4) level(90) nocons

reg Medium Hispanic Black, vce(cluster address_id)	
outreg, ctitles("" "Medium") rtitle("Hispanic"\""\"African American")  stats(b ci) dbldiv(,) ///
summstat(N) merge(gender)  starlevels(10 5 1) starloc(1) bdec(4) level(90) nocons
	
reg High Hispanic Black, vce(cluster address_id)	
outreg, ctitles("" "High") rtitle("Hispanic"\""\"African American")  stats(b ci) dbldiv(,) ///
summstat(N) merge(gender)  starlevels(10 5 1) starloc(1) bdec(4) level(90) nocons
		
outreg, replay(order) append(gender) store(all)		

outreg using "tables/table_b1.tex", replay(all) tex nocenter note("") fragment plain replace