* -------------------------------------------------------------------------------------------------- *
*   Generate Table B5 for 														 					 *
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

gen Minority1=(Minority==1 & gender=="female")
gen Minority2=(Minority==1 & gender=="male")

gen Hispanic1=(Hispanic==1 & gender=="female")
gen Hispanic2=(Hispanic==1 & gender=="male")

gen Black1=(Black==1 & gender=="female")
gen Black2=(Black==1 & gender=="male")

gen White1=(Minority==0 & gender=="female")
gen White2=(Minority==0 & gender=="male")


label variable Minority1 "Minority: Female Name"
label variable Minority2 "Minority: Male Name"


label variable Hispanic1 "Hispanic: Female Name"
label variable Hispanic2 "Hispanic: Male Name"

label variable Black1 "Black: Female Name"
label variable Black2 "Black: Male Name"


*--------------------------------------------------------------------
*Difference in means
*--------------------------------------------------------------------

qui sum choice if White==1
loc r_white=r(mean)
  
reg choice  Minority1 Minority2 , vce(cluster address_id)
outreg, ctitle("" "Difference in Means") ///
summstat(N\N_clust) keep(Minority1 Minority2) stats(b ci) dbldiv(,) starlevels(10 5 1)  starloc(1) bdec(4) store(model1_diff) level(90)

nlcom   (Minority1 :_b[Minority1]  /`r_white' +1) ///
		(Minority2 :_b[Minority2]  /`r_white' +1) , post
test Minority1=1
test Minority2=1		
outreg,  stats(b ci) dbldiv(,) ///
summstat(N\N_clust) store(model_diff_r)  starlevels(10 5 1) starloc(1) bdec(4) level(90) ctitle("" "Conditional Relative Response")

reg choice  Hispanic1 Hispanic2 Black1  Black2 , vce(cluster address_id)
outreg, ctitle("" "Difference in Means") ///
summstat(N\N_clust) keep(Hispanic1 Hispanic2 Black1 Black2) stats(b ci) dbldiv(,) starlevels(10 5 1)  starloc(1) bdec(4) store(model2_diff) level(90) 

nlcom   (Hispanic1 : _b[Hispanic1]  /`r_white' +1) ///
		(Hispanic2 : _b[Hispanic2] /`r_white' +1) ///
		(Black1 : _b[Black1] /`r_white' +1) ///
		(Black2 : _b[Black2] /`r_white' +1) , post
test Hispanic1=1
test Hispanic2=1	
test Black1=1
test Black2=1		
outreg,   stats(b ci) dbldiv(,) ///
summstat(N\N_clust) store(model_diff_r2)  starlevels(10 5 1) starloc(1) bdec(4) level(90) ctitle("" "Conditional Relative Response")

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

outreg using "tables/table_b5.tex", replay(model_diff) append(overall) tex nocenter note("") fragment plain replace
