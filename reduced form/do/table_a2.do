* -------------------------------------------------------------------------------------------------- *
*   Generate Table A2 for 														 					 *
*	THE DAMAGES AND DISTORTIONS FROM DISCRIMINATION IN THE RENTAL HOUSING MARKET                     *
*   STATA Version: 17.0                                                                              *                                                             
*   Date Last Modification: 12/15/2022                                                               *
* -------------------------------------------------------------------------------------------------- *

* Clear workspace

clear all

* Set working directory
capture cd ""

* Load Data
use "data\inquiries_screening.dta", clear

*--------------------------------------------------------------------
*Gen race dummies
*--------------------------------------------------------------------
gen Black=(race==3)
gen Hispanic=(race==2)
gen White=(race==1)
gen Minority=(race==3|race==2)

  qui sum screening_term if White==1
  loc r_white_s=r(mean)
  qui sum Income if White==1
  loc r_white_i=r(mean)
    qui sum References if White==1
  loc r_white_r=r(mean)
    qui sum Credit if White==1
  loc r_white_c=r(mean)
  
label variable Hispanic "Hispanic"
label variable Black "African American"


reg screening_term Hispanic Black, vce(cluster address_id)
outreg, ctitle("Any Screening Term") ///
summstat(N) keep(Hispanic Black) stats(b ci) dbldiv(,) starlevels(10 5 1)  starloc(1) bdec(4) store(model_diff) level(90) 

nlcom   (Hispanic: _b[Hispanic]  /`r_white_s' +1) ///
		(Black: _b[Black] /`r_white_s' +1) , post
test Hispanic=1
test Black=1		
outreg,   stats(b ci) dbldiv(,) ///
noautosumm store(model_diff_r)  starlevels(10 5 1) starloc(1) bdec(4) level(90) 

reg Income Hispanic Black, vce(cluster address_id)
outreg, ctitle("Income") ///
summstat(N) keep(Hispanic Black) stats(b ci) dbldiv(,) starlevels(10 5 1)  starloc(1) bdec(4) merge(model_diff) level(90) 

nlcom   (Hispanic: _b[Hispanic]  /`r_white_i' +1) ///
		(Black: _b[Black] /`r_white_i' +1) , post
test Hispanic=1
test Black=1		
outreg,   stats(b ci) dbldiv(,) ///
noautosumm merge(model_diff_r)  starlevels(10 5 1) starloc(1) bdec(4) level(90) 

reg References Hispanic Black, vce(cluster address_id)
outreg, ctitle("References") ///
summstat(N) keep(Hispanic Black) stats(b ci) dbldiv(,) starlevels(10 5 1)  starloc(1) bdec(4) merge(model_diff) level(90) 

nlcom   (Hispanic: _b[Hispanic]  /`r_white_r' +1) ///
		(Black: _b[Black] /`r_white_r' +1) , post
test Hispanic=1
test Black=1		
outreg,   stats(b ci)  dbldiv(,) ///
noautosumm merge(model_diff_r)  starlevels(10 5 1) starloc(1) bdec(4) level(90) 


reg Credit Hispanic Black, vce(cluster address_id)
outreg, ctitle("Credit") ///
summstat(N) keep(Hispanic Black) stats(b ci) dbldiv(,) starlevels(10 5 1)  starloc(1) bdec(4) merge(model_diff) level(90) 


nlcom   (Hispanic: _b[Hispanic]  /`r_white_c' +1) ///
		(Black: _b[Black] /`r_white_c' +1) , post
test Hispanic=1
test Black=1		
outreg,   stats(b ci) dbldiv(,) ///
noautosumm merge(model_diff_r)  starlevels(10 5 1) starloc(1) bdec(4) level(90) 

outreg, replay(model_diff_r) append(model_diff) store(model_diff)

mat sumstat = J(1,5,.)

*All cities
    mat sumstat[1,1] = `r_white_s'
	mat sumstat[1,2] = `r_white_i'
	mat sumstat[1,3] = `r_white_r'
	mat sumstat[1,4] = `r_white_c'
	
matrix rownames sumstat = "Group Mean (White)"

frmttable, statmat(sumstat) store(sumstat) sdec(4)

outreg using "tables/table_a2.tex", replay(model_diff) append(sumstat) tex nocenter note("") fragment plain replace
