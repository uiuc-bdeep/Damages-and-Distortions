* -------------------------------------------------------------------------------------------------- *
*   Generate Table B2 for 														 					 *
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
*LPM
*--------------------------------------------------------------------

qui sum choice if White==1
loc r_white=r(mean)
qui sum choice if White==1 & inquiry_order==1
loc r_white2=r(mean)


* Minority
reg choice Minority, vce(cluster address_id)
outreg, ctitles("" "(1)") ///
noautosumm keep(Minority) stats(b ci) dbldiv(,) starlevels(10 5 1)  starloc(1) bdec(4) level(90) store(all_diff)

nlcom   (t1: _b[Minority]  /`r_white' +1 ) , post
test t1=1
outreg,  rtitle("Minority")  stats(b ci) dbldiv(,) ///
noautosumm starlevels(10 5 1) starloc(1) ctitles("" "(1)") bdec(4) level(90) store(all_diff_r)

reghdfe choice Minority, absorb(gender) vce(cluster address_id) res
outreg, ctitles("" "(2)") ///
noautosumm keep(Minority) stats(b ci) dbldiv(,) starlevels(10 5 1)  starloc(1) bdec(4) merge(all_diff) level(90)

nlcom   (t1: _b[Minority]  /`r_white' +1 ) , post
test t1=1
outreg,  rtitle("Minority") stats(b ci) dbldiv(,) ///
noautosumm merge(all_diff_r)  starlevels(10 5 1) starloc(1) ctitles("" "(2)") bdec(4) level(90)

reghdfe choice Minority, abs(gender education_level) vce(cluster address_id)
outreg, ctitles("" "(3)") ///
noautosumm keep(Minority) stats(b ci) dbldiv(,) starlevels(10 5 1)  starloc(1) bdec(4) merge(all_diff) level(90) 

nlcom   (t1: _b[Minority]  /`r_white' +1 ) , post
test t1=1
outreg,  rtitle("Minority") stats(b ci) dbldiv(,) ///
noautosumm merge(all_diff_r)  starlevels(10 5 1) starloc(1) ctitles("" "(3)") bdec(4) level(90) 

reghdfe choice Minority, abs(gender education_level inquiry_order) vce(cluster address_id)
outreg, ctitles("" "(4)") ///
noautosumm keep(Minority) stats(b ci) dbldiv(,) starlevels(10 5 1)  starloc(1) bdec(4) merge(all_diff) level(90) 

nlcom   (t1: _b[Minority]  /`r_white' +1 ) , post
test t1=1
outreg,  rtitle("Minority") stats(b ci) dbldiv(,) ///
noautosumm merge(all_diff_r)  starlevels(10 5 1) starloc(1) ctitles("" "(4)") bdec(4) level(90) 

reghdfe choice Minority if inquiry_order==1, abs(gender education_level) vce(cluster address_id)
outreg, ctitles("" "(5)") ///
noautosumm keep(Minority) stats(b ci) dbldiv(,) starlevels(10 5 1)  starloc(1) bdec(4) merge(all_diff) level(90) store(all_diff)

nlcom   (t1: _b[Minority]  /`r_white2' +1 ) , post
test t1=1
outreg,  rtitle("Minority") stats(b ci) dbldiv(,) ///
noautosumm merge(all_diff_r)  starlevels(10 5 1) starloc(1) ctitles("" "(5)") bdec(4) level(90) store(all_diff_r)

qui sum choice if White==1
loc r_white=r(mean)
qui sum choice if White==1 & inquiry_order==1
loc r_white2=r(mean)


* Minority
reg choice Minority, vce(cluster address_id)
outreg, ctitles("" "(1)") ///
noautosumm keep(Minority) stats(b ci) dbldiv(,) starlevels(10 5 1)  starloc(1) bdec(4) level(90) store(all_diff)

nlcom   (t1: _b[Minority]  /`r_white' +1 ) , post
test t1=1
outreg,  rtitle("Minority")  stats(b ci) dbldiv(,) ///
noautosumm starlevels(10 5 1) starloc(1) ctitles("" "(1)") bdec(4) level(90) store(all_diff_r)

reghdfe choice Minority, absorb(gender) vce(cluster address_id) res
outreg, ctitles("" "(2)") ///
noautosumm keep(Minority) stats(b ci) dbldiv(,) starlevels(10 5 1)  starloc(1) bdec(4) merge(all_diff) level(90)

nlcom   (t1: _b[Minority]  /`r_white' +1 ) , post
test t1=1
outreg,  rtitle("Minority") stats(b ci) dbldiv(,) ///
noautosumm merge(all_diff_r)  starlevels(10 5 1) starloc(1) ctitles("" "(2)") bdec(4) level(90)

reghdfe choice Minority, abs(gender education_level) vce(cluster address_id)
outreg, ctitles("" "(3)") ///
noautosumm keep(Minority) stats(b ci) dbldiv(,) starlevels(10 5 1)  starloc(1) bdec(4) merge(all_diff) level(90) 

nlcom   (t1: _b[Minority]  /`r_white' +1 ) , post
test t1=1
outreg,  rtitle("Minority") stats(b ci) dbldiv(,) ///
noautosumm merge(all_diff_r)  starlevels(10 5 1) starloc(1) ctitles("" "(3)") bdec(4) level(90) 

reghdfe choice Minority, abs(gender education_level inquiry_order) vce(cluster address_id)
outreg, ctitles("" "(4)") ///
noautosumm keep(Minority) stats(b ci) dbldiv(,) starlevels(10 5 1)  starloc(1) bdec(4) merge(all_diff) level(90) 

nlcom   (t1: _b[Minority]  /`r_white' +1 ) , post
test t1=1
outreg,  rtitle("Minority") stats(b ci) dbldiv(,) ///
noautosumm merge(all_diff_r)  starlevels(10 5 1) starloc(1) ctitles("" "(4)") bdec(4) level(90) 

reghdfe choice Minority if inquiry_order==1, abs(gender education_level) vce(cluster address_id)
outreg, ctitles("" "(5)") ///
noautosumm keep(Minority) stats(b ci) dbldiv(,) starlevels(10 5 1)  starloc(1) bdec(4) merge(all_diff) level(90) store(all_diff)

nlcom   (t1: _b[Minority]  /`r_white2' +1 ) , post
test t1=1
outreg,  rtitle("Minority") stats(b ci) dbldiv(,) ///
noautosumm merge(all_diff_r)  starlevels(10 5 1) starloc(1) ctitles("" "(5)") bdec(4) level(90) store(all_diff_r)

*hisp and black
reg choice Black Hispanic, vce(cluster address_id)
outreg, ctitle("" "(1)") ///
summstat(N) keep(Black Hispanic) stats(b ci) dbldiv(,) starlevels(10 5 1)  starloc(1) bdec(4) store(all_diff2) level(90)

nlcom   (t2: _b[Black]  /`r_white' +1) ///
		(t3: _b[Hispanic] /`r_white' +1) , post
test t2=1
test t3=1
outreg,  rtitle("African American"\""\"Hispanic")  stats(b ci) dbldiv(,) ///
noautosumm store(all_diff_r2)  starlevels(10 5 1) starloc(1) ctitles("" "(1)") bdec(4) level(90) 

reghdfe choice Black Hispanic, abs(gender) vce(cluster address_id)
outreg, ctitle("" "(2)") ///
summstat(N) keep(Black Hispanic) stats(b ci) dbldiv(,) starlevels(10 5 1)  starloc(1) bdec(4) merge(all_diff2) level(90)

nlcom   (t2: _b[Black]  /`r_white' +1) ///
		(t3: _b[Hispanic] /`r_white' +1) , post
test t2=1
test t3=1
outreg,  rtitle("African American"\""\"Hispanic")  stats(b ci) dbldiv(,) ///
noautosumm merge(all_diff_r2)  starlevels(10 5 1) starloc(1) ctitles("" "(2)") bdec(4) level(90) 


reghdfe choice Black Hispanic, abs(gender education_level) vce(cluster address_id)
outreg, ctitle("" "(3)") ///
summstat(N) keep(Black Hispanic) stats(b ci) dbldiv(,) starlevels(10 5 1)  starloc(1) bdec(4) merge(all_diff2) level(90)

nlcom   (t2: _b[Black]  /`r_white' +1) ///
		(t3: _b[Hispanic] /`r_white' +1) , post
test t2=1
test t3=1
outreg,  rtitle("African American"\""\"Hispanic")  stats(b ci) dbldiv(,) ///
noautosumm merge(all_diff_r2)  starlevels(10 5 1) starloc(1) ctitles("" "(3)") bdec(4) level(90) 

reghdfe choice Black Hispanic, abs(gender education_level inquiry_order) vce(cluster address_id)
outreg, ctitle("" "(4)") ///
summstat(N) keep(Black Hispanic) stats(b ci) dbldiv(,) starlevels(10 5 1)  starloc(1) bdec(4) merge(all_diff2) level(90)

nlcom   (t2: _b[Black]  /`r_white' +1) ///
		(t3: _b[Hispanic] /`r_white' +1) , post
test t2=1
test t3=1
outreg,  rtitle("African American"\""\"Hispanic")  stats(b ci) dbldiv(,) ///
noautosumm merge(all_diff_r2)  starlevels(10 5 1) starloc(1) ctitles("" "(4)") bdec(4) level(90) 

reghdfe choice Black Hispanic if inquiry_order==1, abs(gender education_level) vce(cluster address_id)
outreg, ctitle("" "(5)") ///
summstat(N) keep(Black Hispanic) stats(b ci) dbldiv(,) starlevels(10 5 1)  starloc(1) bdec(4) merge(all_diff2) level(90)

nlcom   (t2: _b[Black]  /`r_white2' +1) ///
		(t3: _b[Hispanic] /`r_white2' +1) , post
test t2=1
test t3=1
outreg,  rtitle("African American"\""\"Hispanic")  stats(b ci) dbldiv(,) ///
noautosumm merge(all_diff_r2)  starlevels(10 5 1) starloc(1) ctitles("" "(5)") bdec(4) level(90) 


outreg, replay(all_diff) append(all_diff2) store(all_diff)
outreg, replay(all_diff_r) append(all_diff_r2) store(all_diff_r)
outreg, replay(all_diff_r) append(all_diff) store(all)
*--------------------------------------------------------------------
*overall means
*--------------------------------------------------------------------
mat overall=J(1,5,.)

mat overall[1,1] = `r_white'
mat overall[1,2] = `r_white'
mat overall[1,3] = `r_white'
mat overall[1,4] = `r_white'
mat overall[1,5] = `r_white2'

matrix rownames overall = "Mean White (Overall)" 
frmttable, statmat(overall) store(overall) sdec(4)

outreg using "tables/table_b2.tex", replay(all) append(overall) tex nocenter note("") fragment plain replace

