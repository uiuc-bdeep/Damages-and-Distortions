* -------------------------------------------------------------------------------------------------- *
*   Generate Table 2 for 														 					 *
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
*Gen race dummies and city indicators
*--------------------------------------------------------------------
gen Black=(race=="black")
gen Hispanic=(race=="hispanic")
gen White=(race=="white")
gen Minority=(race=="black"|race=="hispanic")



label variable Hispanic "Hispanic"
label variable Black "African American"

sum choice if White==1
sum choice if Minority==1
sum choice if Minority==0

replace trial="SanJose" if trial=="San Jose"

encode(trial), gen(cbsa)
tabulate cbsa, gen(city)

************************************************************************************************
*Difference in means 
************************************************************************************************
  qui sum choice if White==1
  loc r_white=r(mean)

foreach c in "Atlanta" "Houston" "Philadelphia" "Cleveland" "SanJose"{
  qui sum choice if White==1 & trial=="`c'"
  loc r_white_`c'=r(mean)
}

reg choice Minority, vce(cluster address_id)
outreg, keep(Minority) rtitle("Minority") stats(b ci) dbldiv(,) ///
noautosumm store(all_diff)  starlevels(10 5 1) starloc(1) ctitles("" "All Cities") bdec(4) level(90) 

nlcom   ( t1: _b[Minority]  /`r_white' +1) , post
test t1=1
outreg,  rtitle("Minority") stats(b ci) dbldiv(,) ///
noautosumm store(all_diff_r)  starlevels(10 5 1) starloc(1) ctitles("" "All Cities") bdec(4) level(90) 


reg choice Black Hispanic, vce(cluster address_id)
outreg, drop(_cons) rtitle("African American"\""\"Hispanic") ///
stats(b ci) dbldiv(,) summstat(N\N_clust) stor(all_diff2)  starlevels(10 5 1)  starloc(1)  bdec(4) level(90)

nlcom   (t2: _b[Black]  /`r_white' +1) ///
		(t3: _b[Hispanic] /`r_white' +1) , post
		test t2=1
		test t3=1
outreg,  rtitle("African American"\""\"Hispanic")  stats(b ci) dbldiv(,) ///
noautosumm store(all_diff_r2)  starlevels(10 5 1) starloc(1) ctitles("All Cities") bdec(4) level(90) 


foreach c in "Atlanta" "Houston" "Philadelphia" "Cleveland" "SanJose"{
reg choice Minority if trial=="`c'", vce(cluster address_id)
outreg, keep(Minority)  rtitle("Minority") stats(b ci) dbldiv(,) ///
noautosumm store(diff1_`c')  starlevels(10 5 1) starloc(1) bdec(4) level(90)

nlcom   (t1: _b[Minority]  /`r_white_`c'' +1) , post
test t1=1
outreg,  rtitle("Minority") stats(b ci) dbldiv(,) ///
noautosumm store(diff_r_`c')  starlevels(10 5 1) starloc(1) bdec(4) level(90) 

		
reg choice Black Hispanic if trial=="`c'", vce(cluster address_id)
outreg, keep(Black Hispanic) rtitle("African American"\""\"Hispanic") ///
stats(b ci) dbldiv(,) summstat(N\N_clust) store(diff2_`c')  starlevels(10 5 1)  starloc(1) bdec(4) level(90)

nlcom   (t2: _b[Black]  /`r_white_`c'' +1) ///
		(t3: _b[Hispanic] /`r_white_`c'' +1) , post
		test t2=1
		test t3=1
outreg,  rtitle("African American"\""\"Hispanic")  stats(b ci) dbldiv(,) ///
noautosumm store(diff_r2_`c')  starlevels(10 5 1) starloc(1) ctitles("All Cities") bdec(4) level(90) 
}

outreg, replay(all_diff) append(all_diff2) store(all_diff) 
foreach c in "Atlanta" "Houston" "Philadelphia" "Cleveland" "SanJose"{
outreg, replay(diff1_`c') append(diff2_`c') store(diff_`c') ctitles("",`c') level(90)
outreg, replay(all_diff) merge(diff_`c') store (all_diff)  level(90)
}		

outreg, replay(all_diff_r) append(all_diff_r2) store(all_diff_r) 
foreach c in "Atlanta" "Houston" "Philadelphia" "Cleveland" "SanJose"{
outreg, replay(diff_r_`c') append(diff_r2_`c') store(diff_r_`c') ctitles("",`c') level(90)
outreg, replay(all_diff_r) merge(diff_r_`c') store (all_diff_r)  level(90)
}	

outreg, replay(all_diff_r) append(all_diff) store(all) level(90)
************************************************************************************************
*Summary Statistics
************************************************************************************************

mat sumstat = J(4,6,.)

*All cities
    quietly: summarize choice if Minority==1 
    mat sumstat[1,1] = r(mean)
    quietly: summarize choice if Black==1 
    mat sumstat[2,1] = r(mean)
	quietly: summarize choice if Hispanic==1 
    mat sumstat[3,1] = r(mean)
	quietly: summarize choice if Minority==0 
    mat sumstat[4,1] = r(mean)
mat list sumstat
*by city
local i = 2
foreach c in "Atlanta" "Houston" "Philadelphia" "Cleveland" "SanJose" {
    quietly: summarize choice if Minority==1 & trial == "`c'"
    mat sumstat[1,`i'] = r(mean)
    quietly: summarize choice if Black==1 & trial == "`c'"
    mat sumstat[2,`i'] = r(mean)
	quietly: summarize choice if Hispanic==1 & trial == "`c'"
    mat sumstat[3,`i'] = r(mean)
	quietly: summarize choice if Minority==0 & trial == "`c'"
    mat sumstat[4,`i'] = r(mean)
	local i = `i' + 1
}
mat list sumstat
matrix rownames sumstat = "Minority" "African American" "Hispanic" "White"

frmttable, statmat(sumstat) store(sumstat) sdec(4)


outreg using "tables\table_2.tex", replay(all) append(sumstat) tex nocenter note("") fragment plain replace
