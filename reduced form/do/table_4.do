* -------------------------------------------------------------------------------------------------- *
*   Generate Table 4 for 														 					 *
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

gen Minority1=(Minority==1 & inquiry_order==1)
gen Minority2=(Minority==1 & inquiry_order==2)
gen Minority3=(Minority==1 & inquiry_order==3)

gen Hispanic1=(Hispanic==1 & inquiry_order==1)
gen Hispanic2=(Hispanic==1 & inquiry_order==2)
gen Hispanic3=(Hispanic==1 & inquiry_order==3)

gen Black1=(Black==1 & inquiry_order==1)
gen Black2=(Black==1 & inquiry_order==2)
gen Black3=(Black==1 & inquiry_order==3)

gen White1=(Minority==0 & inquiry_order==1)
gen White2=(Minority==0 & inquiry_order==2)
gen White3=(Minority==0 & inquiry_order==3)


label variable Hispanic "Hispanic"
label variable Black "African American"



*--------------------------------------------------------------------
*difference in means
*--------------------------------------------------------------------
  qui sum choice if White==1
  loc r_white=r(mean)
  
reg choice  Minority1 Minority2 Minority3 White2 White3, vce(cluster address_id)
outreg, ctitle("Full Sample") ///
summstat(N\N_clust) keep(Minority1 Minority2 Minority3 White2 White3) stats(b ci) dbldiv(,) starlevels(10 5 1)  starloc(1) bdec(4) store(model1_diff) level(90)

nlcom   (Minority1 :_b[Minority1]  /`r_white' +1) ///
		(Minority2 :_b[Minority2]  /`r_white' +1) ///
		(Minority3 :_b[Minority3]  /`r_white' +1) ///
		(White2 :_b[White2]  /`r_white' +1) ///
		(White3 :_b[White3]  /`r_white' +1) , post
	test Minority1=1
	test Minority2=1 
	test Minority3=1
	test White2=1 
	test White3=1
outreg,  stats(b ci) dbldiv(,) ///
noautosumm store(model_diff_r)  starlevels(10 5 1) starloc(1) bdec(4) level(90) 

reg choice  Minority1 Minority2 Minority3 White2 White3  if whiteshare>.6218774, vce(cluster address_id)
outreg, ctitle("White Neighborhoods") ///
summstat(N\N_clust) keep(Minority1 Minority2 Minority3 White2 White3) stats(b ci) dbldiv(,) starlevels(10 5 1)  starloc(1) bdec(4) store(model2_diff) level(90)

  qui sum choice if White==1 & whiteshare>.6218774
  loc r_white=r(mean)
  
nlcom   (Minority1 :_b[Minority1]  /`r_white' +1) ///
		(Minority2 :_b[Minority2]  /`r_white' +1) ///
		(Minority3 :_b[Minority3]  /`r_white' +1) ///
		(White2 :_b[White2]  /`r_white' +1) ///
		(White3 :_b[White3]  /`r_white' +1) , post
	test Minority1=1
	test Minority2=1 
	test Minority3=1
	test White2=1 
	test White3=1
outreg,  stats(b ci) dbldiv(,) ///
noautosumm store(model_diff_r2)  starlevels(10 5 1) starloc(1) bdec(4) level(90) 

reg choice  Minority1 Minority2 Minority3 White2 White3  if whiteshare<=.6218774, vce(cluster address_id)
outreg, ctitle("Minority Neighborhoods") ///
summstat(N\N_clust) keep(Minority1 Minority2 Minority3 White2 White3) stats(b ci) dbldiv(,) starlevels(10 5 1)  starloc(1) bdec(4) store(model3_diff) level(90)

  qui sum choice if White==1 & whiteshare<=.6218774
  loc r_white=r(mean)
  
nlcom   (Minority1 :_b[Minority1]  /`r_white' +1) ///
		(Minority2 :_b[Minority2]  /`r_white' +1) ///
		(Minority3 :_b[Minority3]  /`r_white' +1) ///
		(White2 :_b[White2]  /`r_white' +1) ///
		(White3 :_b[White3]  /`r_white' +1) , post
	test Minority1=1
	test Minority2=1 
	test Minority3=1
	test White2=1 
	test White3=1
outreg,  stats(b ci) dbldiv(,) ///
noautosumm store(model_diff_r3)  starlevels(10 5 1) starloc(1) bdec(4) level(90) 

outreg, replay(model1_diff) merge(model2_diff) store(model_diff)
outreg, replay(model_diff) merge(model3_diff) store(model_diff)

outreg, replay(model_diff_r) merge(model_diff_r2) store(model_diff_r)
outreg, replay(model_diff_r) merge(model_diff_r3) store(model_diff_r)

outreg, replay(model_diff_r) append(model_diff) store(model_diff)

*outreg, replay(model_lpm) append(model_cl) store(model_all)
*outreg, replay(model_all) append(model_diff) store(model_all)

************************************************************************************************
*Summary Statistics
************************************************************************************************
mat sumstat = J(7,3,.)
mat list sumstat
*All cities
    quietly: summarize choice if Minority1==1 
    mat sumstat[1,1] = r(mean)
    quietly: summarize choice if Minority2==1 
    mat sumstat[2,1] = r(mean)
	quietly: summarize choice if Minority3==1 
    mat sumstat[3,1] = r(mean)
	quietly: summarize choice if White2==1 
    mat sumstat[4,1] = r(mean)
	quietly: summarize choice if White3==1 
    mat sumstat[5,1] = r(mean)
	quietly: summarize choice if White1==1 
    mat sumstat[6,1] = r(mean)
	quietly: summarize choice if White==1 
    mat sumstat[7,1] = r(mean)
	quietly: summarize choice if Minority1==1 & whiteshare>.6218774
    mat sumstat[1,2] = r(mean)
    quietly: summarize choice if Minority2==1 & whiteshare>.6218774
    mat sumstat[2,2] = r(mean)
	quietly: summarize choice if Minority3==1 & whiteshare>.6218774
    mat sumstat[3,2] = r(mean)
	quietly: summarize choice if White2==1 & whiteshare>.6218774
    mat sumstat[4,2] = r(mean)
	quietly: summarize choice if White3==1 & whiteshare>.6218774
    mat sumstat[5,2] = r(mean)
	quietly: summarize choice if White1==1 & whiteshare>.6218774
    mat sumstat[6,2] = r(mean)
	quietly: summarize choice if White==1 & whiteshare>.6218774
    mat sumstat[7,2] = r(mean)
	 quietly: summarize choice if Minority1==1 & whiteshare<=.6218774
    mat sumstat[1,3] = r(mean)
    quietly: summarize choice if Minority2==1 & whiteshare<=.6218774
    mat sumstat[2,3] = r(mean)
	quietly: summarize choice if Minority3==1 & whiteshare<=.6218774
    mat sumstat[3,3] = r(mean)
	quietly: summarize choice if White2==1 & whiteshare<=.6218774
    mat sumstat[4,3] = r(mean)
	quietly: summarize choice if White3==1 & whiteshare<=.6218774
    mat sumstat[5,3] = r(mean)
	quietly: summarize choice if White1==1 & whiteshare<=.6218774
    mat sumstat[6,3] = r(mean)
	quietly: summarize choice if White==1 & whiteshare<=.6218774
    mat sumstat[7,3] = r(mean)
mat list sumstat

matrix rownames sumstat = "Minority-1st" "Minority-2nd" "Minority-3rd" "White-2nd" "White-3rd" "White-1st" "White (overall)"

frmttable, statmat(sumstat) store(sumstat) sdec(4)

outreg using "tables/table_4.tex", replay(model_diff) append(sumstat) tex nocenter note("") fragment plain replace
