* -------------------------------------------------------------------------------------------------- *
*   Generate Table B4 for 														 					 *
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
*gen person ids
*--------------------------------------------------------------------
gen person_id=.
replace person_id=1   if race=="black" & gender=="male" & education_level=="high"
replace person_id=2   if race=="black" & gender=="male" & education_level=="medium"
replace person_id=3   if race=="black" & gender=="male" & education_level=="low"
replace person_id=4   if race=="black" & gender=="female" & education_level=="high"
replace person_id=5   if race=="black" & gender=="female" & education_level=="medium"
replace person_id=6   if race=="black" & gender=="female" & education_level=="low"
replace person_id=7   if race=="hispanic" & gender=="male" & education_level=="high"
replace person_id=8   if race=="hispanic" & gender=="male" & education_level=="medium"
replace person_id=9   if race=="hispanic" & gender=="male" & education_level=="low"
replace person_id=10  if race=="hispanic" & gender=="female" & education_level=="high"
replace person_id=11  if race=="hispanic" & gender=="female" & education_level=="medium"
replace person_id=12  if race=="hispanic" & gender=="female" & education_level=="low"
replace person_id=13  if race=="white" & gender=="male" & education_level=="high"
replace person_id=14  if race=="white" & gender=="male" & education_level=="medium"
replace person_id=15  if race=="white" & gender=="male" & education_level=="low"
replace person_id=16  if race=="white" & gender=="female" & education_level=="high"
replace person_id=17  if race=="white" & gender=="female" & education_level=="medium"
replace person_id=18  if race=="white" & gender=="female" & education_level=="low"


tab person_id, gen(person_)


*--------------------------------------------------------------------
*means by identity
*--------------------------------------------------------------------

mean choice if person_1==1, vce(cluster address_id)
outreg, ///
noautosumm stats(b ci) dbldiv(,) nostars bdec(4) store(m1) level(90)
mean choice if person_2==1, vce(cluster address_id) 
outreg, ///
noautosumm stats(b ci) dbldiv(,) nostars bdec(4) store(m2) level(90)
outreg, replay(m1) append(m2) store(m) 
mean choice if person_3==1, vce(cluster address_id) level(90)
outreg, ///
noautosumm stats(b ci) dbldiv(,) nostars bdec(4) store(m3) 
outreg, replay(m) append(m3) store(m_m_aa) rtitles("High" \""\ "Medium" \""\ "Low") ctitles("" "Male")

mean choice if person_4==1, vce(cluster address_id) 
outreg, ///
noautosumm stats(b ci) dbldiv(,) nostars bdec(4) store(m1) level(90)
mean choice if person_5==1, vce(cluster address_id) 
outreg, ///
noautosumm stats(b ci) dbldiv(,) nostars bdec(4) store(m2) level(90)
outreg, replay(m1) append(m2) store(m) 
mean choice if person_6==1, vce(cluster address_id)
outreg, ///
noautosumm stats(b ci) dbldiv(,) nostars bdec(4) store(m3) level(90)
outreg, replay(m) append(m3) store(m_f_aa) rtitles("High" \""\ "Medium" \""\ "Low") ctitles("" "Female")

outreg, replay(m_m_aa) merge(m_f_aa) store(m_aa)

mean choice if person_7==1, vce(cluster address_id)
outreg, ///
noautosumm stats(b ci) dbldiv(,) nostars bdec(4) store(m1) level(90)
mean choice if person_8==1, vce(cluster address_id) 
outreg, ///
noautosumm stats(b ci) dbldiv(,) nostars bdec(4) store(m2) level(90)
outreg, replay(m1) append(m2) store(m) 
mean choice if person_9==1, vce(cluster address_id)
outreg, ///
noautosumm stats(b ci) dbldiv(,) nostars bdec(4) store(m3)  level(90)
outreg, replay(m) append(m3) store(m_m_hisp) rtitles("High" \""\ "Medium" \""\ "Low") ctitles("" "Male")

mean choice if person_10==1, vce(cluster address_id)
outreg, ///
noautosumm stats(b ci) dbldiv(,) nostars bdec(4) store(m1) level(90)
mean choice if person_11==1, vce(cluster address_id) 
outreg, ///
noautosumm stats(b ci) dbldiv(,) nostars bdec(4) store(m2) level(90)
outreg, replay(m1) append(m2) store(m) 
mean choice if person_12==1, vce(cluster address_id)
outreg, ///
noautosumm stats(b ci) dbldiv(,) nostars bdec(4) store(m3) level(90)
outreg, replay(m) append(m3) store(m_f_hisp) rtitles("High" \""\ "Medium" \""\ "Low") ctitles("" "Female")

outreg, replay(m_m_hisp) merge(m_f_hisp) store(m_hisp) 

mean choice if person_13==1, vce(cluster address_id)
outreg, ///
noautosumm stats(b ci) dbldiv(,) nostars bdec(4) store(m1) level(90)
mean choice if person_14==1, vce(cluster address_id) 
outreg, ///
noautosumm stats(b ci) dbldiv(,) nostars bdec(4) store(m2) level(90)
outreg, replay(m1) append(m2) store(m) 
mean choice if person_15==1, vce(cluster address_id)
outreg, ///
noautosumm stats(b ci) dbldiv(,) nostars bdec(4) store(m3) level(90)
outreg, replay(m) append(m3) store(m_m_white) rtitles("High" \""\ "Medium" \""\ "Low") ctitles("" "Male")

mean choice if person_16==1, vce(cluster address_id)
outreg, ///
noautosumm stats(b ci) dbldiv(,) nostars bdec(4) store(m1) level(90)
mean choice if person_17==1, vce(cluster address_id) 
outreg, ///
noautosumm stats(b ci) dbldiv(,) nostars bdec(4) store(m2) level(90)
outreg, replay(m1) append(m2) store(m) 
mean choice if person_18==1, vce(cluster address_id) 
outreg, ///
noautosumm stats(b ci) dbldiv(,) nostars bdec(4) store(m3) level(90)
outreg, replay(m) append(m3) store(m_f_white) rtitles("High" \""\ "Medium" \""\ "Low") ctitles("" "Female")

outreg, replay(m_m_white) merge(m_f_white) store(m_white) 

outreg, replay(m_aa) merge(m_hisp) store(m_all)
outreg, replay(m_all) merge(m_white) store(m_all)

*--------------------------------------------------------------------
*tests
*--------------------------------------------------------------------
mat Tests=J(2,6,.)

reg choice person_1 person_2 person_3, vce(cluster address_id)
test person_1=person_2=person_3
mat Tests[1,1]=`r(F)'
mat Tests[2,1]=`r(p)'

reg choice person_4 person_5 person_6, vce(cluster address_id)
test person_4=person_5=person_6
mat Tests[1,2]=`r(F)'
mat Tests[2,2]=`r(p)'

reg choice person_7 person_8 person_9, vce(cluster address_id)
test person_7=person_8=person_9
mat Tests[1,3]=`r(F)'
mat Tests[2,3]=`r(p)'

reg choice person_10 person_11 person_12, vce(cluster address_id)
test person_10=person_11=person_12
mat Tests[1,4]=`r(F)'
mat Tests[2,4]=`r(p)'

reg choice person_13 person_14 person_15, vce(cluster address_id)
test person_13=person_14=person_15
mat Tests[1,5]=`r(F)'
mat Tests[2,5]=`r(p)'

reg choice person_16 person_17 person_18, vce(cluster address_id)
test person_16=person_17=person_18
mat Tests[1,6]=`r(F)'
mat Tests[2,6]=`r(p)'

matrix rownames Tests = "F-stat" "P-value" 
matrix colnames Tests = "African American" " " "Hispanic" " " "White" " "

frmttable, statmat(Tests) store(tests) sdec(4)

*--------------------------------------------------------------------
*overall means
*--------------------------------------------------------------------
mat overall=J(1,6,.)

quietly: summarize choice if race=="black"
mat overall[1,1] = r(mean)

quietly: summarize choice if race=="hispanic"
mat overall[1,3] = r(mean)

quietly: summarize choice if race=="white"
mat overall[1,5] = r(mean)

matrix rownames overall = "Mean" 
frmttable, statmat(overall) store(overall) sdec(4)

outreg, replay(m_all) append(tests) store(all)

outreg using "tables/table_b4.tex", replay(all) append(overall) tex nocenter note("") fragment plain replace
