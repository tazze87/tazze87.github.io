// ------------------------------------------------------------------------------------
// Replication for: Individual Cheating in the Lab: A New Measure and External Validity
// ------------------------------------------------------------------------------------
version 15
clear all
use AA_individual_cheating.dta, clear

*************************************************
***************** SECTION 3.1 *******************
*************************************************

// Figure 1
preserve
proportion list_lie
matrix C = r(table)
matrix list C
matrix C = C[5, 1..4]\C[6, 1..4]
matrix list C
replace list_lie =2 if list_lie ==3
replace list_lie =3 if list_lie ==5
gen lb = C[1,1] if list_lie == 0
gen ub = C[2,1] if list_lie == 0
replace lb = C[1,2] if list_lie == 1
replace ub = C[2,2] if list_lie == 1
replace lb = C[1,3] if list_lie == 2
replace ub = C[2,3] if list_lie == 2
replace lb = C[1,4] if list_lie == 3
replace ub = C[2,4] if list_lie == 3
hist list_lie, d bcolor(black*0.4) discrete barw(0.8) xlabel(none) xti("") ///
addlabels addlabopts(mlabp(2) yvarformat(%3.2f))
graph addplot rcap ub lb list_lie, ysc(r(0 .5)) ylab(0(.1).5, nogrid) ///
msize(small) lw(vthin) lc(black) ///
yti("Fraction of choices", height(5)) xlabel(0 "£0 (Honest)" 1 "£1" 2 "£3" 3 "£5") xti("List selected",  height(6)) ///
xsc(r(-0.5 3.9)) ///
graphregion(color(white)) bgcolor(white)
restore

// Result 1
preserve // 1 vs 3
drop if list_lie != 1 & list_lie != 3
bitest list3 = 0.5
restore
preserve // 3 vs 5
drop if list_lie != 3 & list_lie != 5
bitest list5 = 0.5
restore
preserve // 1 vs 5
drop if list_lie != 1 & list_lie != 5
bitest list5 = 0.5
restore

// Figure 2
hist belief_list, d w(1) xlab(0(5)100, labs(small)) ylab(0(.05).2, labs(small)) xti("Belief", height(6)) ///
color(black*0.4) ///
yti("Fraction of answers", height(5)) name(belief_all, replace) ///
graphregion(color(white)) bgcolor(white) xli(5,lp(dash)lc(gs2) lw(thin))

// Figure 3
preserve
drop if session == 8 //drop if session == 8 Because the trust game crashed...the list game was played before trust in that session but not the dice game
tab yes liedl, exact
recode liedl (0 = 0 "Honest") (1 = 1 "Lied"), gen(list_dummy)
hist yes, d by(list_dummy, graphregion(color(white)) bgcolor(white) sub("List game")) ylab(0(.1).6, labs(small))  ///
color(black*0.4) barw(0.8) xlabel(-1 " " 0 "No" 1 "Yes" 2 " ") xti("Die-roll game") ///
yti("Fraction of answers", height(5))
restore

*************************************************
***************** SECTION 3.2 *******************
*************************************************

// Result 2 and Figure 4
preserve
drop if treatment == 1
drop if session == 8  & (selectpay1 == 2 | selectpay2 == 2) // Drop who got paid for trust game
drop if field == . | field == -1 | forgot == 1  // exluding who did not participate and people who forgot their payment
tab liedl liedf, exact
spearman list_lie lienorm
replace list_lie = 2 if list_lie == 3
replace list_lie = 3 if list_lie == 5
keep lienorm list_lie id 
egen weight = count(id), by (list_lie lienorm)
sort list_lie weight  lienorm
by list_lie weight lienorm: gen new = 1 if _n==1
drop if new != 1
expand 4
replace lienorm = . if _n>(_N/4)
recode list_lie (0=1) (1=0) (2=3) (3=2) if lienorm==.
scatter lienorm list_lie if list_lie == 0 [w = weight], msymbol(o) mc(black*.4) mlwidth(medium) || ////
scatter lienorm list_lie if list_lie == 1 [w = weight], msymbol(o) mc(black*.4) mlwidth(medium) || ////
scatter lienorm list_lie if list_lie == 2 [w = weight], msymbol(o) mc(black*.4) mlwidth(medium) || ////
scatter lienorm list_lie if list_lie == 3 [w = weight], msymbol(o) mc(black*.4) mlwidth(medium) , ///
ysc(r(-0.05 1)) ylab(0(.1)1, labs(small) nogrid) xsc(r(-.5 3.55)) xlab(0 "0£ (Honest)" 1 "1£" 2 "3£" 3 "5£")  ///
xli(0 1 2 3, lp(dash)lc(gs1*.2) lw(vthin)) ///
yli(0 0.1 0.2 0.3 0.4 0.5 0.6 .7 0.8 .9 1, lp(dash)lc(gs1*.2) lw(vthin)) ///
legend(off) graphregion(color(white)) bgcolor(white) ///
yti("Cheat field", height(7)) xti("Choices in the list game", height(7)) sub("NoFtF") name(fig4left, replace)
restore
preserve
drop if treatment == 2 
drop if field == . | field == -1 | forgot == 1  // exluding who did not participate and people who forgot their payment
tab liedl liedf, exact
spearman list_lie lienorm
replace list_lie = 2 if list_lie == 3
replace list_lie = 3 if list_lie == 5
keep lienorm list_lie id 
egen weight = count(id), by (list_lie lienorm)
sort list_lie weight  lienorm
by list_lie weight lienorm: gen new = 1 if _n==1
drop if new != 1
expand 4
replace lienorm = . if _n>(_N/4)
recode list_lie (0=1) (1=0) (2=3) (3=2) if lienorm==.
scatter lienorm list_lie if list_lie == 0 [w = weight], msymbol(o) mc(black*.4) mlwidth(medium) || ////
scatter lienorm list_lie if list_lie == 1 [w = weight], msymbol(o) mc(black*.4) mlwidth(medium) || ////
scatter lienorm list_lie if list_lie == 2 [w = weight], msymbol(o) mc(black*.4) mlwidth(medium) || ////
scatter lienorm list_lie if list_lie == 3 [w = weight], msymbol(o) mc(black*.4) mlwidth(medium) , ///
ysc(r(-0.05 1)) ylab(0(.1)1, labs(small) nogrid) xsc(r(-.5 3.55)) xlab(0 "0£ (Honest)" 1 "1£" 2 "3£" 3 "5£")  ///
xli(0 1 2 3, lp(dash)lc(gs1*.2) lw(vthin)) ///
yli(0 0.1 0.2 0.3 0.4 0.5 0.6 .7 0.8 .9 1, lp(dash)lc(gs1*.2) lw(vthin)) ///
legend(off) graphregion(color(white)) bgcolor(white) ///
yti("Cheat field", height(7)) xti("Choices in the list game", height(7)) sub("FtF") name(fig4right, replace)
restore

// Table 1
preserve
drop if session == 8 // drop session where trust crushed
drop if field == . | field == -1 | forgot == 1 // exluding who did not participate and people who forgot their payment
eststo clear
quietly reg lienorm list1 list3 list5 risk split sent noftf, r
estadd scalar Obs = e(N)
eststo reg1
quietly reg lienorm list1 list3 list5 risk split sent noftf moneyearned, r
estadd scalar Obs = e(N)
eststo reg2
quietly reg lienorm list1 list3 list5 risk split sent noftf moneyearned female, r
estadd scalar Obs = e(N)
eststo reg3
quietly probit liedf liedl risk split sent noftf, r
quietly margins, dydx(*) post
estadd scalar Obs = e(N)
eststo pro1
quietly probit liedf liedl risk split sent noftf moneyearned, r
quietly margins, dydx(*) post
estadd scalar Obs = e(N)
eststo pro2
quietly probit liedf liedl risk split sent noftf moneyearned female, r
quietly margins, dydx(*) post
estadd scalar Obs = e(N)
eststo pro3
esttab reg1 reg2 reg3 pro1 pro2 pro3, star(* 0.1 ** 0.05 *** 0.01) booktabs ///
stats(Obs) cells(b(fmt(3)star) se(fmt(3)par)) ///
mlabels("OLS" "OLS" "OLS" "dy/dx" "dy/dx" "dy/dx") nogaps compress onecell title(Cheating and other individial attitudes \label{tab1})
restore

// Result 3 and Figure 5
preserve
drop if treatment == 1 
drop if session == 8  & (selectpay1 == 2 | selectpay2 == 2) // Drop who got paid for trust game
drop if field == . | field == -1 | forgot == 1  // exluding who did not participate and people who forgot their payment
tab yes liedf, exact
spearman yes lienorm
keep lienorm yes id 
egen weight = count(id), by (yes lienorm)
sort yes weight  lienorm
by yes weight lienorm: gen new = 1 if _n==1
drop if new != 1
expand 2
replace lienorm = . if _n>(_N/2)
recode yes(0=1) (1=0) if lienorm==.
scatter lienorm yes if yes == 0 [w = weight], msymbol(o) mc(black*.4) mlwidth(medium) || ////
scatter lienorm yes if yes == 1 [w = weight], msymbol(o) mc(black*.4) mlwidth(medium) , ///
ysc(r(-0.05 1)) ylab(0(.1)1, labs(small) nogrid) xsc(r(-.5 1.5)) xlab(0 "No" 1 "Yes")  ///
xli(0 1, lp(dash)lc(gs1*.2) lw(vthin)) ///
yli(0 0.1 0.2 0.3 0.4 0.5 0.6 .7 0.8 .9 1, lp(dash)lc(gs1*.2) lw(vthin)) ///
legend(off) graphregion(color(white)) bgcolor(white) ///
yti("Cheat field", height(7)) xti("Answers in die-roll game", height(7)) sub("NoFtF") name(fig5left, replace)
restore
preserve
drop if treatment == 2 
drop if field == . | field == -1 | forgot == 1  // exluding who did not participate and people who forgot their payment
tab yes liedf, exact
spearman yes lienorm
keep lienorm yes id 
egen weight = count(id), by (yes lienorm)
sort yes weight  lienorm
by yes weight lienorm: gen new = 1 if _n==1
drop if new != 1
expand 2
replace lienorm = . if _n>(_N/2)
recode yes(0=1) (1=0) if lienorm==.
scatter lienorm yes if yes == 0 [w = weight], msymbol(o) mc(black*.4) mlwidth(medium) || ////
scatter lienorm yes if yes == 1 [w = weight], msymbol(o) mc(black*.4) mlwidth(medium) , ///
ysc(r(-0.05 1)) ylab(0(.1)1, labs(small) nogrid) xsc(r(-.5 1.5)) xlab(0 "No" 1 "Yes")  ///
xli(0 1, lp(dash)lc(gs1*.2) lw(vthin)) ///
yli(0 0.1 0.2 0.3 0.4 0.5 0.6 .7 0.8 .9 1, lp(dash)lc(gs1*.2) lw(vthin)) ///
legend(off) graphregion(color(white)) bgcolor(white) ///
yti("Cheat field", height(7)) xti("Answers in die-roll game", height(7)) sub("FtF") name(fig5right, replace)
restore

// Table 2
preserve
drop if session == 8 // drop session where trust crushed
drop if field == . | field == -1 | forgot == 1 // exluding who did not participate and people who forgot their payment
eststo clear
quietly reg lienorm yes risk split sent noftf, r
estadd scalar Obs = e(N)
eststo reg1
quietly reg lienorm yes risk split sent noftf moneyearned, r
estadd scalar Obs = e(N)
eststo reg2
quietly reg lienorm yes risk split sent noftf moneyearned female, r
estadd scalar Obs = e(N)
eststo reg3
quietly probit liedf yes risk split sent noftf, r
quietly margins, dydx(*) post
estadd scalar Obs = e(N)
eststo pro1
quietly probit liedf yes risk split sent noftf moneyearned, r
quietly margins, dydx(*) post
estadd scalar Obs = e(N)
eststo pro2
quietly probit liedf yes risk split sent noftf moneyearned female, r
quietly margins, dydx(*) post
estadd scalar Obs = e(N)
eststo pro3
esttab reg1 reg2 reg3 pro1 pro2 pro3, star(* 0.1 ** 0.05 *** 0.01) booktabs ///
stats(Obs) cells(b(fmt(3)star) se(fmt(3)par)) ///
mlabels("OLS" "OLS" "OLS" "dy/dx" "dy/dx" "dy/dx") nogaps compress onecell title(Cheating and other individial attitudes \label{tab1})
restore

*************************************************
****************** APPENDIX *********************
*************************************************

//Table B.1
preserve
drop if session == 8 // drop session where trust crushed
eststo clear
quietly reg list_lie yes risk split sent die_before, r
estadd scalar Obs = e(N)
eststo reg1
quietly reg list_lie yes risk split sent die_before noftf, r
estadd scalar Obs = e(N)
eststo reg2
quietly reg list_lie yes risk split sent die_before noftf female, r
estadd scalar Obs = e(N)
eststo reg3
quietly probit liedl yes risk split sent die_before, r
quietly margins, dydx(*) post
estadd scalar Obs = e(N)
eststo pro1
quietly probit liedl yes risk split sent die_before noftf, r
quietly margins, dydx(*) post
estadd scalar Obs = e(N)
eststo pro2
quietly probit liedl yes risk split sent die_before noftf female, r
quietly margins, dydx(*) post
estadd scalar Obs = e(N)
eststo pro3
esttab reg1 reg2 reg3 pro1 pro2 pro3, star(* 0.1 ** 0.05 *** 0.01) booktabs ///
stats(Obs) cells(b(fmt(3)star) se(fmt(3)par)) ///
mlabels("OLS" "OLS" "OLS" "dy/dx" "dy/dx" "dy/dx") nogaps compress onecell title(Cheating and other individial attitudes \label{tab1})
restore

//Table C.2
preserve
drop if session == 8 // drop session where trust crushed
eststo clear
quietly reg list_lie yes risk split back die_before, r
estadd scalar Obs = e(N)
eststo reg1
quietly reg list_lie yes risk split back die_before noftf, r
estadd scalar Obs = e(N)
eststo reg2
quietly reg list_lie yes risk split back die_before noftf female, r
estadd scalar Obs = e(N)
eststo reg3
quietly probit liedl yes risk split back die_before, r
quietly margins, dydx(*) post
estadd scalar Obs = e(N)
eststo pro1
quietly probit liedl yes risk split back die_before noftf, r
quietly margins, dydx(*) post
estadd scalar Obs = e(N)
eststo pro2
quietly probit liedl yes risk split back die_before noftf female, r
quietly margins, dydx(*) post
estadd scalar Obs = e(N)
eststo pro3
esttab reg1 reg2 reg3 pro1 pro2 pro3, replace star(* 0.1 ** 0.05 *** 0.01) booktabs ///
stats(Obs) cells(b(fmt(3)star) se(fmt(3)par)) ///
mlabels("OLS" "OLS" "OLS" "dy/dx" "dy/dx" "dy/dx") nogaps compress onecell title(Cheating and other individial attitudes \label{tab1})
restore

//Table C.3
preserve
tab where, gen(dwhere)
rename dwhere1 Africa
rename dwhere2 Asia
rename dwhere3 S_America
rename dwhere4 Europe
rename dwhere5 N_America
tab field_of_study, gen(dstudy)
rename dstudy1 Biology
rename dstudy2 Computer_Sc
rename dstudy3 Economics
rename dstudy4 Gov
rename dstudy5 Linguistics
rename dstudy6 Other
rename dstudy7 Psy
rename dstudy8 Socio
eststo clear
quietly reg list_lie age female religious student, r
estadd scalar Obs = e(N)
eststo reg1
quietly reg list_lie age female religious student Africa Asia N_America S_America, r
estadd scalar Obs = e(N)
eststo reg2
quietly reg list_lie age female religious student Biology Computer_Sc Economics Gov Linguistics Psy Socio, r
estadd scalar Obs = e(N)
eststo reg3
quietly reg list_lie age female religious student Africa Asia N_America S_America Biology Computer_Sc Economics Gov Linguistics Psy Socio, r
estadd scalar Obs = e(N)
eststo reg4
quietly probit liedl age female religious student , r
quietly margins, dydx(*) post
estadd scalar Obs = e(N)
eststo pro1
quietly probit liedl age female religious student Africa Asia N_America S_America, r
quietly margins, dydx(*) post
estadd scalar Obs = e(N)
eststo pro2
quietly probit liedl age female religious student Biology Computer_Sc Economics Gov Linguistics Psy Socio, r
quietly margins, dydx(*) post
estadd scalar Obs = e(N)
eststo pro3
quietly probit liedl age female religious student Africa Asia N_America S_America Biology Computer_Sc Economics Gov Linguistics Psy Socio, r
quietly margins, dydx(*) post
estadd scalar Obs = e(N)
eststo pro4
esttab reg1 reg2 reg3 reg4 pro1 pro2 pro3 pro4, star(* 0.1 ** 0.05 *** 0.01) booktabs ///
stats(Obs) cells(b(fmt(3)star) se(fmt(3)par)) ///
mlabels("OLS" "OLS" "OLS" "OLS" "dy/dx" "dy/dx" "dy/dx" "dy/dx") nogaps compress onecell title(Cheating and individual demographics \label{tab1})
restore

//Table C.4
preserve
eststo clear
quietly reg list_lie agree consc extra neuro open, r
estadd scalar Obs = e(N)
eststo reg1
quietly reg list_lie agree consc extra neuro open female, r
estadd scalar Obs = e(N)
eststo reg2
quietly probit liedl agree consc extra neuro open, r
quietly margins, dydx(*) post
estadd scalar Obs = e(N)
eststo pro1
quietly probit liedl agree consc extra neuro open female, r
quietly margins, dydx(*) post
estadd scalar Obs = e(N)
eststo pro2
esttab reg1 reg2 pro1 pro2, star(* 0.1 ** 0.05 *** 0.01) booktabs ///
stats(Obs) cells(b(fmt(3)star) se(fmt(3)par)) ///
mlabels("OLS" "OLS" "OLS" "dy/dx" "dy/dx" "dy/dx") nogaps compress onecell title(Cheating and personal traits \label{tab1})
restore

//Table C.5
preserve
drop if session == 8 // drop session where trust crushed
drop if field == . | field == -1 | forgot == 1  // exluding who did not participate and people who forgot their payment
eststo clear
quietly reg field_lie list1 list3 list5 risk split sent noftf, r
estadd scalar Obs = e(N)
eststo reg1
quietly reg field_lie list1 list3 list5 risk split sent noftf moneyearned, r
estadd scalar Obs = e(N)
eststo reg2
quietly reg field_lie list1 list3 list5 risk split sent noftf moneyearned female, r
estadd scalar Obs = e(N)
eststo reg3
quietly probit liedf liedl risk split sent noftf, r
quietly margins, dydx(*) post
estadd scalar Obs = e(N)
eststo pro1
quietly probit liedf liedl risk split sent noftf moneyearned, r
quietly margins, dydx(*) post
estadd scalar Obs = e(N)
eststo pro2
quietly probit liedf liedl risk split sent noftf moneyearned female, r
quietly margins, dydx(*) post
estadd scalar Obs = e(N)
eststo pro3
esttab reg1 reg2 reg3 pro1 pro2 pro3, star(* 0.1 ** 0.05 *** 0.01) booktabs ///
stats(Obs) cells(b(fmt(3)star) se(fmt(3)par)) ///
mlabels("OLS" "OLS" "OLS" "dy/dx" "dy/dx" "dy/dx") nogaps compress onecell title(Cheating and other individial attitudes \label{tab1})
restore

//Table C.6
eststo clear
estpost su age if treatment == 1
estpost su age if treatment == 2	
estpost su female if treatment == 1
estpost su female if treatment == 2	
estpost su religious if treatment == 1
estpost su religious if treatment == 2
estpost tab degree if treatment == 1
estpost tab degree if treatment == 2
estpost sum agree if treatment == 1
estpost sum agree if treatment == 2
estpost sum consc if treatment == 1
estpost sum consc if treatment == 2	
estpost sum extra if treatment == 1
estpost sum extra if treatment == 2
estpost sum neur if treatment == 1
estpost sum neur if treatment == 2
estpost sum open if treatment == 1
estpost sum open if treatment == 2
