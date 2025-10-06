version 18
clear all
use choices.dta


******************************************************************
***************** REGRESSIONS THEORETICAL BENCHMARKS *************
******************************************************************

** SENDER 1 **
* --- MONO ---
preserve
drop if type != "A"
drop if treatment  == "Competition"
gen pos_report = (report_a > 0)
gen black = (state == "black")
mixed pos_report black || session: || id: 
* MIE
estadd scalar somma = _b[black] + _b[_cons]
test _b[black] + _b[_cons] = 1
* LIE
test _b[_cons] = 0.95
test _b[black] + _b[_cons] = 1
reg pos_report black, vce(cluster id)
estadd scalar ols = _b[black] + _b[_cons]
restore

* --- COMP ---
preserve
drop if type != "A"
drop if treatment == "Monopoly"
gen pos_report = (report_a > 0)
gen black = (state == "black")
mixed pos_report black || session: || id:
* MIE
estadd scalar somma = _b[black] + _b[_cons]
test _b[black] + _b[_cons] = 1
* AE
test _b[_cons] = 0.6
test _b[black] + _b[_cons] = 1
reg pos_report black , vce(cluster id)
estadd scalar ols = _b[black] + _b[_cons]
restore


** SENDER 2 **
* --- COMP ---
preserve
drop if type != "B"
drop if treatment == "Monopoly"
gen neg_report = (report_b < 0)
gen red = (state == "red")
mixed neg_report red || session: || id:
* MIE
estadd scalar somma = _b[red] + _b[_cons]
test _b[red] + _b[_cons] = 1
* AE
test _b[_cons] = 0.6
test _b[red] + _b[_cons] = 1
reg neg_report red , vce(cluster id)
estadd scalar ols = _b[red] + _b[_cons]
restore



** DM **
* --- MONO ---
preserve
drop if type != "C"
drop if treatment == "Competition"
gen black = (state == "black")
gen chose_black = (choice == "black")
gen chose_red = (choice == "red")
* MIE
mixed chose_black black || session: || id: 
estadd scalar somma = _b[black] + _b[_cons]
test _b[black] + _b[_cons] = 1
reg chose_black black, vce(cluster id)
estadd scalar ols = _b[black] + _b[_cons]

mixed chose_red black || session: || id: 
test _b[_cons] = 1
* LIE
test _b[_cons] = 0.05
reg chose_red black, vce(cluster id)
restore

* --- COMP ---
preserve
drop if type != "C"
drop if treatment == "Monopoly"
gen black = (state == "black")
gen chose_black = (choice == "black")
gen chose_red = (choice == "red")
* MIE
mixed chose_black black || session: || id: 
estadd scalar somma = _b[black] + _b[_cons]
test _b[black] + _b[_cons] = 1
reg chose_black black, vce(cluster id)
estadd scalar ols = _b[black] + _b[_cons]

mixed chose_red black || session: || id:
test _b[_cons] = 1
reg chose_red black, vce(cluster id)
* AE
mixed chose_black black || session: || id: 
estadd scalar somma = _b[black] + _b[_cons]
test _b[black] + _b[_cons] = 0.62
reg chose_black black, vce(cluster id)
estadd scalar ols = _b[black] + _b[_cons]

mixed chose_red black || session: || id:
test _b[_cons] = 0.62
reg chose_red black, vce(cluster id)
restore


** DM THRESHOLDS **
* --- MONO ---
preserve
drop if type != "C"
drop if treatment == "Competition"
gen black = (state == "black")
gen chose_black = (choice == "black")
** MIE - threshold = 96
gen less_than96 = (report_a < 96)
tab less_than96 chose_black
mixed chose_black less_than96 || session: || id:
estadd scalar somma0 = _b[less_than96] + _b[_cons]
test _b[less_than96] + _b[_cons] = 0
reg chose_black less_than96, vce(cluster id)
estadd scalar ols0 = _b[less_than96] + _b[_cons]

** LIE - threshold = 48
gen more_than48 = (report_a >= 48)
tab more_than48 chose_black
* lambda
mixed chose_black more_than48 || session: || id:
estadd scalar somma1 = _b[more_than48] + _b[_cons]
test _b[more_than48] + _b[_cons] = 1
reg chose_black more_than48, vce(cluster id)
estadd scalar ols1 = _b[more_than48] + _b[_cons]
* phi
gen less_than48 = (report_a < 48)
tab less_than48 chose_black
mixed chose_black less_than48 || session: || id:
estadd scalar somma2 = _b[less_than48] + _b[_cons]
test _b[less_than48] + _b[_cons] = 0
reg chose_black less_than48, vce(cluster id)
estadd scalar ols2 = _b[less_than48] + _b[_cons]
restore

* --- COMP ---
preserve
drop if type != "C"
drop if treatment == "Monopoly"
gen black = (state == "black")
gen chose_black = (choice == "black")
** MIE
gen sone_more_than96 = (report_a >= 96)
gen sone_less_than96 = (report_a < 96)
* lambda first eq
tab chose_black sone_more_than96
mixed chose_black sone_more_than96 || session: || id:
estadd scalar somma1 = _b[sone_more_than96] + _b[_cons]
test _b[sone_more_than96] + _b[_cons] = 1
reg chose_black sone_more_than96, vce(cluster id)
estadd scalar ols1 = _b[sone_more_than96] + _b[_cons]
* phi first eq
mixed chose_black sone_less_than96 || session: || id:
estadd scalar somma2 = _b[sone_less_than96] + _b[_cons]
test _b[sone_less_than96] + _b[_cons] = 0
reg chose_black sone_less_than96, vce(cluster id)
estadd scalar ols2 = _b[sone_less_than96] + _b[_cons]
* lambda second eq
gen stwomore_than96 = (report_b > -96)
tab chose_black stwomore_than96
mixed chose_black stwomore_than96 || session: || id:
estadd scalar somma3 = _b[stwomore_than96] + _b[_cons]
test _b[stwomore_than96] + _b[_cons] = 1
reg chose_black stwomore_than96, vce(cluster id)
estadd scalar ols3 = _b[stwomore_than96] + _b[_cons]
* phi second eq
gen stwoless_than96 = (report_b <= -96)
tab chose_black stwoless_than96
mixed chose_black stwoless_than96 || session: || id:
estadd scalar somma4 = _b[stwoless_than96] + _b[_cons]
test _b[stwoless_than96] + _b[_cons] = 0
reg chose_black stwoless_than96, vce(cluster id)
estadd scalar ols4 = _b[stwoless_than96] + _b[_cons]
restore

preserve
** AE
drop if type != "C"
drop if treatment == "Monopoly"
gen black = (state == "black")
gen chose_black = (choice == "black")
gen avg_report = (report_a + report_b)/2
gen avg_more_than_zero = (avg_report >= 0)
gen avg_less_than_zero = (avg_report < 0)
tab chose_black avg_more_than_zero
* lambda
mixed chose_black avg_more_than_zero || session: || id:
estadd scalar somma = _b[avg_more_than_zero] + _b[_cons]
test _b[avg_more_than_zero] + _b[_cons] = 1
reg chose_black avg_more_than_zero, vce(cluster id)
estadd scalar ols = _b[avg_more_than_zero] + _b[_cons]
* phi
mixed chose_black avg_less_than_zero || session: || id:
estadd scalar somma2 = _b[avg_less_than_zero] + _b[_cons]
test _b[avg_less_than_zero] + _b[_cons] = 0
reg chose_black avg_less_than_zero, vce(cluster id)
estadd scalar ols2 = _b[avg_less_than_zero] + _b[_cons]
restore





******************************************************************
***************** FIGURE 3 *************
******************************************************************

*MONOPOLY
preserve
drop if type != "C"
drop if treatment == "Competition"
gen Black = (choice == "black")
bysort report_a: egen avg_black = mean(Black)
bysort report_a: egen wpop = count(Black == 1)
scatter avg_black report_a [w=wpop], msymbol(circle) mcol(black*.5%50) mfc(black*.4%20) msiz(small) mlwidth(vvvthin) xsc(r(-90 90)) xlab(-90(10)90, labs(medium) nogrid) ///
ysc(r(0 1)) ylab(0(.1)1, labs(medium) nogrid) xli(0, lp(dash)lc(gs9) lw(vthin)) ///
yti("") xti(Report) name(avg_black_mono, replace)
graph2tex, epsfile(avg_black_mono)
restore


* COMPETITION
preserve
drop if type != "C"
drop if treatment == "Monopoly"
gen average_report = (report_a + report_b)/2
gen Black = (choice == "black")
gen conflict = 0
replace conflict = 1 if report_a < 0 & report_b >= 0
replace conflict = 1 if report_a <= 0 & report_b > 0
replace conflict = 1 if report_a > 0 & report_b <= 0
replace conflict = 1 if report_a >= 0 & report_b < 0
bysort conflict average_report: egen avg_black = mean(Black)
bysort conflict average_report: egen wpop = count(Black == 1)
twoway (scatter avg_black average_report [w=wpop] if conflict == 0, msymbol(triangle) mcol(black*.9%50) msiz(small) mlwidth(vvthin)) ///
(scatter avg_black average_report [w=wpop] if conflict == 1, msymbol(circle) mcol(black*.5%50) mfc(black*.4%20) msiz(small) mlwidth(vvvthin)) ///
(scatter avg_black average_report  if avg_black == ., msymbol(triangle) mcol(black*.9%50) msiz(large) mlwidth(vvthin)) ///
(scatter avg_black average_report  if avg_black == ., msymbol(circle) mcol(black*.5%50) mfc(black*.4%20) msiz(large) mlwidth(vvvthin)), xsc(r(-90 90)) xlab(-90(10)90, labs(medium) nogrid) ///
ysc(r(0 1)) ylab(0(.1)1, labs(medium) nogrid) xli(0, lp(dash)lc(gs9) lw(vthin)) ///
yti("") xti(Average report) legend(order(3 "Same sign" 4 "Different signs") ring(0) pos(4)) name(avg_black_comp, replace)
gen correct = (wrong_c == 0)
tab correct conflict
graph2tex, epsfile(avg_black_comp)
restore


******************************************************************
*************************** TEST ON REACH ************************
******************************************************************

* MONOPOLY
preserve
drop if type != "A"
drop if treatment == "Competition"
gen up  = drawn + 96
gen down = drawn - 96
gen in_reach = (report_a <= up & report_a >= down)
tab in_reach
restore

* COMPETITION
preserve
drop if type != "A"
drop if treatment == "Monopoly"
gen up  = drawn + 96
gen down = drawn - 96
gen in_reach_a = (report_a <= up & report_a >= down)
tab in_reach_a

gen in_reach_b = (report_b <= up & report_b >= down)
tab in_reach_b
restore





















