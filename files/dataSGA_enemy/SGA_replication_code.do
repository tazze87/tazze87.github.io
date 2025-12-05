version 19
clear all
use SGA_replication_dataset.dta, clear


*********************************************************************
// Figure 1
preserve
gen coop_percent_first = coop_1 * 100
gen coop_percent_all = cooperate * 100
egen avgcoop_first = mean(coop_percent_first), by(treatment supergame)
egen avgcoop_all = mean(coop_percent_all), by(treatment supergame)
twoway (scatter avgcoop_first supergame if treatment==0, sort c(l) msymbol(o) mc(gs10) msiz(small) lcol(gs10%70) ) ///
	(scatter avgcoop_first supergame if treatment==1, sort c(l) msymbol(diamond) mc(gs3) msiz(small) lcol(gs3%70) ), ///
	legend(order(2 "Tournament" 1 "Control") ring(0) pos(1)) yti("") xti("") ti("First rounds") xlab(1 5 10 15 20 25 30) ysc(r(0 100)) ylab(0(10)100, labs(small)) ///
	 name(first_rounds_coop_trend, replace)
twoway (scatter avgcoop_all supergame if treatment==0, sort c(l) msymbol(o) mc(gs10) msiz(small) lcol(gs10%70) ) ///
	(scatter avgcoop_all supergame if treatment==1, sort c(l) msymbol(diamond) mc(gs3) msiz(small) lcol(gs3%70) ), ///
	legend(order(2 "Tournament" 1 "Control") ring(0) pos(1)) yti("") xti("") ti("All rounds") xlab(1 5 10 15 20 25 30) ysc(r(0 100)) ylab(0(10)100, labs(small)) ///
	 name(all_rounds_coop_trend, replace)	 
graph combine first_rounds_coop_trend all_rounds_coop_trend, cols(2) ycommon name(cooperation, replace) ///
	  b1title("Supergame") l1title("Percentage of cooperation")
graph export cooperation.pdf, replace
restore
// Test for Figure 1
* first rounds
bysort treatment: sum coop_1
meprobit coop_1 i.treatment || id :, vce(cluster id)
meprobit coop_1 i.treatment##c.supergame || id:, vce(cluster id)
lincom c.supergame + 1.treatment#c.supergame
* all rounds
bysort treatment: sum cooperate
meprobit cooperate i.treatment || id:, vce(cluster id)
meprobit cooperate i.treatment##c.supergame || id:, vce(cluster id)
*********************************************************************





*********************************************************************
// Table 2
sort id supergame interaction
bysort id supergame: gen int n_in_sg = _N
gen first_in_sg = (period == 1)
by id (supergame interaction): gen long cum_inter = sum(n_in_sg * first_in_sg)
by id: egen total_inter = max(interaction)
gen quartile_interaction = ceil(4 * cum_inter / total_inter)
bysort treatment: sum coop_1 cooperate
bysort treatment quartile_interaction: sum coop_1 cooperate
quietly meprobit coop_1 i.treatment || id :, vce(cluster id)
margins, dydx(_all) at(treatment=1) predict(pr fixedonly)
quietly meprobit cooperate i.treatment || id :, vce(cluster id)
margins, dydx(_all) at(treatment=1) predict(pr fixedonly)
quietly meprobit coop_1 i.treatment##i.quartile_interaction || id:, vce(cluster id)
margins, dydx(treatment) at(quartile_interaction=(1 2 3 4)) predict(pr fixedonly)
quietly meprobit cooperate i.treatment##i.quartile_interaction || id:, vce(cluster id)
margins, dydx(treatment) at(quartile_interaction=(1 2 3 4)) predict(pr fixedonly)
*********************************************************************



***********************************************************************
*** Figure 2
bysort treatment: sum both_coop_1 one_coop_1 both_defect_1
bysort treatment: sum both_coop one_coop both_defect
preserve
label define trt 0 "Control" 1 "Tournament"
label values treatment trt
collapse (mean) both_coop_1 one_coop_1 both_defect_1, by(treatment)
rename (both_coop_1 one_coop_1 both_defect_1) (coop1 coop2 coop3)
reshape long coop, i(treatment) j(outcome)
label define outcome_lab 1 "C,C" 2 "C,D;D,C" 3 "D,D"
label values outcome outcome_lab
graph bar coop, over(outcome, gap(0) label(angle(45))) by(treatment, cols(2) legend(off) title("First rounds") note("")) ///
    bar(1, fcolor(gs12) lcolor(black)) blabel(bar, position(outside) format(%4.2f)) ysc(r(0 0.6)) ylab(0(.1).6, labs(small)) ytitle("") name(outcomes_first_round,replace)
restore
preserve
label define trt 0 "Control" 1 "Tournament"
label values treatment trt
collapse (mean) both_coop one_coop both_defect, by(treatment)
rename (both_coop one_coop both_defect) (coop1 coop2 coop3)
reshape long coop, i(treatment) j(outcome)
label define outcome_lab 1 "C,C" 2 "C,D;D,C" 3 "D,D"
label values outcome outcome_lab
graph bar coop, over(outcome, gap(0) label(angle(45))) by(treatment, cols(2) legend(off) title("All rounds") note("")) ///
    bar(1, fcolor(gs12) lcolor(black)) blabel(bar, position(outside) format(%4.2f)) ysc(r(0 0.6)) ylab(0(.1).6, labs(small)) ytitle("") name(outcomes_all_rounds,replace)
restore
* last supergames
preserve
drop if quartile_interaction != 4
label define trt 0 "Control" 1 "Tournament"
label values treatment trt
collapse (mean) both_coop_1 one_coop_1 both_defect_1, by(treatment)
rename (both_coop_1 one_coop_1 both_defect_1) (coop1 coop2 coop3)
reshape long coop, i(treatment) j(outcome)
label define outcome_lab 1 "C,C" 2 "C,D;D,C" 3 "D,D"
label values outcome outcome_lab
graph bar coop, over(outcome, gap(0) label(angle(45))) by(treatment, cols(2) legend(off) subtitle("Last quartile") note("")) ///
    bar(1, fcolor(gs12) lcolor(black)) blabel(bar, position(outside) format(%4.2f)) ysc(r(0 0.6)) ylab(0(.1).6, labs(small)) ytitle("") name(outcomes_first_round_Q4,replace)
restore
preserve
drop if quartile_interaction != 4
label define trt 0 "Control" 1 "Tournament"
label values treatment trt
collapse (mean) both_coop one_coop both_defect, by(treatment)
rename (both_coop one_coop both_defect) (coop1 coop2 coop3)
reshape long coop, i(treatment) j(outcome)
label define outcome_lab 1 "C,C" 2 "C,D;D,C" 3 "D,D"
label values outcome outcome_lab
graph bar coop, over(outcome, gap(0) label(angle(45))) by(treatment, cols(2) legend(off) subtitle("Last quartile") note("")) ///
    bar(1, fcolor(gs12) lcolor(black)) blabel(bar, position(outside) format(%4.2f)) ysc(r(0 0.6)) ylab(0(.1).6, labs(small)) ytitle("") name(outcomes_all_rounds_Q4,replace)
restore
graph combine outcomes_first_round outcomes_all_rounds outcomes_first_round_Q4 outcomes_all_rounds_Q4, cols(2) ycommon name(outcomes, replace) ///
	  b1title("") l1title("Fraction") imargin(0 0 0 0)  
*tests	  
preserve  
gen id_low  = cond(subject < partner, subject, partner)
gen id_high = cond(subject < partner, partner, subject)
egen pair = group(session supergame id_low id_high)
keep if subject == id_high  // one row per pair
quietly probit both_coop_1 i.treatment, vce(cluster session)
margins, dydx(treatment) predict(pr)
quietly probit both_coop i.treatment, vce(cluster session)
margins, dydx(treatment) predict(pr)
quietly probit both_coop_1 i.treatment if quartile_interaction == 4, vce(cluster session)
margins, dydx(treatment) predict(pr)
quietly probit both_coop i.treatment if quartile_interaction == 4, vce(cluster session)
margins, dydx(treatment) predict(pr)
restore
***********************************************************************


*** LOOK AT OUTCOME AT T-1 ON COOPERATION AT T
preserve
gen lag_CC = both_coop[_n-1] if period != 1
gen lag_DD = both_defect[_n-1] if period != 1
gen lag_cooperate = cooperate[_n-1] if period != 1
gen lag_partner_cooperate = partnercoop[_n-1] if period != 1
gen lag_CD = (lag_cooperate == 1 & lag_partner_cooperate == 0)
gen lag_DC = (lag_cooperate == 0 & lag_partner_cooperate == 1)
gen outcome = 1
replace outcome = 2 if lag_CD == 1
replace outcome = 3 if lag_DC == 1
replace outcome = 4 if lag_DD == 1
label define outcome_lab 1 "C,C" 2 "C,D" 3 "D,C" 4 "D,D"
label values outcome outcome_lab
label define trt 0 "Control" 1 "Tournament"
label values treatment trt
graph bar cooperate if quartile_interaction == 4, over(outcome, gap(0) ) by(treatment, note("")) bar(1, fcolor(gs12) lcolor(black)) ///
blabel(bar, position(outside) format(%4.2f)) yti("") ylab(0(.1).8, labs(small)) ///
name(outcome_historyQ4, replace)
graph bar cooperate, over(outcome, gap(0) ) by(treatment, note("")) bar(1, fcolor(gs12) lcolor(black)) ///
blabel(bar, position(outside) format(%4.2f)) yti("") ylab(0(.1).8, labs(small)) ///
name(outcome_history_all, replace)
graph combine outcome_historyQ4 outcome_history_all, cols(2) ycommon name(outcome_history, replace) ti("Last quartile      All decisions") ///
	  b1title("") l1title("Percentage of cooperation") imargin(0 0 0 0)  
restore
************************************************************************




// Table C.1
preserve
drop if interaction != 1
bysort treatment: sum age female student economics experience risk trust

foreach var of varlist age female student economics experience risk trust {
		reg `var' treatment
		}
restore





* check supergame length across treatments
preserve
drop if subject != 1
sort id supergame
by id supergame: egen max_lenght = max(period)
drop if period != 1
tab max_lenght treatment
hist max_lenght, by(treatment) d frac
ranksum max_lenght, by(treatment)
restore




//FOR STRATEGIES
preserve
* keep only when behaviour seems stable, from supergame 20. This corresponds to about the 74th percentile for sessions with more than 19 supergames. For those with less, keep last 26% of decisions.
drop if interaction < 79 & session == "contr_1"
drop if interaction < 66 & session == "contr_2"
drop if interaction < 101 & session == "contr_3"
drop if interaction < 74 & session == "comp_1"
drop if interaction < 78 & session == "comp_2"
drop if interaction < 88 & session == "comp_3"
rename supergame game
gen choice = "c"
replace choice = "d" if cooperate == 0
gen other_choice = "c"
replace other_choice = "d" if partnercoop == 0
rename treatment treat_number
gen treatment = "Control"
replace treatment = "Tournament" if treat_number == 1
keep treatment id game period choice other_choice session
order treatment id game period choice other_choice session
sort treatment session id game period
export delimited data_for_strategies.txt, delimiter(tab) replace
restore