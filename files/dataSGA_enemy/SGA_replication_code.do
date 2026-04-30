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
quietly meprobit coop_1 i.treatment || id :, vce(cluster id)
margins, dydx(treatment) predict(pr fixedonly)
quietly meprobit coop_1 i.treatment##c.supergame || id:, vce(cluster id)
margins, dydx(supergame) at(treatment = (0 1)) predict(pr fixedonly) post
lincom _b[supergame:2._at] - _b[supergame:1._at]
* all rounds
bysort treatment: sum cooperate
quietly meprobit cooperate i.treatment || id:, vce(cluster id)
margins, dydx(treatment) predict(pr fixedonly)
quietly meprobit cooperate i.treatment##c.supergame || id:, vce(cluster id)
margins, dydx(supergame) at(treatment = (0 1)) predict(pr fixedonly) post
lincom _b[supergame:2._at] - _b[supergame:1._at]
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





***************************************************
***************************************************
*     APPENDIX
***************************************************
***************************************************


// Table C.1
preserve
drop if interaction != 1
bysort treatment: sum age female student economics experience risk trust

foreach var of varlist age female student economics experience risk trust {
		reg `var' treatment
		}
restore



// Summary by session
preserve
bysort id supergame: egen max_lenght = max(period)
bysort treatment session_id: sum supergame if period == 1 & subject == 1
bysort treatment session_id: sum max_lenght if period == 1 & subject == 1
bysort session_id treatment supergame: egen coop_r1_sg = mean(coop_1)
bysort treatment session_id: sum coop_r1_sg if period == 1 & subject == 1
bysort session_id treatment supergame: egen coop_sg = mean(cooperate)
bysort treatment session_id: sum coop_sg if period == 1 & subject == 1
restore






version 19
clear all
use SGA_replication_dataset.dta, clear

***************************************************
* Figure 1 with CIs


* 1. Save point estimates as in Figure 1
preserve
gen coop_percent_first = 100 * coop_1
gen coop_percent_all   = 100 * cooperate
egen avg_first = mean(coop_percent_first), by(treatment supergame)
egen avg_all   = mean(coop_percent_all),   by(treatment supergame)
keep treatment supergame avg_first avg_all
bys treatment supergame: keep if _n == 1
sort treatment supergame
tempfile pointest
save `pointest', replace
restore


* 2. Subject-block bootstrap
tempname POSTH
tempfile bootreps

postfile `POSTH' ///
    int rep byte treatment int supergame ///
    double mean_first mean_all ///
    using `bootreps', replace

local B = 10000
set seed 123456789

forvalues b = 1/`B' {
    preserve

    * Resample subjects within treatment
    bsample, cluster(id) strata(treatment)

    gen coop_percent_first = 100 * coop_1
    gen coop_percent_all   = 100 * cooperate

    * Compute the SAME statistics as in Figure 1
    collapse (mean) mean_first = coop_percent_first ///
             (mean) mean_all   = coop_percent_all, ///
             by(treatment supergame)

    quietly {
        forvalues i = 1/`=_N' {
            post `POSTH' ///
                (`b') ///
                (treatment[`i']) ///
                (supergame[`i']) ///
                (mean_first[`i']) ///
                (mean_all[`i'])
        }
    }

    restore
}

postclose `POSTH'


* 3. Build percentile 95% CIs
use `bootreps', clear
bys treatment supergame: egen lo_first = pctile(mean_first), p(2.5)
bys treatment supergame: egen hi_first = pctile(mean_first), p(97.5)
bys treatment supergame: egen lo_all   = pctile(mean_all),   p(2.5)
bys treatment supergame: egen hi_all   = pctile(mean_all),   p(97.5)
bys treatment supergame: gen nreps = _N
bys treatment supergame: keep if _n == 1
keep treatment supergame lo_first hi_first lo_all hi_all nreps
sort treatment supergame
tempfile cis
save `cis', replace


* 4. Merge CIs onto original Figure 1
use `pointest', clear
merge 1:1 treatment supergame using `cis', nogen
sort treatment supergame
tempfile plotdata
save `plotdata', replace


* First rounds
use `plotdata', clear
twoway ///
    (rarea lo_first hi_first supergame if treatment==0, ///
        sort fcolor(gs10%30) lwidth(none)) ///
    (rarea lo_first hi_first supergame if treatment==1, ///
        sort fcolor(gs3%30) lwidth(none)) ///
    (scatter avg_first supergame if treatment==0, ///
        sort c(l) msymbol(o) mc(gs10) msiz(small) lcol(gs10%70)) ///
    (scatter avg_first supergame if treatment==1, ///
        sort c(l) msymbol(diamond) mc(gs3) msiz(small) lcol(gs3%70)), ///
    legend(order(3 "Control" 4 "Tournament") ring(0) pos(1)) ///
    yti("") xti("") ti("First rounds") ///
    xlab(1 5 10 15 20 25 30) ysc(r(0 100)) ///
    ylab(0(10)100, labs(small)) ///
    name(first_rounds_coop_trend_ci, replace)
	

* All rounds
twoway ///
    (rarea lo_all hi_all supergame if treatment==0, ///
        sort fcolor(gs10%30) lwidth(none)) ///
    (rarea lo_all hi_all supergame if treatment==1, ///
        sort fcolor(gs3%30) lwidth(none)) ///
    (scatter avg_all supergame if treatment==0, ///
        sort c(l) msymbol(o) mc(gs10) msiz(small) lcol(gs10%70)) ///
    (scatter avg_all supergame if treatment==1, ///
        sort c(l) msymbol(diamond) mc(gs3) msiz(small) lcol(gs3%70)), ///
    legend(order(3 "Control" 4 "Tournament") ring(0) pos(1)) ///
    ytitle("") xtitle("") title("All rounds") ///
    xlabel(1 5 10 15 20 25 30) ///
    yscale(range(0 100)) ///
    ylabel(0(10)100, labsize(small)) ///
    name(all_rounds_coop_trend_ci, replace)

graph combine first_rounds_coop_trend_ci all_rounds_coop_trend_ci, cols(2) ycommon name(cooperation_ci, replace) ///
	  b1title("Supergame") l1title("Percentage of cooperation")
graph export cooperation_CI.pdf, replace





***************************************************
* Figure 2 with CIs             
use SGA_replication_dataset.dta, clear
sort id supergame interaction
bysort id supergame: gen int n_in_sg = _N
gen first_in_sg = (period == 1)
by id (supergame interaction): gen long cum_inter = sum(n_in_sg * first_in_sg)
by id: egen total_inter = max(interaction)
gen quartile_interaction = ceil(4 * cum_inter / total_inter)
gen id_low  = cond(subject < partner, subject, partner)
gen id_high = cond(subject < partner, partner, subject)
egen pair = group(session supergame id_low id_high)
tempfile raw2
save `raw2', replace


* 1. Original point estimates
tempname POSTP
tempfile pointest2

postfile `POSTP' ///
    str10 panel byte treatment byte outcome double frac ///
    using `pointest2', replace

* First rounds
use `raw2', clear
keep if subject == id_high
collapse (mean) both_coop_1 one_coop_1 both_defect_1, by(treatment)
rename (both_coop_1 one_coop_1 both_defect_1) (x1 x2 x3)
reshape long x, i(treatment) j(outcome)
forvalues i = 1/`=_N' {
    post `POSTP' ("first") (treatment[`i']) (outcome[`i']) (x[`i'])
}

* All rounds
use `raw2', clear
keep if subject == id_high
collapse (mean) both_coop one_coop both_defect, by(treatment)
rename (both_coop one_coop both_defect) (x1 x2 x3)
reshape long x, i(treatment) j(outcome)
forvalues i = 1/`=_N' {
    post `POSTP' ("all") (treatment[`i']) (outcome[`i']) (x[`i'])
}

* First rounds, last quartile
use `raw2', clear
keep if quartile_interaction == 4
keep if subject == id_high
collapse (mean) both_coop_1 one_coop_1 both_defect_1, by(treatment)
rename (both_coop_1 one_coop_1 both_defect_1) (x1 x2 x3)
reshape long x, i(treatment) j(outcome)
forvalues i = 1/`=_N' {
    post `POSTP' ("first_q4") (treatment[`i']) (outcome[`i']) (x[`i'])
}

* All rounds, last quartile
use `raw2', clear
keep if quartile_interaction == 4
keep if subject == id_high
collapse (mean) both_coop one_coop both_defect, by(treatment)
rename (both_coop one_coop both_defect) (x1 x2 x3)
reshape long x, i(treatment) j(outcome)
forvalues i = 1/`=_N' {
    post `POSTP' ("all_q4") (treatment[`i']) (outcome[`i']) (x[`i'])
}

postclose `POSTP'



* 2. Pair-block bootstrap
tempname POSTB
tempfile bootreps2

postfile `POSTB' ///
    int rep str10 panel byte treatment byte outcome double frac ///
    using `bootreps2', replace

local B = 10000
set seed 123456789

forvalues b = 1/`B' {

    * Start from raw data every time
    use `raw2', clear

    * One row per pair
    keep if subject == id_high

    * Resample pair blocks within treatment
    bsample, cluster(pair) strata(treatment)

    tempfile bootsample
    save `bootsample', replace

    * First rounds
    use `bootsample', clear
    collapse (mean) both_coop_1 one_coop_1 both_defect_1, by(treatment)
    rename (both_coop_1 one_coop_1 both_defect_1) (x1 x2 x3)
    reshape long x, i(treatment) j(outcome)
    forvalues i = 1/`=_N' {
        post `POSTB' (`b') ("first") (treatment[`i']) (outcome[`i']) (x[`i'])
    }

    * All rounds
    use `bootsample', clear
    collapse (mean) both_coop one_coop both_defect, by(treatment)
    rename (both_coop one_coop both_defect) (x1 x2 x3)
    reshape long x, i(treatment) j(outcome)
    forvalues i = 1/`=_N' {
        post `POSTB' (`b') ("all") (treatment[`i']) (outcome[`i']) (x[`i'])
    }

    * First rounds, last quartile
    use `bootsample', clear
    keep if quartile_interaction == 4
    collapse (mean) both_coop_1 one_coop_1 both_defect_1, by(treatment)
    rename (both_coop_1 one_coop_1 both_defect_1) (x1 x2 x3)
    reshape long x, i(treatment) j(outcome)
    forvalues i = 1/`=_N' {
        post `POSTB' (`b') ("first_q4") (treatment[`i']) (outcome[`i']) (x[`i'])
    }

    * All rounds, last quartile
    use `bootsample', clear
    keep if quartile_interaction == 4
    collapse (mean) both_coop one_coop both_defect, by(treatment)
    rename (both_coop one_coop both_defect) (x1 x2 x3)
    reshape long x, i(treatment) j(outcome)
    forvalues i = 1/`=_N' {
        post `POSTB' (`b') ("all_q4") (treatment[`i']) (outcome[`i']) (x[`i'])
    }
}

postclose `POSTB'

* 3. Percentile 95% CIs
use `bootreps2', clear
bys panel treatment outcome: egen lo = pctile(frac), p(2.5)
bys panel treatment outcome: egen hi = pctile(frac), p(97.5)
bys panel treatment outcome: keep if _n == 1
keep panel treatment outcome lo hi
sort panel treatment outcome
tempfile cis2
save `cis2', replace

* 4. Merge CIs onto means
use `pointest2', clear
merge 1:1 panel treatment outcome using `cis2', nogen
sort panel treatment outcome
label define trt 0 "Control" 1 "Tournament", replace
label values treatment trt
label define outcome_lab 1 "C,C" 2 "C,D;D,C" 3 "D,D", replace
label values outcome outcome_lab
gen frac_lbl = string(frac, "%4.2f")
tempfile plotdata2
save `plotdata2', replace


use `plotdata2', clear
* First rounds
twoway ///
    (bar frac outcome if panel=="first", barw(.8) fcolor(gs12) lcolor(black)) ///
    (rcap hi lo outcome if panel=="first", lcolor(black)) ///
    (scatter frac outcome if panel=="first", msymbol(none) mlabel(frac_lbl) ///
        mlabposition(2) mlabsize(small) mlabcolor(black)), ///
    by(treatment, cols(2) legend(off) title("First rounds") note("")) ///
    xlabel(1 "C,C" 2 "C,D;D,C" 3 "D,D", angle(45)) ///
    yscale(range(0 .6)) ylabel(0(.1).6, labsize(small)) ///
    ytitle("") xtitle("") ///
    name(outcomes_first_round_ci, replace)

* All rounds
twoway ///
    (bar frac outcome if panel=="all", barw(.8) fcolor(gs12) lcolor(black)) ///
    (rcap hi lo outcome if panel=="all", lcolor(black)) ///
    (scatter frac outcome if panel=="all", msymbol(none) mlabel(frac_lbl) ///
        mlabposition(2) mlabsize(small) mlabcolor(black)), ///
    by(treatment, cols(2) legend(off) title("All rounds") note("")) ///
    xlabel(1 "C,C" 2 "C,D;D,C" 3 "D,D", angle(45)) ///
    yscale(range(0 .6)) ylabel(0(.1).6, labsize(small)) ///
    ytitle("") xtitle("") ///
    name(outcomes_all_rounds_ci, replace)

* First rounds, last quartile
twoway ///
    (bar frac outcome if panel=="first_q4", barw(.8) fcolor(gs12) lcolor(black)) ///
    (rcap hi lo outcome if panel=="first_q4", lcolor(black)) ///
    (scatter frac outcome if panel=="first_q4", msymbol(none) mlabel(frac_lbl) ///
        mlabposition(2) mlabsize(small) mlabcolor(black)), ///
    by(treatment, cols(2) legend(off) subtitle("Last quartile") note("")) ///
    xlabel(1 "C,C" 2 "C,D;D,C" 3 "D,D", angle(45)) ///
    yscale(range(0 .6)) ylabel(0(.1).6, labsize(small)) ///
    ytitle("") xtitle("") ///
    name(outcomes_first_round_q4_ci, replace)

* All rounds, last quartile
twoway ///
    (bar frac outcome if panel=="all_q4", barw(.8) fcolor(gs12) lcolor(black)) ///
    (rcap hi lo outcome if panel=="all_q4", lcolor(black)) ///
    (scatter frac outcome if panel=="all_q4", msymbol(none) mlabel(frac_lbl) ///
        mlabposition(2) mlabsize(small) mlabcolor(black)), ///
    by(treatment, cols(2) legend(off) subtitle("Last quartile") note("")) ///
    xlabel(1 "C,C" 2 "C,D;D,C" 3 "D,D", angle(45)) ///
    yscale(range(0 .6)) ylabel(0(.1).6, labsize(small)) ///
    ytitle("") xtitle("") ///
    name(outcomes_all_rounds_q4_ci, replace)

graph combine ///
    outcomes_first_round_ci ///
    outcomes_all_rounds_ci ///
    outcomes_first_round_q4_ci ///
    outcomes_all_rounds_q4_ci, ///
    cols(2) ycommon name(outcomes_ci, replace) ///
    b1title("") l1title("Fraction") imargin(0 0 0 0)

	
