version 18
clear all

cd "C:\Users\tazze\OneDrive\Desktop"
use SGA_replication_dataset.dta, clear


// Figure 1
preserve
recode treatment (0 = 0 "Control") (1 = 1 "Tournament"), gen(comp)
gen coop_1_percent = coop_1 * 100
graph bar (mean) coop_1_percent, over(comp, gap(*0.5)) blabel(bar, format(%9.2f) size(medium) ) bar(1, color(gs10)) bar(2, color(gs2)) ///
legend(off)  yscale(range(0 100)) ylab(0 (10) 100, nogrid) ///
yti(Percentage of cooperation)  name(first_round_coop, replace)
restore

// Test for Figure 1 (Table C.2)
quietly meprobit coop_1 treatment || id :, vce(cluster id)
margins, predict(pr fixedonly) dydx(_all) post
quietly meprobit coop_1 treatment age female student economics experience risk trust || id :, vce(cluster id)
margins, predict(pr fixedonly) dydx(_all) post

// Figure 2
preserve
drop if period != 1
gen cooperate_percent = cooperate * 100
egen avgcoop = mean(cooperate_percent), by(treatment supergame)
twoway (scatter avgcoop supergame if treatment==0, sort c(l) msymbol(o) mc(gs10) msiz(small) lcol(gs10) ) ///
	(scatter avgcoop supergame if treatment==1, sort c(l) msymbol(diamond) mc(gs3) msiz(small) lcol(gs3) ), ///
	legend(order(2 "Tournament" 1 "Control") ring(0) pos(1)) yti(Percentage of cooperation) xti(Supergame) xlab(1 5 10 15 20 25 30) ysc(r(0 100)) ylab(0(10)100, labs(small)) ///
	 name(first_round_coop_trend, replace)
restore

// Test for Figure 2 (Table 3)
*trend
quietly meprobit coop_1 supergame if treatment == 0 || id :, vce(cluster id)
margins, predict(pr fixedonly) dydx(_all) post
quietly meprobit coop_1 supergame age female student economics experience risk trust if treatment == 0 || id :, vce(cluster id)
margins, predict(pr fixedonly) dydx(_all) post
quietly meprobit coop_1 supergame if treatment == 1 || id :, vce(cluster id)
margins, predict(pr fixedonly) dydx(_all) post
quietly meprobit coop_1 supergame age female student economics experience risk trust if treatment == 1 || id :, vce(cluster id)
margins, predict(pr fixedonly) dydx(_all) post


// Table C.1
preserve
drop if interaction != 1
bysort treatment: sum age female student economics experience risk trust

foreach var of varlist age female student economics experience risk trust {
		reg `var' treatment
		}
restore