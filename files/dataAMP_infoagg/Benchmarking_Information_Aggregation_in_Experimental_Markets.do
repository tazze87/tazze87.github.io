use data_for_replication.dta

***************************************
*************  SECTION 3  *************
***************************************

////////////   FIGURE 1   ////////////
preserve
drop if individual != 1
recode mkt (0 = 0 "BDM") (1 = 1 "Market"), gen(Market)
bysort mkt PI period: egen avgpHH = mean(marketpriceH)
bysort mkt PI period: egen avgpLL = mean(marketpriceL)
twoway (scatter avgpHH avgpLL period if PI == 0, msymbol(i i) mc(gs1 gs9) msiz(vsmall vsmall) sort c(l l) lpattern(solid solid) lw(medthick   medthick ) lc(gs1 gs9)) ///
	   (scatter avgpHH avgpLL period if PI == 1, msymbol(o o) mc(gs1 gs9) msiz(small small) sort c(l l) lpattern(shortdash shortdash) lw(medthin medthin) lc(gs1 gs9)), ///
		by(Market, graphregion(color(white)) bgcolor(white) ) ///
		ysc(r(50 120)) ylab(50(10)120, notick labs(vsmall)) ///
		legend(order(1 "Price H, Public Info" 2 "Price L, Public Info" 3 "Price H, Private Info" 4 "Price L, Private Info")) ///
		xlab(0(5)30, notick labs(vsmall)) ///   //axes
		xli(10,lp(dash)lc(gs9) lw(thin)) xli(20,lp(dash)lc(gs9)lw(thin)) ///      // repetitions
		yti("Average asset prices") name(Figure_1, replace)
restore


////////////   TABLE 2   ////////////
preserve
drop if individual != 1   // since we need only one observation per group

eststo clear
//---------- linear model-----
xtreg probH PI mkt PIXmkt, re vce(robust)
estadd scalar Obs = e(N)
estadd scalar betadelta = _b[PI] + _b[PIXmkt]
test _b[PI] + _b[PIXmkt] =0
estadd scalar p_valuebetadelta = r(p)
testnl abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]) = 0  //  nlcom gives the same result
local sign_coeff = sign(abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]))
estadd scalar p_valueabsolute = 1-normal(`sign_coeff'*sqrt(r(chi2)))
estadd scalar gammadelta = _b[mkt] + _b[PIXmkt]
test _b[mkt] + _b[PIXmkt] =0
estadd scalar p_valuegammadelta = r(p) 

xtreg probH PI mkt PIXmkt rho rhoXmkt, re vce(robust)
estadd scalar Obs = e(N)
estadd scalar betadelta = _b[PI] + _b[PIXmkt]
test _b[PI] + _b[PIXmkt] =0
estadd scalar p_valuebetadelta = r(p)
testnl abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]) = 0  //  nlcom gives the same result
local sign_coeff = sign(abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]))
estadd scalar p_valueabsolute = 1-normal(`sign_coeff'*sqrt(r(chi2)))
estadd scalar gammadelta = _b[mkt] + _b[PIXmkt]
test _b[mkt] + _b[PIXmkt] =0
estadd scalar p_valuegammadelta = r(p) 

xtreg probH PI mkt PIXmkt rho rhoXmkt rep1 rep2, re vce(robust)
estadd scalar Obs = e(N)
estadd scalar betadelta = _b[PI] + _b[PIXmkt]
test _b[PI] + _b[PIXmkt] =0
estadd scalar p_valuebetadelta = r(p)
testnl abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]) = 0  //  nlcom gives the same result
local sign_coeff = sign(abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]))
estadd scalar p_valueabsolute = 1-normal(`sign_coeff'*sqrt(r(chi2)))
estadd scalar gammadelta = _b[mkt] + _b[PIXmkt]
test _b[mkt] + _b[PIXmkt] =0
estadd scalar p_valuegammadelta = r(p)  

//---------- probit model-------
quietly xtprobit probH PI mkt PIXmkt, re vce(robust) nolog
margins, predict(pu0) dydx(*) post
estadd scalar Obs = e(N)
estadd scalar betadelta = _b[PI] + _b[PIXmkt]
test _b[PI] + _b[PIXmkt] =0
estadd scalar p_valuebetadelta = r(p)
testnl abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]) = 0  //  nlcom gives the same result
local sign_coeff = sign(abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]))
estadd scalar p_valueabsolute = 1-normal(`sign_coeff'*sqrt(r(chi2)))
estadd scalar gammadelta = _b[mkt] + _b[PIXmkt]
test _b[mkt] + _b[PIXmkt] =0
estadd scalar p_valuegammadelta = r(p)  

quietly xtprobit probH PI mkt PIXmkt rho rhoXmkt, re vce(robust) nolog
margins, predict(pu0) dydx(*) post
estadd scalar Obs = e(N)
estadd scalar betadelta = _b[PI] + _b[PIXmkt]
test _b[PI] + _b[PIXmkt] =0
estadd scalar p_valuebetadelta = r(p)
testnl abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]) = 0  //  nlcom gives the same result
local sign_coeff = sign(abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]))
estadd scalar p_valueabsolute = 1-normal(`sign_coeff'*sqrt(r(chi2)))
estadd scalar gammadelta = _b[mkt] + _b[PIXmkt]
test _b[mkt] + _b[PIXmkt] =0
estadd scalar p_valuegammadelta = r(p)  

quietly xtprobit probH PI mkt PIXmkt rho rhoXmkt rep1 rep2, re vce(robust)
margins, predict(pu0) dydx(*) post
estadd scalar Obs = e(N)
estadd scalar betadelta = _b[PI] + _b[PIXmkt]
test _b[PI] + _b[PIXmkt] =0
estadd scalar p_valuebetadelta = r(p)
testnl abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]) = 0  //  nlcom gives the same result
local sign_coeff = sign(abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]))
estadd scalar p_valueabsolute = 1-normal(`sign_coeff'*sqrt(r(chi2)))
estadd scalar gammadelta = _b[mkt] + _b[PIXmkt]
test _b[mkt] + _b[PIXmkt] =0
estadd scalar p_valuegammadelta = r(p)  
restore


////////////   TABLE 3   ////////////
preserve
drop if individual != 1   // since we need only one observation per group

xtreg marketpriceH PI mkt PIXmkt, re vce(robust)
estadd scalar Obs = e(N)
estadd scalar betadelta = _b[PI] + _b[PIXmkt]
test _b[PI] + _b[PIXmkt] =0
estadd scalar p_valuebetadelta = r(p)
testnl abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]) = 0  //  nlcom gives the same result
local sign_coeff = sign(abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]))
estadd scalar p_valueabsolute = 1-normal(`sign_coeff'*sqrt(r(chi2)))
estadd scalar gammadelta = _b[mkt] + _b[PIXmkt]
test _b[mkt] + _b[PIXmkt] =0
estadd scalar p_valuegammadelta = r(p)


xtreg marketpriceH PI mkt PIXmkt rho rhoXmkt, re vce(robust)
estadd scalar Obs = e(N)
estadd scalar betadelta = _b[PI] + _b[PIXmkt]
test _b[PI] + _b[PIXmkt] =0
estadd scalar p_valuebetadelta = r(p)
testnl abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]) = 0  //  nlcom gives the same result
local sign_coeff = sign(abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]))
estadd scalar p_valueabsolute = 1-normal(`sign_coeff'*sqrt(r(chi2)))
estadd scalar gammadelta = _b[mkt] + _b[PIXmkt]
test _b[mkt] + _b[PIXmkt] =0
estadd scalar p_valuegammadelta = r(p) 


xtreg marketpriceH PI mkt PIXmkt rho rhoXmkt rep1 rep2, re vce(robust)
estadd scalar Obs = e(N)
estadd scalar betadelta = _b[PI] + _b[PIXmkt]
test _b[PI] + _b[PIXmkt] =0
estadd scalar p_valuebetadelta = r(p)
testnl abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]) = 0  //  nlcom gives the same result
local sign_coeff = sign(abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]))
estadd scalar p_valueabsolute = 1-normal(`sign_coeff'*sqrt(r(chi2)))
estadd scalar gammadelta = _b[mkt] + _b[PIXmkt]
test _b[mkt] + _b[PIXmkt] =0
estadd scalar p_valuegammadelta = r(p) 


xtreg marketpriceL PI mkt PIXmkt, re vce(robust)
estadd scalar Obs = e(N)
estadd scalar betadelta = _b[PI] + _b[PIXmkt]
test _b[PI] + _b[PIXmkt] =0
estadd scalar p_valuebetadelta = r(p)
testnl abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]) = 0  //  nlcom gives the same result
local sign_coeff = sign(abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]))
estadd scalar p_valueabsolute = 1-normal(`sign_coeff'*sqrt(r(chi2)))
estadd scalar gammadelta = _b[mkt] + _b[PIXmkt]
test _b[mkt] + _b[PIXmkt] =0
estadd scalar p_valuegammadelta = r(p) 


xtreg marketpriceL PI mkt PIXmkt rho rhoXmkt, re vce(robust)
estadd scalar Obs = e(N)
estadd scalar betadelta = _b[PI] + _b[PIXmkt]
test _b[PI] + _b[PIXmkt] =0
estadd scalar p_valuebetadelta = r(p)
testnl abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]) = 0  //  nlcom gives the same result
local sign_coeff = sign(abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]))
estadd scalar p_valueabsolute = 1-normal(`sign_coeff'*sqrt(r(chi2)))
estadd scalar gammadelta = _b[mkt] + _b[PIXmkt]
test _b[mkt] + _b[PIXmkt] =0
estadd scalar p_valuegammadelta = r(p) 
eststo AssetL2

xtreg marketpriceL PI mkt PIXmkt rho rhoXmkt rep1 rep2, re vce(robust)
estadd scalar Obs = e(N)
estadd scalar betadelta = _b[PI] + _b[PIXmkt]
test _b[PI] + _b[PIXmkt] =0
estadd scalar p_valuebetadelta = r(p)
testnl abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]) = 0  //  nlcom gives the same result
local sign_coeff = sign(abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]))
estadd scalar p_valueabsolute = 1-normal(`sign_coeff'*sqrt(r(chi2)))
estadd scalar gammadelta = _b[mkt] + _b[PIXmkt]
test _b[mkt] + _b[PIXmkt] =0
estadd scalar p_valuegammadelta = r(p) 
restore




***************************************
*************  SECTION 4  *************
***************************************
preserve
drop if period !=1
drop if mkt == 0
tab sensitive
bysort PI: tab sensitive
restore

////////////   TABLE 4   ////////////
eststo clear
// Asset H
// sensitive
xtreg askpriceH PI mkt PIXmkt rho rep1 rep2 if pnotsensH == 0, re vce(cluster group)
estadd scalar Obs = e(N)
estadd scalar betadelta = _b[PI] + _b[PIXmkt]
test _b[PI] + _b[PIXmkt] =0
estadd scalar p_valuebetadelta = r(p)
testnl abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]) = 0  //  nlcom gives the same result
local sign_coeff = sign(abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]))
estadd scalar p_valueabsolute = 1-normal(`sign_coeff'*sqrt(r(chi2)))
estadd scalar gammadelta = _b[mkt] + _b[PIXmkt]
test _b[mkt] + _b[PIXmkt] =0
estadd scalar p_valuegammadelta = r(p)
eststo AskHsens

xtreg bidpriceH PI mkt PIXmkt rho rep1 rep2 if pnotsensH == 0, re vce(cluster group)
estadd scalar Obs = e(N)
estadd scalar betadelta = _b[PI] + _b[PIXmkt]
test _b[PI] + _b[PIXmkt] =0
estadd scalar p_valuebetadelta = r(p)
testnl abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]) = 0  //  nlcom gives the same result
local sign_coeff = sign(abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]))
estadd scalar p_valueabsolute = 1-normal(`sign_coeff'*sqrt(r(chi2)))
estadd scalar gammadelta = _b[mkt] + _b[PIXmkt]
test _b[mkt] + _b[PIXmkt] =0
estadd scalar p_valuegammadelta = r(p)
eststo BidHsens

// not sensitive
xtreg askpriceH PI mkt PIXmkt rho rep1 rep2 if pnotsensH == 1, re vce(cluster group)
estadd scalar Obs = e(N)
estadd scalar betadelta = _b[PI] + _b[PIXmkt]
test _b[PI] + _b[PIXmkt] =0
estadd scalar p_valuebetadelta = r(p)
testnl abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]) = 0  //  nlcom gives the same result
local sign_coeff = sign(abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]))
estadd scalar p_valueabsolute = 1-normal(`sign_coeff'*sqrt(r(chi2)))
estadd scalar gammadelta = _b[mkt] + _b[PIXmkt]
test _b[mkt] + _b[PIXmkt] =0
estadd scalar p_valuegammadelta = r(p)
eststo AskHnotsens

xtreg bidpriceH PI mkt PIXmkt rho rep1 rep2 if pnotsensH == 1, re vce(cluster group)
estadd scalar Obs = e(N)
estadd scalar betadelta = _b[PI] + _b[PIXmkt]
test _b[PI] + _b[PIXmkt] =0
estadd scalar p_valuebetadelta = r(p)
testnl abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]) = 0  //  nlcom gives the same result
local sign_coeff = sign(abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]))
estadd scalar p_valueabsolute = 1-normal(`sign_coeff'*sqrt(r(chi2)))
estadd scalar gammadelta = _b[mkt] + _b[PIXmkt]
test _b[mkt] + _b[PIXmkt] =0
estadd scalar p_valuegammadelta = r(p)
eststo BidHnotsens

// Asset L
// sensitive
xtreg askpriceL PI mkt PIXmkt rho rep1 rep2 if pnotsensL == 0, re vce(cluster group)
estadd scalar Obs = e(N)
estadd scalar betadelta = _b[PI] + _b[PIXmkt]
test _b[PI] + _b[PIXmkt] =0
estadd scalar p_valuebetadelta = r(p)
testnl abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]) = 0  //  nlcom gives the same result
local sign_coeff = sign(abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]))
estadd scalar p_valueabsolute = 1-normal(`sign_coeff'*sqrt(r(chi2)))
estadd scalar gammadelta = _b[mkt] + _b[PIXmkt]
test _b[mkt] + _b[PIXmkt] =0
estadd scalar p_valuegammadelta = r(p)
eststo AskLsens

xtreg bidpriceL PI mkt PIXmkt rho rep1 rep2 if pnotsensL == 0, re vce(cluster group)
estadd scalar Obs = e(N)
estadd scalar betadelta = _b[PI] + _b[PIXmkt]
test _b[PI] + _b[PIXmkt] =0
estadd scalar p_valuebetadelta = r(p)
testnl abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]) = 0  //  nlcom gives the same result
local sign_coeff = sign(abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]))
estadd scalar p_valueabsolute = 1-normal(`sign_coeff'*sqrt(r(chi2)))
estadd scalar gammadelta = _b[mkt] + _b[PIXmkt]
test _b[mkt] + _b[PIXmkt] =0
estadd scalar p_valuegammadelta = r(p)
eststo BidLsens

// not sensitive
xtreg askpriceL PI mkt PIXmkt rho rep1 rep2 if pnotsensL == 1, re vce(cluster group)
estadd scalar Obs = e(N)
estadd scalar betadelta = _b[PI] + _b[PIXmkt]
test _b[PI] + _b[PIXmkt] =0
estadd scalar p_valuebetadelta = r(p)
testnl abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]) = 0  //  nlcom gives the same result
local sign_coeff = sign(abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]))
estadd scalar p_valueabsolute = 1-normal(`sign_coeff'*sqrt(r(chi2)))
estadd scalar gammadelta = _b[mkt] + _b[PIXmkt]
test _b[mkt] + _b[PIXmkt] =0
estadd scalar p_valuegammadelta = r(p)
eststo AskLnotsens

xtreg bidpriceL PI mkt PIXmkt rho rep1 rep2 if pnotsensL == 1, re vce(cluster group)
estadd scalar Obs = e(N)
estadd scalar betadelta = _b[PI] + _b[PIXmkt]
test _b[PI] + _b[PIXmkt] =0
estadd scalar p_valuebetadelta = r(p)
testnl abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]) = 0  //  nlcom gives the same result
local sign_coeff = sign(abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]))
estadd scalar p_valueabsolute = 1-normal(`sign_coeff'*sqrt(r(chi2)))
estadd scalar gammadelta = _b[mkt] + _b[PIXmkt]
test _b[mkt] + _b[PIXmkt] =0
estadd scalar p_valuegammadelta = r(p)
eststo BidLnotsens



****************************************
*************  APPENDIX B  *************
****************************************

////////////   TABLE B.1   ////////////
preserve
drop if period != 1

tab Origin if mkt == 0 & PI==0
tab Field if mkt == 0 & PI==0
tabstat Female age years risk machiav RCI Optimism Open Consc Extra Agree Neuro if mkt == 0 & PI==0, columns(statistics) statistics(mean sd count)

tab Origin if mkt == 1 & PI==0
tab Field if mkt == 1 & PI==0
tabstat Female age years risk machiav RCI Optimism Open Consc Extra Agree Neuro if mkt == 1 & PI==0, columns(statistics) statistics(mean sd count)

tab Origin if mkt == 0 & PI==1
tab Field if mkt == 0 & PI==1
tabstat Female age years risk machiav RCI Optimism Open Consc Extra Agree Neuro if mkt == 0 & PI==1, columns(statistics) statistics(mean sd count)

tab Origin if mkt == 1 & PI==1
tab Field if mkt == 1 & PI==1
tabstat Female age years risk machiav RCI Optimism Open Consc Extra Agree Neuro if mkt == 1 & PI==1, columns(statistics) statistics(mean sd count)
restore


****************************************
*************  APPENDIX D  *************
****************************************

////////////   TABLE D.2   ////////////
preserve
drop if period !=1
reg Female PI mkt PIXmkt
reg age PI mkt PIXmkt
reg risk PI mkt PIXmkt
reg machiav PI mkt PIXmkt
reg Optimism PI mkt PIXmkt
restore


////////////   TABLE D.3   ////////////
//---------------------- TABLE with SC - Ranking with SC ------------------------------
preserve
drop if individual != 1   // since we need only one observation per group

eststo clear
//----------using linear model-----
xtreg probH PI mkt PIXmkt SC SCXPI SCXmkt SCXPIXmkt, re vce(robust)
estadd scalar Obs = e(N)
estadd scalar betadelta = _b[PI] + _b[PIXmkt]
test _b[PI] + _b[PIXmkt] =0
estadd scalar p_valuebetadelta = r(p)
testnl abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]) = 0  //  nlcom gives the same result
local sign_coeff = sign(abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]))
estadd scalar p_valueabsolute = 1-normal(`sign_coeff'*sqrt(r(chi2)))
estadd scalar gammadelta = _b[mkt] + _b[PIXmkt]
test _b[mkt] + _b[PIXmkt] =0
estadd scalar p_valuegammadelta = r(p)
//some tests for SC
estadd scalar diffmktbdmfull = _b[mkt] + _b[SCXmkt]
test _b[mkt] + _b[SCXmkt]=0 // diff between Mkt and BDM under Full Info and SC
estadd scalar p_valuedifffull = r(p)
estadd scalar diffmktbdmPI = _b[mkt] + _b[PIXmkt] + _b[SCXmkt] + _b[SCXPIXmkt]
test _b[mkt] + _b[PIXmkt] + _b[SCXmkt] + _b[SCXPIXmkt]=0 // diff between Mkt and BDM under PI and SC
estadd scalar p_valuediffPI = r(p) 

xtreg probH PI mkt PIXmkt SC SCXPI SCXmkt SCXPIXmkt rho rhoXmkt SCXrho SCXrhoXmkt, re vce(robust)
estadd scalar Obs = e(N)
estadd scalar betadelta = _b[PI] + _b[PIXmkt]
test _b[PI] + _b[PIXmkt] =0
estadd scalar p_valuebetadelta = r(p)
testnl abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]) = 0  //  nlcom gives the same result
local sign_coeff = sign(abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]))
estadd scalar p_valueabsolute = 1-normal(`sign_coeff'*sqrt(r(chi2)))
estadd scalar gammadelta = _b[mkt] + _b[PIXmkt]
test _b[mkt] + _b[PIXmkt] =0
estadd scalar p_valuegammadelta = r(p)
//some tests for SC
estadd scalar diffmktbdmfull = _b[mkt] + _b[SCXmkt]
test _b[mkt] + _b[SCXmkt]=0 // diff between Mkt and BDM under Full Info and SC
estadd scalar p_valuedifffull = r(p)
estadd scalar diffmktbdmPI = _b[mkt] + _b[PIXmkt] + _b[SCXmkt] + _b[SCXPIXmkt]
test _b[mkt] + _b[PIXmkt] + _b[SCXmkt] + _b[SCXPIXmkt]=0 // diff between Mkt and BDM under PI and SC
estadd scalar p_valuediffPI = r(p) 

xtreg probH PI mkt PIXmkt SC SCXPI SCXmkt SCXPIXmkt rho rhoXmkt SCXrho SCXrhoXmkt rep1 rep2, re vce(robust)
estadd scalar Obs = e(N)
estadd scalar betadelta = _b[PI] + _b[PIXmkt]
test _b[PI] + _b[PIXmkt] =0
estadd scalar p_valuebetadelta = r(p)
testnl abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]) = 0  //  nlcom gives the same result
local sign_coeff = sign(abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]))
estadd scalar p_valueabsolute = 1-normal(`sign_coeff'*sqrt(r(chi2)))
estadd scalar gammadelta = _b[mkt] + _b[PIXmkt]
test _b[mkt] + _b[PIXmkt] =0
estadd scalar p_valuegammadelta = r(p)
//some tests for SC
estadd scalar diffmktbdmfull = _b[mkt] + _b[SCXmkt]
test _b[mkt] + _b[SCXmkt]=0 // diff between Mkt and BDM under Full Info and SC
estadd scalar p_valuedifffull = r(p)
estadd scalar diffmktbdmPI = _b[mkt] + _b[PIXmkt] + _b[SCXmkt] + _b[SCXPIXmkt]
test _b[mkt] + _b[PIXmkt] + _b[SCXmkt] + _b[SCXPIXmkt]=0 // diff between Mkt and BDM under PI and SC
estadd scalar p_valuediffPI = r(p)  

//----------using probit model-----
quietly xtprobit probH PI mkt PIXmkt SC SCXPI SCXmkt SCXPIXmkt, re vce(robust) nolog
margins, predict(pu0) dydx(*) post
estadd scalar Obs = e(N)
estadd scalar betadelta = _b[PI] + _b[PIXmkt]
test _b[PI] + _b[PIXmkt] =0
estadd scalar p_valuebetadelta = r(p)
testnl abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]) = 0  //  nlcom gives the same result
local sign_coeff = sign(abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]))
estadd scalar p_valueabsolute = 1-normal(`sign_coeff'*sqrt(r(chi2)))
estadd scalar gammadelta = _b[mkt] + _b[PIXmkt]
test _b[mkt] + _b[PIXmkt] =0
estadd scalar p_valuegammadelta = r(p)
//some tests for SC
estadd scalar diffmktbdmfull = _b[mkt] + _b[SCXmkt]
test _b[mkt] + _b[SCXmkt]=0 // diff between Mkt and BDM under Full Info and SC
estadd scalar p_valuedifffull = r(p)
estadd scalar diffmktbdmPI = _b[mkt] + _b[PIXmkt] + _b[SCXmkt] + _b[SCXPIXmkt]
test _b[mkt] + _b[PIXmkt] + _b[SCXmkt] + _b[SCXPIXmkt]=0 // diff between Mkt and BDM under PI and SC
estadd scalar p_valuediffPI = r(p) 

quietly xtprobit probH PI mkt PIXmkt SC SCXPI SCXmkt SCXPIXmkt rho rhoXmkt SCXrho SCXrhoXmkt, re vce(robust) nolog
margins, predict(pu0) dydx(*) post
estadd scalar Obs = e(N)
estadd scalar betadelta = _b[PI] + _b[PIXmkt]
test _b[PI] + _b[PIXmkt] =0
estadd scalar p_valuebetadelta = r(p)
testnl abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]) = 0  //  nlcom gives the same result
local sign_coeff = sign(abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]))
estadd scalar p_valueabsolute = 1-normal(`sign_coeff'*sqrt(r(chi2)))
estadd scalar gammadelta = _b[mkt] + _b[PIXmkt]
test _b[mkt] + _b[PIXmkt] =0
estadd scalar p_valuegammadelta = r(p)
//some tests for SC
estadd scalar diffmktbdmfull = _b[mkt] + _b[SCXmkt]
test _b[mkt] + _b[SCXmkt]=0 // diff between Mkt and BDM under Full Info and SC
estadd scalar p_valuedifffull = r(p)
estadd scalar diffmktbdmPI = _b[mkt] + _b[PIXmkt] + _b[SCXmkt] + _b[SCXPIXmkt]
test _b[mkt] + _b[PIXmkt] + _b[SCXmkt] + _b[SCXPIXmkt]=0 // diff between Mkt and BDM under PI and SC
estadd scalar p_valuediffPI = r(p) 

quietly xtprobit probH PI mkt PIXmkt SC SCXPI SCXmkt SCXPIXmkt rho rhoXmkt SCXrho SCXrhoXmkt rep1 rep2, re vce(robust) nolog
margins, predict(pu0) dydx(*) post
estadd scalar Obs = e(N)
estadd scalar betadelta = _b[PI] + _b[PIXmkt]
test _b[PI] + _b[PIXmkt] =0
estadd scalar p_valuebetadelta = r(p)
testnl abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]) = 0  //  nlcom gives the same result
local sign_coeff = sign(abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]))
estadd scalar p_valueabsolute = 1-normal(`sign_coeff'*sqrt(r(chi2)))
estadd scalar gammadelta = _b[mkt] + _b[PIXmkt]
test _b[mkt] + _b[PIXmkt] =0
estadd scalar p_valuegammadelta = r(p)
//some tests for SC
estadd scalar diffmktbdmfull = _b[mkt] + _b[SCXmkt]
test _b[mkt] + _b[SCXmkt]=0 // diff between Mkt and BDM under Full Info and SC
estadd scalar p_valuedifffull = r(p)
estadd scalar diffmktbdmPI = _b[mkt] + _b[PIXmkt] + _b[SCXmkt] + _b[SCXPIXmkt]
test _b[mkt] + _b[PIXmkt] + _b[SCXmkt] + _b[SCXPIXmkt]=0 // diff between Mkt and BDM under PI and SC
estadd scalar p_valuediffPI = r(p)  
restore



////////////   TABLE D.4   ////////////
preserve
drop if individual != 1   // since we need only one observation per group

eststo clear
xtreg marketpriceH PI mkt PIXmkt SC SCXPI SCXmkt SCXPIXmkt, re vce(robust)
estadd scalar Obs = e(N)
estadd scalar betadelta = _b[PI] + _b[PIXmkt]
test _b[PI] + _b[PIXmkt] =0
estadd scalar p_valuebetadelta = r(p)
testnl abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]) = 0  //  nlcom gives the same result
local sign_coeff = sign(abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]))
estadd scalar p_valueabsolute = 1-normal(`sign_coeff'*sqrt(r(chi2)))
estadd scalar gammadelta = _b[mkt] + _b[PIXmkt]
test _b[mkt] + _b[PIXmkt] =0
estadd scalar p_valuegammadelta = r(p)
//some tests for SC
estadd scalar diffmktbdmfull = _b[mkt] + _b[SCXmkt]
test _b[mkt] + _b[SCXmkt]=0 // diff between Mkt and BDM under Full Info and SC
estadd scalar p_valuedifffull = r(p)
estadd scalar diffmktbdmPI = _b[mkt] + _b[PIXmkt] + _b[SCXmkt] + _b[SCXPIXmkt]
test _b[mkt] + _b[PIXmkt] + _b[SCXmkt] + _b[SCXPIXmkt]=0 // diff between Mkt and BDM under PI and SC
estadd scalar p_valuediffPI = r(p)

xtreg marketpriceH PI mkt PIXmkt SC SCXPI SCXmkt SCXPIXmkt rho rhoXmkt SCXrho SCXrhoXmkt, re vce(robust)
estadd scalar Obs = e(N)
estadd scalar betadelta = _b[PI] + _b[PIXmkt]
test _b[PI] + _b[PIXmkt] =0
estadd scalar p_valuebetadelta = r(p)
testnl abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]) = 0  //  nlcom gives the same result
local sign_coeff = sign(abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]))
estadd scalar p_valueabsolute = 1-normal(`sign_coeff'*sqrt(r(chi2)))
estadd scalar gammadelta = _b[mkt] + _b[PIXmkt]
test _b[mkt] + _b[PIXmkt] =0
estadd scalar p_valuegammadelta = r(p)
//some tests for SC
estadd scalar diffmktbdmfull = _b[mkt] + _b[SCXmkt]
test _b[mkt] + _b[SCXmkt]=0 // diff between Mkt and BDM under Full Info and SC
estadd scalar p_valuedifffull = r(p)
estadd scalar diffmktbdmPI = _b[mkt] + _b[PIXmkt] + _b[SCXmkt] + _b[SCXPIXmkt]
test _b[mkt] + _b[PIXmkt] + _b[SCXmkt] + _b[SCXPIXmkt]=0 // diff between Mkt and BDM under PI and SC
estadd scalar p_valuediffPI = r(p)

xtreg marketpriceH PI mkt PIXmkt SC SCXPI SCXmkt SCXPIXmkt rho rhoXmkt SCXrho SCXrhoXmkt rep1 rep2, re vce(robust)
estadd scalar Obs = e(N)
estadd scalar betadelta = _b[PI] + _b[PIXmkt]
test _b[PI] + _b[PIXmkt] =0
estadd scalar p_valuebetadelta = r(p)
testnl abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]) = 0  //  nlcom gives the same result
local sign_coeff = sign(abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]))
estadd scalar p_valueabsolute = 1-normal(`sign_coeff'*sqrt(r(chi2)))
estadd scalar gammadelta = _b[mkt] + _b[PIXmkt]
test _b[mkt] + _b[PIXmkt] =0
estadd scalar p_valuegammadelta = r(p)
//some tests for SC
estadd scalar diffmktbdmfull = _b[mkt] + _b[SCXmkt]
test _b[mkt] + _b[SCXmkt]=0 // diff between Mkt and BDM under Full Info and SC
estadd scalar p_valuedifffull = r(p)
estadd scalar diffmktbdmPI = _b[mkt] + _b[PIXmkt] + _b[SCXmkt] + _b[SCXPIXmkt]
test _b[mkt] + _b[PIXmkt] + _b[SCXmkt] + _b[SCXPIXmkt]=0 // diff between Mkt and BDM under PI and SC
estadd scalar p_valuediffPI = r(p)

xtreg marketpriceL PI mkt PIXmkt SC SCXPI SCXmkt SCXPIXmkt, re vce(robust)
estadd scalar Obs = e(N)
estadd scalar betadelta = _b[PI] + _b[PIXmkt]
test _b[PI] + _b[PIXmkt] =0
estadd scalar p_valuebetadelta = r(p)
testnl abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]) = 0  //  nlcom gives the same result
local sign_coeff = sign(abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]))
estadd scalar p_valueabsolute = 1-normal(`sign_coeff'*sqrt(r(chi2)))
estadd scalar gammadelta = _b[mkt] + _b[PIXmkt]
test _b[mkt] + _b[PIXmkt] =0
estadd scalar p_valuegammadelta = r(p)
//some tests for SC
estadd scalar diffmktbdmfull = _b[mkt] + _b[SCXmkt]
test _b[mkt] + _b[SCXmkt]=0 // diff between Mkt and BDM under Full Info and SC
estadd scalar p_valuedifffull = r(p)
estadd scalar diffmktbdmPI = _b[mkt] + _b[PIXmkt] + _b[SCXmkt] + _b[SCXPIXmkt]
test _b[mkt] + _b[PIXmkt] + _b[SCXmkt] + _b[SCXPIXmkt]=0 // diff between Mkt and BDM under PI and SC
estadd scalar p_valuediffPI = r(p) 

xtreg marketpriceL PI mkt PIXmkt SC SCXPI SCXmkt SCXPIXmkt rho rhoXmkt SCXrho SCXrhoXmkt, re vce(robust)
estadd scalar Obs = e(N)
estadd scalar betadelta = _b[PI] + _b[PIXmkt]
test _b[PI] + _b[PIXmkt] =0
estadd scalar p_valuebetadelta = r(p)
testnl abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]) = 0  //  nlcom gives the same result
local sign_coeff = sign(abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]))
estadd scalar p_valueabsolute = 1-normal(`sign_coeff'*sqrt(r(chi2)))
estadd scalar gammadelta = _b[mkt] + _b[PIXmkt]
test _b[mkt] + _b[PIXmkt] =0
estadd scalar p_valuegammadelta = r(p) 
//some tests for SC
estadd scalar diffmktbdmfull = _b[mkt] + _b[SCXmkt]
test _b[mkt] + _b[SCXmkt]=0 // diff between Mkt and BDM under Full Info and SC
estadd scalar p_valuedifffull = r(p)
estadd scalar diffmktbdmPI = _b[mkt] + _b[PIXmkt] + _b[SCXmkt] + _b[SCXPIXmkt]
test _b[mkt] + _b[PIXmkt] + _b[SCXmkt] + _b[SCXPIXmkt]=0 // diff between Mkt and BDM under PI and SC
estadd scalar p_valuediffPI = r(p)

xtreg marketpriceL PI mkt PIXmkt SC SCXPI SCXmkt SCXPIXmkt rho rhoXmkt SCXrho SCXrhoXmkt rep1 rep2, re vce(robust)
estadd scalar Obs = e(N)
estadd scalar betadelta = _b[PI] + _b[PIXmkt]
test _b[PI] + _b[PIXmkt] =0
estadd scalar p_valuebetadelta = r(p)
testnl abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]) = 0  //  nlcom gives the same result
local sign_coeff = sign(abs(_b[PI] + _b[PIXmkt]) - abs(_b[PI]))
estadd scalar p_valueabsolute = 1-normal(`sign_coeff'*sqrt(r(chi2)))
estadd scalar gammadelta = _b[mkt] + _b[PIXmkt]
test _b[mkt] + _b[PIXmkt] =0
estadd scalar p_valuegammadelta = r(p)
//some tests for SC
estadd scalar diffmktbdmfull = _b[mkt] + _b[SCXmkt]
test _b[mkt] + _b[SCXmkt]=0 // diff between Mkt and BDM under Full Info and SC
estadd scalar p_valuedifffull = r(p)
estadd scalar diffmktbdmPI = _b[mkt] + _b[PIXmkt] + _b[SCXmkt] + _b[SCXPIXmkt]
test _b[mkt] + _b[PIXmkt] + _b[SCXmkt] + _b[SCXPIXmkt]=0 // diff between Mkt and BDM under PI and SC
estadd scalar p_valuediffPI = r(p) 
restore

////////////   TABLE D.5   ////////////
// ---------------- number of trades --------------------
preserve
drop if individual != 1
sum marketquantityH marketquantityL if mkt == 0 & PI == 0
sum marketquantityH marketquantityL if mkt == 1 & PI == 0
sum marketquantityH marketquantityL if mkt == 0 & PI == 1
sum marketquantityH marketquantityL if mkt == 1 & PI == 1
ttest marketquantityH if PI == 0, by(mkt)
ttest marketquantityH if PI == 1, by(mkt)
ttest marketquantityL if PI == 0, by(mkt)
ttest marketquantityL if PI == 1, by(mkt)
restore


****************************************
*************  APPENDIX E  *************
****************************************

////////////   FIGURE E.1   ////////////
twoway (scatter avgpH avgpL period if PI == 0, msymbol(i i) mc(gs1 gs9) msiz(vsmall vsmall) sort c(l l) lpattern(solid solid) lw(medthick   medthick ) lc(gs1 gs9)) ///
	   (scatter avgpH avgpL period if PI == 1, msymbol(o x) mc(gs1 gs9) msiz(vsmall vsmall) sort c(l l) lpattern(shortdash shortdash) lw(medthin medthin) lc(gs1 gs9)), ///
		by(Institution, graphregion(color(white)) bgcolor(white)) ///
		ysc(r(60 110)) ylab(60(10)110, notick labs(vsmall)) ///
		legend(order(1 "Price H, Public Info" 2 "Price L, Public Info" 3 "Price H, Private Info" 4 "Price L, Private Info")) ///
		xlab(0(5)30, notick labs(vsmall)) ///   //axes
		xli(10,lp(dash)lc(gs9) lw(thin)) xli(20,lp(dash)lc(gs9)lw(thin)) ///      // repetitions
		yti("Average asset prices") name(Figure_E_1, replace)
		

////////////   FIGURE E.2   ////////////
// All price sensitive
*************************
* ASSET H   FULL
*************************
* select +- range from previous prices
local step = 2.5

preserve
drop if mkt == 0 | PI == 1
* keep price sensitive only
drop if sensitive == 0

sort id period
// generate lagged mkt price
gen L_mktpriceH = L1.marketpriceH if period != 1 & period != 11 & period != 21

// creating a simulated price for each leading period
forvalues lead = 1(10)21 {
bysort period: egen avgasksensH = mean(askpriceH) if period == `lead'
bysort period: egen avgbidsensH = mean(bidpriceH) if period == `lead'
gen simulpriceH`lead' = (avgasksensH + avgbidsensH) / 2  if period == `lead'
//copy the simulpriceH1 to all periods 
levelsof simulpriceH`lead'
	foreach p in `r(levels)'{     // there is only one value
		replace simulpriceH`lead' = `p'
	}
drop avgasksensH avgbidsensH
}



/* generate bids and asks of subjects with lagged market price within the range
of the lagged simulated price. Bids and asks of periods 1,11,21 are excluded since
the lagged mkt price (L_mktpriceH) is missing in these periods*/
gen indicator = 1 if period != 1 & period != 11 & period != 21
forvalues period = 2/30 {
	if (`period' != 11 & `period' != 21){
	di `period'
	local before = `period' - 1
	sort id period
	levelsof simulpriceH`before'
	scalar lowerbound = `r(levels)'-`step'
	scalar upperbound = `r(levels)'+`step'
	egen avgasksensH = mean(askpriceH) if (L_mktpriceH >= lowerbound & L_mktpriceH <= upperbound)
	egen avgbidsensH = mean(bidpriceH) if (L_mktpriceH >= lowerbound & L_mktpriceH <= upperbound)
	gen simulpriceH`period' = (avgasksensH + avgbidsensH) / 2
	sum simulpriceH`period'
	// copy simul price to the rest of subjects
	levelsof simulpriceH`period'
	foreach p in `r(levels)'{     // there is only one value
	replace simulpriceH`period' = `p'
	scalar drop lowerbound upperbound
	drop avgasksensH avgbidsensH  // this also returns error if there are no observations
}
}
}

* merge all simulated prices in one variable
gen pricesensH = .
forvalues period = 1/30{
	replace pricesensH = simulpriceH`period' if period == `period'
}
drop simulpriceH* indicator


scatter pricesensH period, msymbol(i) mc(gs1) msiz(vsmall) sort c(l) lpattern(solid) lw(medthick ) lc(gs1) ///
		xlab(0(5)30, notick labs(vsmall)) ///   //axes
		xli(10,lp(dash)lc(gs9) lw(thin)) xli(20,lp(dash)lc(gs9)lw(thin)) ///      // repetitions
		graphregion(color(white)) bgcolor(white) ///
	    ysc(r(50 120)) ylab(50(10)120, notick labs(vsmall)) ///		
		name(Pricessens, replace)

restore


*************************
* ASSET H   PI
*************************
preserve
drop if mkt == 0 | PI == 0
* keep price sensitive only
drop if sensitive == 0

sort id period
// generate lagged mkt price
gen L_mktpriceH = L1.marketpriceH if period != 1 & period != 11 & period != 21

// creating a simulated price for each leading period
forvalues lead = 1(10)21 {
bysort period: egen avgasksensH = mean(askpriceH) if period == `lead'
bysort period: egen avgbidsensH = mean(bidpriceH) if period == `lead'
gen simulpriceH`lead' = (avgasksensH + avgbidsensH) / 2  if period == `lead'
//copy the simulpriceH1 to all periods 
levelsof simulpriceH`lead'
	foreach p in `r(levels)'{     // there is only one value
		replace simulpriceH`lead' = `p'
	}
drop avgasksensH avgbidsensH
}


/* generate bids and asks of subjects with lagged market price within the range
of the lagged simulated price. Bids and asks of periods 1,11,21 are excluded since
the lagged mkt price (L_mktpriceH) is missing in these periods*/
gen indicator = 1 if period != 1 & period != 11 & period != 21
forvalues period = 2/30 {
	if (`period' != 11 & `period' != 21){
	di `period'
	local before = `period' - 1
	sort id period
	levelsof simulpriceH`before'
	scalar lowerbound = `r(levels)'-`step'
	scalar upperbound = `r(levels)'+`step'
	egen avgasksensH = mean(askpriceH) if (L_mktpriceH >= lowerbound & L_mktpriceH <= upperbound)
	egen avgbidsensH = mean(bidpriceH) if (L_mktpriceH >= lowerbound & L_mktpriceH <= upperbound)
	gen simulpriceH`period' = (avgasksensH + avgbidsensH) / 2
	sum simulpriceH`period'
	// copy simul price to the rest of subjects
	levelsof simulpriceH`period'
	foreach p in `r(levels)'{     // there is only one value
	replace simulpriceH`period' = `p'
	scalar drop lowerbound upperbound
	drop avgasksensH avgbidsensH  // this also returns error if there are no observations
}
}
}

* merge all simulated prices in one variable
gen pricesensH = .
forvalues period = 1/30{
	replace pricesensH = simulpriceH`period' if period == `period'
}
drop simulpriceH* indicator

addplot Pricessens: scatter pricesensH period, msymbol(o) mc(gs1) msiz(small) sort c(l) lpattern(shortdash) lw(medthick) lc(gs1) ///
		xlab(0(5)30, notick labs(vsmall)) ///   //axes
		xli(10,lp(dash)lc(gs9) lw(thin)) xli(20,lp(dash)lc(gs9)lw(thin)) ///     // repetitions
		graphregion(color(white)) bgcolor(white)  ///
		legend(off) ///
		ylab(50(10)120, notick labs(vsmall)) ///
		yti("")
restore


//use revision_2, clear
*************************
* ASSET L   FULL
*************************
* select +- range from previous prices

preserve
drop if mkt == 0 | PI == 1
* keep price sensitive only
drop if sensitive == 0

sort id period
// generate lagged mkt price
gen L_mktpriceL = L1.marketpriceL if period != 1 & period != 11 & period != 21

// creating a simulated price for each leading period
forvalues lead = 1(10)21 {
bysort period: egen avgasksensL = mean(askpriceL) if period == `lead'
bysort period: egen avgbidsensL = mean(bidpriceL) if period == `lead'
gen simulpriceL`lead' = (avgasksensL + avgbidsensL) / 2  if period == `lead'
//copy the simulpriceL1 to all periods 
levelsof simulpriceL`lead'
	foreach p in `r(levels)'{     // there is only one value
		replace simulpriceL`lead' = `p'
	}
drop avgasksensL avgbidsensL
}



/* generate bids and asks of subjects with lagged market price within the range
of the lagged simulated price. Bids and asks of periods 1,11,21 are excluded since
the lagged mkt price (L_mktpriceL) is missing in these periods*/
gen indicator = 1 if period != 1 & period != 11 & period != 21
forvalues period = 2/30 {
	if (`period' != 11 & `period' != 21){
	di `period'
	local before = `period' - 1
	sort id period
	levelsof simulpriceL`before'
	scalar lowerbound = `r(levels)'-`step'
	scalar upperbound = `r(levels)'+`step'
	egen avgasksensL = mean(askpriceL) if (L_mktpriceL >= lowerbound & L_mktpriceL <= upperbound)
	egen avgbidsensL = mean(bidpriceL) if (L_mktpriceL >= lowerbound & L_mktpriceL <= upperbound)
	gen simulpriceL`period' = (avgasksensL + avgbidsensL) / 2
	sum simulpriceL`period'
	// copy simul price to the rest of subjects
	levelsof simulpriceL`period'
	foreach p in `r(levels)'{     // there is only one value
	replace simulpriceL`period' = `p'
	scalar drop lowerbound upperbound
	drop avgasksensL avgbidsensL  // this also returns error if there are no observations
}
}
}
* merge all simulated prices in one variable
gen pricesensL = .
forvalues period = 1/30{
	replace pricesensL = simulpriceL`period' if period == `period'
}
drop simulpriceL* indicator
addplot Pricessens: scatter pricesensL period, msymbol(i) mc(gs9) msiz(vsmall) sort c(l) lpattern(solid) lw(medthick ) lc(gs9) ///
		xlab(0(5)30, notick labs(vsmall)) ///   //axes
		xli(10,lp(dash)lc(gs9) lw(thin)) xli(20,lp(dash)lc(gs9)lw(thin)) ///      // repetitions
		graphregion(color(white)) bgcolor(white) ///
	    ysc(r(50 120)) ylab(50(10)120, notick labs(vsmall)) ///		
		yti("")
restore

*************************
* ASSET L   PI
*************************
preserve
drop if mkt == 0 | PI == 0

* keep price sensitive only
drop if sensitive == 0

sort id period
// generate lagged mkt price
gen L_mktpriceL = L1.marketpriceL if period != 1 & period != 11 & period != 21

// creating a simulated price for each leading period
forvalues lead = 1(10)21 {
bysort period: egen avgasksensL = mean(askpriceL) if period == `lead'
bysort period: egen avgbidsensL = mean(bidpriceL) if period == `lead'
gen simulpriceL`lead' = (avgasksensL + avgbidsensL) / 2  if period == `lead'
//copy the simulpriceL1 to all periods 
levelsof simulpriceL`lead'
	foreach p in `r(levels)'{     // there is only one value
		replace simulpriceL`lead' = `p'
	}
drop avgasksensL avgbidsensL
}

/* generate bids and asks of subjects with lagged market price within the range
of the lagged simulated price. Bids and asks of periods 1,11,21 are excluded since
the lagged mkt price (L_mktpriceL) is missing in these periods*/
gen indicator = 1 if period != 1 & period != 11 & period != 21
forvalues period = 2/30 {
	if (`period' != 11 & `period' != 21){
	di `period'
	local before = `period' - 1
	sort id period
	levelsof simulpriceL`before'
	scalar lowerbound = `r(levels)'-`step'
	scalar upperbound = `r(levels)'+`step'
	egen avgasksensL = mean(askpriceL) if (L_mktpriceL >= lowerbound & L_mktpriceL <= upperbound)
	egen avgbidsensL = mean(bidpriceL) if (L_mktpriceL >= lowerbound & L_mktpriceL <= upperbound)
	gen simulpriceL`period' = (avgasksensL + avgbidsensL) / 2
	sum simulpriceL`period'
	// copy simul price to the rest of subjects
	levelsof simulpriceL`period'
	foreach p in `r(levels)'{     // there is only one value
	replace simulpriceL`period' = `p'
	scalar drop lowerbound upperbound
	drop avgasksensL avgbidsensL  // this also returns error if there are no observations
}
}
}
* merge all simulated prices in one variable
gen pricesensL = .
forvalues period = 1/30{
	replace pricesensL = simulpriceL`period' if period == `period'
}
drop simulpriceL* indicator

addplot Pricessens: scatter pricesensL period, msymbol(o) mc(gs9) msiz(small) sort c(l) lpattern(shortdash) lw(medthick) lc(gs9) ///
		xlab(0(5)30, notick labs(vsmall)) ///   //axes
		xli(10,lp(dash)lc(gs9) lw(thin)) xli(20,lp(dash)lc(gs9)lw(thin)) ///     // repetitions
		graphregion(color(white)) bgcolor(white)  ///
		legend(off) ///
		ylab(50(10)120, notick labs(vsmall)) ///
		sub("Price Sensitive") yti("")
restore
		

*** WHOLE SAMPLE ***
preserve
drop if individual != 1
drop if mkt  == 0
bysort PI period: egen avgpHH = mean(marketpriceH)
bysort PI period: egen avgpLL = mean(marketpriceL)
twoway (scatter avgpHH avgpLL period if PI == 0, msymbol(i i) mc(gs1 gs9) msiz(vsmall vsmall) sort c(l l) lpattern(solid solid) lw(medthick   medthick ) lc(gs1 gs9)) ///
	   (scatter avgpHH avgpLL period if PI == 1, msymbol(o o) mc(gs1 gs9) msiz(small small) sort c(l l) lpattern(shortdash shortdash) lw(medthin medthin) lc(gs1 gs9)), ///
		ysc(r(50 120)) ylab(50(10)120, notick labs(vsmall)) ///
		xlab(0(5)30, notick labs(vsmall)) ///   //axes
		xli(10,lp(dash)lc(gs9) lw(thin)) xli(20,lp(dash)lc(gs9)lw(thin)) ///      // repetitions
		graphregion(color(white)) bgcolor(white) ///
		legend(off) ///
		sub("Full Sample") ///
		yti("Average asset prices") name(Pricesmkt, replace)
restore
			
graph combine Pricesmkt Pricessens, ycommon xcommon graphregion(color(white))
