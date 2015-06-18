/*
Author: Eddy Malesky (For "Monopoly Money" paper)
Anh Le's modifying it to clean the data for his prospectus
*/

set mem 500m
set more off
cap cd "/home/anh/projects/prospectus/data/PCI"

/*Sort ISIC Revision 4 Codes for Use in Analysis*/

/*Domestic Data*/
use isic_2010_dom.dta, clear
sort id
save isic_2010_dom.dta, replace

use isic_2011_dom.dta, clear
sort id
save isic_2011_dom.dta, replace

use isic_2012_dom.dta, clear
sort id
save isic_2012_dom.dta, replace

/*Foreign Data*/
use isic_2010_fdi.dta, clear
sort id
save isic_2010_fdi.dta, replace

use isic_2011_fdi.dta, clear
drop if id == ""
sort id
save isic_2011_fdi.dta, replace

use isic_2012_fdi.dta, clear
sort id
save isic_2012_fdi.dta, replace

/*Recode Previous Iterations of PCI Data, so that ALL Years Share The Same Code*/

global PCI_KEEP_VARS pci_id id* form ///
	a1 a2 a4 a5* a6_1 a7_1 a7_2 a7_3 a8_1 a8_2 a8_3 a9 a10 ///
	c1 c3 c4_2010 c6_2010 d14_2010 ///
	a11* ///
	a13* a14* ownland b3 b4 d1 d6 d10 d11 d12 d14_2 d14_3 ///
	h3* h4* h5

use PCI2010.dta, clear
keep $PCI_KEEP_VARS
drop a11_c_new
lab var a11_c "% of firm sales sold to private individuals or firms"
lab var h3 "Do you agree with the consideration “It is more prior that provinces attract for"
lab val h3 h_opinion
generate FDI=0
generate year=2010
sort id 
merge 1:1 id using isic_2010_dom.dta
order id pci_id form FDI year
save rents2010_dom.dta, replace


use PCI2011.dta, clear
keep $PCI_KEEP_VARS
drop if id == "ID" /* 1 miscoded row */
generate FDI=0
generate year=2011
order id pci_id form FDI year
sort id 
merge 1:1 id using isic_2011_dom.dta
drop if _m == 2
order id pci_id form FDI year
save rents2011_dom.dta, replace


use PCI2012.dta, clear
keep $PCI_KEEP_VARS
generate FDI=0
generate year=2012
sort id 
merge 1:1 id using isic_2012_dom.dta
drop if _m == 2
order id pci_id form FDI year
save rents2012_dom.dta, replace

global FDI_KEEP_VARS id* form pci_id companycountry managercountry  position ///
	a1 a1_1 a3_1 a4 a5* a6* a8 a9* a10* a11* a13 ///
	a15* ///
	b8* c1_1 c2 c3 c6 f9 e9 d2 d4 e1 e3 e5 e6 e7 ///
	j1 j3

use FDI2010.dta, clear
keep $FDI_KEEP_VARS
drop if id == "" /* These rows are all empty somehow */
label define j3 1 "Strongly agree" 2 "Agree" 3 "Disagree" 4 "Strongly disagree", replace
label values j3 j3
/*replace c6=0 if c6==.|c6==.b*/
generate FDI=1
generate year=2010
sort id 
merge 1:1 id using isic_2010_fdi.dta
order id pci_id form FDI year
save rents2010_fdi.dta, replace


use FDI2011.dta, clear
keep $FDI_KEEP_VARS
label variable j3 "make opinion on: The provincial authorities favor state owned enterprises in gov"
label values j3 j3
drop if id == "" /* These rows are all empty somehow */
generate FDI=1
generate year=2011
order id pci_id form FDI year
sort id
merge 1:1 id using isic_2011_fdi.dta
drop if _m == 2 /* Drop using, keep master, so we have the full corruption measurement */
order id pci_id form FDI year
save rents2011_fdi.dta, replace


use FDI2012.dta, clear
keep $FDI_KEEP_VARS
generate FDI=1
generate year=2012

* Could have use recode here instead. Just recode form ("A"="B") ("B"="A")
generate form2="B" if form=="A" & year==2012
generate form3="A" if form=="B" & year==2012
drop form
generate form=form2
replace form=form3 if form2==""
drop form2 form3


sort id 
merge 1:1 id using isic_2012_fdi.dta
order id pci_id form FDI year 
save rents2012_fdi.dta, replace


/*****************************************Stack Domestic Data and Re-Caode Variable Names for Consistency******************************/
/*Stack*/
use rents2010_dom.dta, clear
append using rents2011_dom.dta
append using rents2012_dom.dta

/*Recode*/
* drop if year==. * Anh fixed this by better merging. 
* The missing values intially come from some id == "" in both isic and PCI datasets
rename a1 est_year
rename a2 reg_year
* replace reg_year =1990 if reg_year<=1990
* generate time=2012-reg_year
* generate time_sq=time^2
rename a5_1 manufacturing
rename a5_2 construction
rename a5_3 services
rename a5_4 agriculture
rename a5_5 mining
rename a6_1 product

* Equity est = equity this year and last year
* replace a7_1 =a7_2 if a7_1==.|a7_1==.a|a7_1==.b
* replace a7_1 =a7_3 if a7_1==.|a7_1==.a|a7_1==.b

rename a7_1 equity_est
rename a7_2 equity_lastyear
rename a7_3 equity_thisyear

* Labor est = labor this year / last year
* replace a8_1 =a8_2 if a8_1==.|a8_1==.a|a8_1==.b
* replace a8_1 =a8_3 if a8_1==.|a8_1==.a|a8_1==.b

rename a4 ownership_type
rename a8_1 labor_est
rename a8_2 labor_lastyear
rename a8_3 labor_thisyear
rename a9 performance
rename a10 expand

rename a11_a pctsale_soe
rename a11_b pctsale_state
rename a11_c pctsale_private
recode pctsale_private -99=.
rename a11_d pctsale_foreign
rename a11_e pctsale_export
rename a11_f pctsale_exportind

rename a13_1 lsoe
rename a13_2 csoe
rename a13_3 equity_nn
rename a13_4 household
rename a13_5 HOSE
rename a14_1 university
rename a14_2 MBA
rename a14_3 connection_gov
rename a14_4 connection_mil
rename a14_5 connection_soe_boss
rename a14_6 connection_soe_employee
drop a13_6 a14_7 /* Drop other kind of a13 background of firms, a14 soe connection */
rename b3 iz
rename c1 reg_time
rename c3 oss
rename c6_2010 reg_corrupt
rename d14_2010 proc_corrupt
rename b4 lurc

rename d1  inspections
rename d6 bureaucratic_time
rename d10 bribe_time
rename d14_2 bureaucratic_rents 
rename d11 bribe_size
rename d12 service_delivered
rename d14_3 negotiate_tax

rename h3 fdi_favoritism
rename h3_1 fdi_favoritism_landaccess
rename h3_2 fdi_favoritism_quickeradminproc
rename h3_3 fdi_favoritism_landclearance
rename h3_4 fdi_favoritism_provincialsupport
rename h3_5 fdi_favoritism_other

rename h4 soe_favoritism
rename h4_1_1 soe_favoritism_landaccess
rename h4_1_2 soe_favoritism_loanaccess
rename h4_1_3 soe_favoritism_quickeradminproc
rename h4_1_4 soe_favoritism_statecontract
rename h4_1_5 soe_favoritism_other

rename h5 connection_favoritism

sort id year
save ts_rents_domestic.dta, replace
rm rents2010_dom.dta
rm rents2011_dom.dta
rm rents2012_dom.dta

/***********************************************************Stack FDI Data and Re-Code for Consistency***********************************/
/*Stack*/
use rents2010_fdi.dta, clear
append using rents2011_fdi.dta
append using rents2012_fdi.dta

/*Recode*/
rename a1 est_year
rename a1_1 reg_year
* replace reg_year =1990 if reg_year<=1990
* generate time=2012-reg_year
* generate time_sq=time^2

rename a8 ownership_type
rename a9 mnc

* Let's not make this assumption
* replace a10_1=a10_2 if a10_1==.|a10_1==.a|a10_1==.b
* replace a10_1=a10_3 if a10_1==.|a10_1==.a|a10_1==.b
* replace a10_1=a10_4 if a10_1==.|a10_1==.a|a10_1==.b

generate labor_est=1 if a10_1<5
replace labor_est=2 if a10_1>5 & a10_1<10
replace labor_est=3 if a10_1>10 & a10_1<50
replace labor_est=4 if a10_1>50 & a10_1<200
replace labor_est=5 if a10_1>200 & a10_1<300
replace labor_est=6 if a10_1>300 & a10_1<5000
replace labor_est=7 if a10_1>500 & a10_1<1000
replace labor_est=8 if a10_1>1000 & a10_1 !=. & a10_1 !=.a & a10_1!=.b
replace labor_est=a10_1 if year==2012

* Let's not make this assumption 
/*
replace a4=a5_1 if a4==.|a4==.a|a4==.b
replace a4=a5_2 if a4==.|a4==.a|a4==.b
replace a4=a5_3 if a4==.|a4==.a|a4==.b
replace a5_1=a5_2 if a4==.|a4==.a|a4==.b
replace a5_2=a5_3 if a4==.|a4==.a|a4==.b 
*/

sum  a4, detail
generate equity_est=1 if a4<25000
replace equity_est=2 if a4>=25000 & a4<50000
replace equity_est=3 if a4>=50000 & a4<250000
replace equity_est=4 if a4>=250000 & a4<500000
replace equity_est=5 if a4>=500000 & a4<2500000
replace equity_est=6 if a4>=2500000 & a4<10000000
replace equity_est=7 if a4>=10000000 & a4<25000000
replace equity_est=8 if a4>=25000000 & a4!=. & a4 !=.a & a4 !=.b
replace equity_est=a5_1 if year==2012 /* a5_1 is coded as equity at est in 2012 */ 


rename a6_1 manufacturing
rename a6_2 construction
rename a6_3 services
rename a6_4 agriculture
rename a6_5 mining
rename a6_6 finance

rename a11_4 performance
rename a13 expand

rename a15_a pctsale_soe
rename a15_b pctsale_state
rename a15_c pctsale_private
rename a15_d pctsale_foreign
rename a15_e pctsale_export
rename a15_f pctsale_exportind

rename d2 iz
rename c1_1 reg_time
rename c2 oss

rename e1 inspections
rename e3 bureaucratic_time
rename e5 bureaucratic_rents 
rename e6 bribe_size
rename e7 service_delivered


rename c6 reg_corrupt
rename e9 proc_corrupt
rename d4 lurc

rename j1 fdi_attitude
recode fdi_attitude -99 = .
rename j3 soe_favoritism
recode soe_favoritism -99=.

sort id year
save ts_rents_fdi.dta, replace
rm rents2010_fdi.dta
rm rents2011_fdi.dta
rm rents2012_fdi.dta

/**********************************************************MERGE FDI AND DOMESTIC DATA******************************/

use ts_rents_domestic.dta, clear
append using ts_rents_fdi.dta
save ts_rents_combined.dta, replace
rm ts_rents_domestic.dta
rm ts_rents_fdi.dta

* Fill in the pctsale
egen temp=rowtotal( pctsale_export pctsale_exportind pctsale_foreign pctsale_private pctsale_soe pctsale_state)
foreach var of varlist  pctsale_export pctsale_exportind pctsale_foreign pctsale_private pctsale_soe pctsale_state {
	 replace `var' = 0 if temp ==100 & missing(`var')
	 * Assume that > 95 and < 105 is meant to be 100
	 replace `var' = 0 if temp > 95 & temp < 105 & !missing(temp) & missing(`var')
}

generate treatment=1 if form=="A" & FDI==0
replace treatment=0 if form=="B" & FDI==0
replace treatment=1 if form=="B" & FDI==1
replace treatment=0 if form=="A" & FDI==1

generate SOE=1 if lsoe==1
replace SOE=1 if csoe==1
replace SOE=0 if SOE==.
generate WTO=1 if reg_year>=2007 & reg_year!=.
replace WTO=0 if reg_year<2007
generate connection= connection_gov+ connection_mil+ connection_soe_boss + connection_soe_employee
* replace connection=0 if FDI==1
generate connection_dich=1 if connection>0 & connection !=.
replace connection_dich=0 if connection<=0

generate form2=1 if form=="A"
replace form2=0 if form=="B"

drop _merge
sort pci_id year
merge m:1 pci_id year using 7yr_timeseries_v3.dta

/*****************************ISIC Codes*************************/
split isic, parse(/) limit(3)
destring isic1, generate(isic4) force

replace  isic4=2500 if manufacturing==1 &  isic4==.
replace  isic4=4600 if services==1 &  isic4==.
replace  isic4=100 if agriculture==1 &  isic4==.
replace  isic4=500 if mining==1 &  isic4==.
replace  isic4=4100 if construction==1 &  isic4==.
replace  isic4=6400 if finance==1 &  isic4==.
replace  isic4=4900 if  isic4==49
replace  isic4=5000 if isic4==50

replace isic4=4100 if isic=="F"
replace isic4=4100 if isic=="41-"
replace isic4=4500 if isic=="G"
replace isic4=4900 if isic=="H"
replace isic4=9400 if isic=="S"


replace isic4=isic4*1000 if isic4<10 & agriculture==0 & mining==0
replace isic4=isic4*100 if isic4<100 & isic4>=10 & agriculture==0 & mining==0
replace isic4=isic4*10 if isic4<1000 & isic4>=100 & agriculture==0 & mining==0
replace isic4=isic4*100 if isic4<10 & agriculture==1 
replace isic4=isic4*100 if isic4<10 & mining==1 
replace isic4=isic4*10 if isic4<100 & isic4>=10 & agriculture==1
replace isic4=isic4*10 if isic4<100 & isic4>=10 & mining==1

#delimit;
drop isic1 isic2 isic3;
generate zero=0 if isic4<1000;
tostring zero, replace;
tostring isic4, replace;
egen isic4_extra = concat(zero isic4) if zero=="0";

#delimit;
replace isic4=isic4_extra if zero=="0";
drop isic4_extra;
drop zero;


#delimit;
order isic4;
generate isic_dig1=regexs(1) if regexm(isic4, "([0-9])([0-9])([0-9])([0-9])");
generate isic_dig2=regexs(2) if regexm(isic4, "([0-9])([0-9])([0-9])([0-9])");
generate isic_dig3=regexs(3) if regexm(isic4, "([0-9])([0-9])([0-9])([0-9])");
generate isic_dig4=regexs(4) if regexm(isic4, "([0-9])([0-9])([0-9])([0-9])");
egen isic_rev4_2digitcode = concat(isic_dig1 isic_dig2);
lab var isic_rev4_2digitcode "ISIC Revision 4 2 Digit Code";
egen isic_rev4_4digit = concat(isic_dig1 isic_dig2 isic_dig3 isic_dig4);
lab var isic_rev4_4digit "ISIC Revision 4 4 Digit Code";
sort isic_rev4_4digit;




#delimit;
destring isic_rev4_4digit, generate(isic4_no) force;

#delimit;
cap cd "C:\Users\ejm5\Dropbox\Rents_Asunder\AJPS_ReplicationFiles\do\";
cap cd "/Users/dimitargueorguiev/Dropbox/Rents_Asunder/AJPS_ReplicationFiles/do/";
cap cd "/home/anh/projects/prospectus/code";
do "04_clean_pci_panel_restricted_FIL_only.do";

#delimit;
replace restrict=. if reg_year==.;
replace restrict=. if reg_year==.b;

/*
#delimit cr
/*New Variables*/
generate treatment_FDI=treatment*FDI
generate treatment_restrict=treatment*restrict
generate FDI_res=FDI*restrict
generate FDI_res_treatment=FDI*restrict*treatment
generate y2011=1 if year==2011
replace y2011=0 if year!=2011
generate y2012=1 if year==2012
replace y2012=0 if year!=2012

/*Connections*/

set more off
generate FDI_res_all=FDI*restrict_all
generate connect_restrict=connection_dich*restrict_all
generate soe_restrict=SOE*restrict_all
generate lsoe_restrict=lsoe*restrict_all
generate csoe_restrict=csoe*restrict_all
generate gov_restrict=connection_gov*restrict_all
generate mil_restrict=connection_mil*restrict_all
generate boss_restrict=connection_soe_b*restrict_all
generate empl_restrict=connection_soe_e*restrict_all
replace connection_soe_b=0 if FDI==1
g restrict_connect=restrict*connection_dich 
g restrict_all_connect=restrict_all*connection_dich 

g restrict_connect_gov=restrict*connection_gov 
g restrict_all_connect_gov=restrict_all*connection_gov 

g restrict_connect_mil=restrict*connection_mil 
g restrict_all_connect_mil=restrict_all*connection_mil 

g restrict_connect_soe=restrict*connection_soe_e 
g restrict_all_connect_soe=restrict_all*connection_soe_e 

g restrict_equitized=restrict*SOE 
g restrict_all_equitized=restrict_all*SOE

g restrict_equity=restrict*equity_nn
g restrict_all_equity=restrict_all*equity_nn

g restrict_connect_soe_b=restrict*connection_soe_b 
g restrict_all_connect_soe_b=restrict_all*connection_soe_b 

**Combine Employee and Boss connections
g connection_soe=0
replace connection_soe=1 if connection_soe_b==1 | connection_soe_e==1
replace restrict_connect_soe=1 if restrict_connect_soe_b ==1
replace restrict_all_connect_soe =1 if restrict_all_connect_soe_b==1
*/

#delimit cr
generate private=1 if ownership_type==1 & FDI==0
replace private=0 if private==.
generate LLC=1 if ownership_type==3 & FDI==0
replace LLC=0 if LLC==.
generate JS=1 if ownership_type==4 & FDI==0
replace JS=0 if JS==.
generate fully_owned=1 if ownership_type==1 & FDI==1
replace fully_owned=0 if fully_owned==.
generate jv=1 if ownership_type==2 & FDI==1
replace jv=1 if ownership_type==3 & FDI==1
replace jv=0 if jv==.
rename percen_asphalted_roads roads

/*
*service dummy*
gen sector2=0
replace sector2=1 if services==1
replace sector2=. if services==. 
ttest sector2 if FDI==1, by(treatment)
*/

label variable FDI "Foreign Invested Firm (Dummy)"
label variable year "Survey Year"
label variable pci_id "PCI Provincial ID"
label variable isic4 "ISIC Industry Code Version 4"
label variable ownland "Own Land Use Certificate (Dummy)"
* label variable time "Time Since Registration"
* label variable time_sq "Time Since Registration (squared)"
label variable region "Geogrpahic Region"
label variable restrict "Restricted to Foreign Investors"
label variable restrict_all "Restricted to Foreign and Domestic Investors"
label variable LLC "Limited Liability Corporation"
label variable JS "Joint Stock Enterprise"
label variable fully_owned "Fully Owned FIE"
label variable jv "Joint Venture"
label variable private "Privately Owned"
* label variable connection_soe "Owner Previously SOE Director or Manager"
label variable treatment "LIST Experiment Treatment"
label variable SOE "SOE Connection"
label variable WTO "Pre Post WTO Entry"
label variable connection "Number of Political Connections min=0 max=4"
label variable connection_dich "Political Connection (binary)"
label variable id "Company ID"
label variable equity_lastyear "Equity capital in previous year"
label variable equity_thisyear "Equity capital current year"
label variable labor_lastyear "Employment size in previous year"
label variable labor_thisyear "Employment size in current year"
label variable performance "Firm's overall performance in current year"
label variable reg_corrupt "Number of Activities at time of Registration"
label variable inspections "Number of inspections in current year"
label variable email_dich "Access to Email"
label variable pop_stacked "Population (in 1000s)"
label variable active_enterprises_stacked "number of active enterprises"
label variable lurc_monre "Total amount of land in province with a formal land use rights certificate"
label variable roads "Percentage of total asphalted roads"
label variable isic "ISIC Industry Code Version 3"
* label variable y2011 "Survey Year 2011 (dummy)"
* label variable y2012 "Survey Year 2010 (dummy)"
/*
label variable treatment_FDI "treatment*fdi (interaction)"
label variable treatment_restrict "treatment*restrict (interaction)"
label variable FDI_res "fdi*restrict (interaction)"
label variable FDI_res_treatment "fdi*restrict*treatment"
label variable FDI_res_all "fdi*restrict_all (interaction)"
label variable connect_restrict "connections_dich*restrict (interaction)"
label variable soe_restrict "SOE*restrict (interaction)"
label variable lsoe_restrict "lsoe*restict (interaction)"
label variable csoe_restrict "csoe*restrict (interaction)"
label variable gov_restrict "gov*restrict (interaction)"
label variable mil_restrict "mil*restrict (interaction)"
label variable boss_restrict "boss*restrict (interaction)"
label variable empl_restrict "empl*restrict (interaction)"
label variable restrict_connect "restrict_connection_dich (interaction)"
label variable restrict_all_connect "restrict_all_connection_dich (interaction)"
label variable restrict_connect_gov "restrict_connection_gov (interaction)"
label variable restrict_all_connect_gov "restrict_all*connection_dich (interaction)"
label variable restrict_connect "restrict*connection_dich (interaction)"
label variable restrict_all_connect "restrict_all*connection_dich (interaction)"
label variable restrict_connect_gov "restrict*connection_gov (interaction)"
label variable restrict_connect_mil "restrict*connection_mil (interaction)"
label variable restrict_all_connect_mil "restrict_all*connection_mil (interaction)"
label variable restrict_connect_soe "restrict*connection_soe (interaction)"
label variable restrict_all_connect_soe "restrict_all*connection_soe (interaction)"
label variable restrict_equitized "restrict_equitized (interaction)"
label variable restrict_all_equitized "restrict_all*equitized (interaction)"
label variable restrict_equity "restrict_all*equity (interaction)"
label variable restrict_all_equity "restrict_all*equity (interaction)"
label variable restrict_equity "restrict*equity (interaction)"
label variable restrict_connect_soe_b "restrict_connections_soe_boss (interaction)"
label variable restrict_all_connect_soe_b "restrict_connections_soe_boss (interaction)"
*/

/*
/*Merge in Sector Data - Predicted Restrictions based on SOE Share*/
#delimit;
drop _merge;
sort isic_rev4_2digitcode reg_year;
merge m:1 isic_rev4_2digitcode reg_year using "HHI_Collapse_2242013.dta", keepusing(predict_restrict2);
#delimit;
lab var predict_restrict2 "Predicted Restrictions based on Share of SOEs in Sector (See Appendix 7)";
replace predict_restrict2=predict_restrict2*10;
generate FDI_predict2=FDI*predict_restrict2;
lab var FDI_predict2 "Interaction between Predicted Restrictions and FDI";

/*Bring in 4-Digit Profit Data*/
#delimit cr
set more off
drop _merge
sort isic_rev4_4digit reg_year
merge m:1 isic_rev4_4digit reg_year using "HHI_Collapse_2242013_4dig.dta", update keepusing(ln_profit)
drop _merge 
*/

/*Drop unused variables*/
drop isic_dig* a3_1 a4 a5_1 a5_2 a5_3 mnc a9_1 a10_1 a10_2 a10_3 a10_4 a11_1 a11_2 a11_3 b8_2_1 b8_2_2  ///
form* id2006 id2010 id2011 product period

saveold "/home/anh/projects/prospectus/clean_data/pci_panel.dta", replace
