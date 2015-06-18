cap log close
set more off
cap cd "/home/anh/projects/prospectus/data/doing_business"

use "StandardizedOld-2002_2005--core2-.dta", replace

/* Keep variables */
#delimit;
keep country industry sector exporter c201 c202 c203* ///
	c211* c212* /* pctsale pctinput source */
	c216* /* Number of competitors, suppliers, customers */
	c218* /* Business constraint */
	c238 c239 c240* c242d* c242e* c243 c245* /* Corruption */
	c254 /* Using foreign licensed tech */
	c262a* /* Labor force size */
	c264 /* Foreign nations as % of perm workers */
	c281a* /* Total assets */
	c282b* /* Total liability */;

#delimit cr
ren c201 year_est

ren c202 ownership_type
ren c203a pctownership_domestic
ren c203b pctownership_foreign
ren c203c pctownership_state
ren c203d pctownership_other

ren c211a1 pctsale_domestic
ren c211a2 pctsale_export
ren c211a3 pctsale_exportind

ren c211b1 pctsale_domestic_gov
ren c211b2 pctsale_domestic_soe
ren c211b3 pctsale_domestic_fie
ren c211b4 pctsale_domestic_parent
ren c211b5 pctsale_domestic_domestic
ren c211b6 pctsale_domestic_other

ren c211c1 year_firstexport

ren c2121 pctinput_domestic
ren c2122 pctinput_import
ren c2123 pctinput_importind

ren c218o constraint_corruption
*
ren c218q constraint_anticompetitive
ren c218r constraint_legal

ren c238 regulation_time
ren c239 bribesize

ren c240b1 bribe_phone
ren c240b2 bribe_electricity
ren c240b3 bribe_water
ren c240b4 bribe_constructionpermit
ren c240b5 bribe_importlicense
ren c240b6 bribe_operatinglicense

ren c240c1 bribesize_phone
ren c240c2 bribesize_electricity
ren c240c3 bribesize_water
ren c240c4 bribesize_constructionpermit
ren c240c5 bribesize_importlicense
ren c240c6 bribesize_operatinglicense

ren c242d1 bribe_tax
ren c242d2 bribe_labor
ren c242d3 bribe_building
ren c242d4 bribe_sanitation
ren c242d5 bribe_police
ren c242d6 bribe_environment
ren c242d7 bribe_allagencies

ren c242e1 bribesize_tax
ren c242e2 bribesize_labor
ren c242e3 bribesize_building
ren c242e4 bribesize_sanitation
ren c242e5 bribesize_police
ren c242e6 bribesize_environment
ren c242e7 bribesize_allagencies

ren c243 bribesize_pct_govcontract

ren c245a corrupt_legislator
ren c245b corrupt_gov
ren c245c corrupt_judge
ren c245d corrupt_party

ren c254 foreign_technology

ren c262a3y labor_3y
lab var c262a2y "Average number of permanent wkrs (total) 2 years ago"
ren c262a2y labor_2y
ren c262a1y labor_1y

ren c281a1y asset_1y
ren c281a2y asset_2y
ren c281a3y asset_3y
ren c282b1y liability_1y
ren c282b2y liability_2y
ren c282b3y liability_3y

/* Create new variable */

* Ownership
gen ownership1 = ""
replace ownership1 = "domestic" if pctownership_domestic > 50 & !missing(pctownership_domestic)
replace ownership1 = "foreign" if pctownership_foreign > 50 & !missing(pctownership_foreign)
replace ownership1 = "state" if pctownership_state > 50 & !missing(pctownership_state)
replace ownership1 = "other" if pctownership_other > 50 & !missing(pctownership_other)
encode ownership1, gen(ownership)
drop ownership1

* Sale
			  
/* ----- Save data ---- */
order country

saveold "/home/anh/projects/prospectus/clean_data/doingbusiness_0206.dta", replace
