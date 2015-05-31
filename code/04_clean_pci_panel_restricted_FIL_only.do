* Introduce Conditional Sectors*		
gen restrict=0
replace restrict=. if reg_year==. 		
replace restrict=. if reg_year==.b 	
g restrict_all=0
replace restrict_all=. if reg_year==. 		
replace restrict_all=. if reg_year==.b 		
gen period=. 		
replace period=5 if reg_year>=2010		
replace period=4 if reg_year<2010		
replace period=3 if reg_year<2007		
replace period=2 if reg_year<2005		
replace period=1 if reg_year<2000	

**Restricted to all investors**
*real estate activities*
replace restrict_all=1 if isic4_no>6799 & isic4_no<6900 
*mining and quarrying*
replace restrict_all=1 if isic4_no>499 & isic4_no<1000 
*defence and public administration*
replace restrict_all=1 if isic4_no>8399 & isic4_no<8500
*higher education and training services*
replace restrict_all=1 if isic4_no>8529 & isic4_no<8600
*culture*
replace restrict_all=1 if isic4_no>8999 & isic4_no<9200
*media publishing (newspapers, journals, television, motion picture)* 
replace restrict_all=1 if isic4_no>5799 & isic4_no<6100 
*printing and re-printing* 
replace restrict_all=1 if isic4_no>1799 & isic4_no<1900 

**Group A originals**
*fishing and aquaculture*
replace restrict=1 if isic4_no>299 & isic4_no<400 
*logging and silviculture*
replace restrict=1 if isic4_no>199 & isic4_no<300
*mining and quarrying*
replace restrict=1 if isic4_no>499 & isic4_no<1000 
*manufacturing of food products (suguar + alcohol + alcohol + oils)* 
replace restrict=1 if isic4_no>999 & isic4_no<1300 
*manufacture of coke and refined petroleum products*
replace restrict=1 if isic4_no>1899 & isic4_no<2000 
*manufacture of chemicals and chemical products*
replace restrict=1 if isic4_no>1999 & isic4_no<2100 
*manufacture of pharmaceutical products and pharmaceutical preparations*
replace restrict=1 if isic4_no>2099 & isic4_no<2200 
*manufacture of non-metallic mineral products (cement)*
replace restrict=1 if isic4_no>2389 & isic4_no<2400 
*production of electricity*
replace restrict=1 if isic4_no>3499 & isic4_no<3600 
*media publishing (newspapers, journals, television, motion picture)* 
replace restrict=1 if isic4_no>5799 & isic4_no<6100 
*printing and re-printing* 
replace restrict=1 if isic4_no>1799 & isic4_no<1900 
*telecom* 
replace restrict=1 if isic4_no>6099 & isic4_no<6400 
*postal services* 
replace restrict=1 if isic4_no>5299 & isic4_no<5400 
*infrastructure construction (civil engineering)* 
replace restrict=1 if isic4_no>4199 & isic4_no<4300 
*land sea and air transport* 
replace restrict=1 if isic4_no>4899 & isic4_no<5200 
*real estate activities*
replace restrict=1 if isic4_no>6799 & isic4_no<6900 
*hospital activities*
replace restrict=1 if isic4_no>8599 & isic4_no<8700
*higher education and training services*
replace restrict=1 if isic4_no>8529 & isic4_no<8600
*gambling*
replace restrict=1 if isic4_no>9199 & isic4_no<9300
*defence and public administration*
replace restrict=1 if isic4_no>8399 & isic4_no<8500
*travel and tourism*
replace restrict=1 if isic4_no>7899 & isic4_no<8000
*culture*
replace restrict=1 if isic4_no>8999 & isic4_no<9200
*financial and insurance activities*
replace restrict=1 if isic4_no>6399 & isic4_no<6500
*legal and accounting*
replace restrict=1 if isic4_no>6799 & isic4_no<7000
*Scientific research and development*
replace restrict=1 if isic4_no>7199 & isic4_no<7300

**Period 3 opening**
*manufacture of non-metallic mineral products (cement)*
replace restrict=0 if isic4_no>2389 & isic4_no<2400 & period>2
*production of electricity*
*replace restrict=0 if isic4_no>3499 & isic4_no<3600 & period>2 

**Period 4 opening**
*fishing and aquaculture*
replace restrict=0 if isic4_no>299 & isic4_no<400 & period>3 
*infrastructure construction (civil engineering)* 
replace restrict=0 if isic4_no>4199 & isic4_no<4300 & period>3 
*Scientific research and development*
replace restrict=0 if isic4_no>7199 & isic4_no<7300 & period>3 
*auxilary financial and insurance activities*
replace restrict=0 if isic4_no>6499 & isic4_no<6500 & period>3 
*manufacture of chemicals and chemical products*
replace restrict=0 if isic4_no>1999 & isic4_no<2100 & period>3

**Period 5 opening**
*mining and quarrying*
*replace restrict=0 if isic4_no>799 & isic4_no<900 & period>4
*manufacture of pharmaceutical products and pharmaceutical preparations*
*replace restrict=0 if isic4_no>2099 & isic4_no<2200 & period>4
*financial intermediation*
*replace restrict=0 if isic4_no>6399 & isic4_no<6400 & period>4
*legal and accounting*
*replace restrict=0 if isic4_no>6799 & isic4_no<7000 & period>4
*travel and tourism*
replace restrict=0 if isic4_no>7899 & isic4_no<8000 & period>4
*water transport*
replace restrict=0 if isic4_no>4999 & isic4_no<5100 & period>4
*real estate *lease/fee*
*replace restrict=0 if isic4_no>6819 & isic4_no<6830 & period>4
*manufacturing of food products (suguar + alcohol + alcohol + oils)* 
*replace restrict=0 if isic4_no>999 & isic4_no<1300  & period>4
*hospital activities*
replace restrict=0 if isic4_no>8599 & isic4_no<8700 & period>4

replace restrict=. if isic4_no==. 
