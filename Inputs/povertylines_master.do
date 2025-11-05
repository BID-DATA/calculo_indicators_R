clear all
tempfile new_2021
use "C:\Users\\`=c(username)'\OneDrive - Inter-American Development Bank Group\Documents\poverty_slv\Final_CPI_PPP_to_be_used.dta",clear
keep if region=="LAC"
keep countryname year cpi2021 icp2021 ppp_2021 cpi2021_unadj //cpi2021 viene de  pepe
rename ppp_2021 ppp2021
gen lp420_2021 = 4.20*(365/12)*cpi2021*icp2021
gen lp830_2021 = 8.30*(365/12)*cpi2021*icp2021
save `new_2021'

global ruta ="${surveysFolder}\general_documentation\data_externa\poverty\International_Poverty_Lines"
use "$ruta/clean/4_ppp21_tc_cpi.dta", clear

	separate cpi if year==2011, by(year)
	bys countryname: egen cpi_11= max(cpi2011) 
	drop cpi2011
	gen cpi_2011 = cpi/cpi_11
	gen cpi2011=cpi_11
	
	separate cpi if year==2017, by(year)
	bys countryname: egen cpi_17= max(cpi2017) 
	drop cpi2017
	gen cpi_2017 = cpi/cpi_17
	gen cpi2017=cpi_17
	
	separate cpi if year==2021, by(year)
	bys countryname: egen cpi_21= max(cpi2021) 
	drop cpi2021
	gen cpi_2021 = cpi/cpi_21
	gen cpi2021=cpi_21

keep countryname year cpi_2011 ppp_2011 cpi_2017 ppp_2017 cpi_2021 ppp_2021 ppp21 cpi2021 ppp_2021WB ppp_2021WB //cpi_2021 viene de International poverty lines
rename cpi2021 cpi_2021_2

gen lp19_2011 = 1.9*(365/12)*cpi_2011*ppp_2011
gen lp31_2011 = 3.1*(365/12)*cpi_2011*ppp_2011
gen lp5_2011  = 5*(365/12)*cpi_2011*ppp_2011

gen lp365_2017 = 3.65*(365/12)*cpi_2017*ppp_2017
gen lp685_2017 = 6.85*(365/12)*cpi_2017*ppp_2017
gen lp14_2017  = 14.15*(365/12)*cpi_2017*ppp_2017
gen lp81_2017 = 81.22*(365/12)*cpi_2017*ppp_2017
	
gen lp420_2021_old = 4.20*(365/12)*cpi_2021*ppp_2021
gen lp830_2021_old = 8.30*(365/12)*cpi_2021*ppp_2021

gen lp420_2021WB = 4.20*(365/12)*cpi_2021*ppp_2021WB
gen lp830_2021WB = 8.30*(365/12)*cpi_2021*ppp_2021WB


merge 1:1 countryname year using `new_2021'
drop if inlist(countryname, "St. Lucia ", "Grenada ")

order countryname year lp420_2021 lp420_2021_old lp420_2021WB lp830_2021 lp830_2021_old	lp830_2021WB

br year cpi_2017 cpi_2021 cpi2021 icp2021 if countryname=="Argentina" 

gen lp420_2021_Against_wb = .
replace lp420_2021_Against_wb = 1 if lp420_2021WB == lp420_2021_old
replace lp420_2021_Against_wb = 0 if lp420_2021WB != lp420_2021_old

gen lp830_2021_Against_wb = .
replace lp830_2021_Against_wb = 1 if lp830_2021WB == lp830_2021_old
replace lp830_2021_Against_wb = 0 if lp830_2021WB != lp830_2021_old
br countryname year lp420_2021WB lp420_2021_old if lp420_2021_Against_wb==0 
br countryname year lp830_2021WB lp830_2021_old if lp830_2021_Against_wb==0 
// Chile colombia y costa rica son diferentes de 2021wb a 2021 old
// Brazil Dominican Republic Jamaica Peru tienen el mismo en 2021, de 2021new a 2021 old
save "C:/Users/mrodriguezm/OneDrive - Inter-American Development Bank Group/Documents/povertylines/review2.dta", replace
** GRAPHS
*** ARG
twoway scatter lp365_2017 lp420_2021 lp420_2021_old lp420_2021WB year if countryname=="Argentina" & year>=2000 & year<=2023, ///
    legend(label(1 "lp365_2017") label(2 "lp420_2021") label(3 "lp420_2021_old") label(4 "lp420_2021WB")) ///
    title("Argentina") ///
    ytitle("Poverty Line") xtitle("") ///
    xlabel(2000(1)2023, angle(45) labsize(vsmall))
	
twoway scatter lp685_2017 lp830_2021 lp830_2021_old lp830_2021WB year if countryname=="Argentina" & year>=2000 & year<=2023, ///
    legend(label(1 "lp685_2017") label(2 "lp830_2021") label(3 "lp830_2021_old") label(4 "lp830_2021WB")) ///
    title("Argentina") ///
    ytitle("Poverty Line") xtitle("") ///
    xlabel(2000(1)2023, angle(45) labsize(vsmall))

*** SLV
twoway scatter lp365_2017 lp420_2021 lp420_2021_old lp420_2021WB year if countryname=="El Salvador" & year>=2000 & year<=2023, ///
    legend(label(1 "lp365_2017") label(2 "lp420_2021") label(3 "lp420_2021_old") label(4 "lp420_2021WB")) ///
    title("EL Salvador") ///
    ytitle("Poverty Line") xtitle("") ///
    xlabel(2000(1)2023, angle(45) labsize(vsmall))
	
twoway scatter lp685_2017 lp830_2021 lp830_2021_old lp830_2021WB year if countryname=="El Salvador" & year>=2000 & year<=2023, ///
    legend(label(1 "lp685_2017") label(2 "lp830_2021") label(3 "lp830_2021_old") label(4 "lp830_2021WB")) ///
    title("El Salvador") ///
    ytitle("Poverty Line") xtitle("") ///
    xlabel(2000(1)2023, angle(45) labsize(vsmall))
	
// ARG salvador tienen problemas

*****************CPI****************************
clear all 
import delimited "C:\Users\mrodriguezm\OneDrive - Inter-American Development Bank Group\Documents\povertylines\dataset_2025-10-31T22_02_36.532278500Z_DEFAULT_INTEGRATION_IMF.STA_CPI_5.0.0.csv", clear varnames(1) case(lower)

	keep if index_type=="Consumer price index (CPI)"
	keep if coicop_1999=="All Items"
	keep if type_of_transformation=="Index"
	keep if frequency=="Annual"
	rename time_period year
	rename country countryname
	destring year, replace
	rename obs_value cpi
	keep countryname year cpi

local countries "Argentina Bolivia Brazil Chile Colombia Costa Rica Dominican Republic Ecuador El Salvador Honduras Panama Paraguay Peru Uruguay Jamaica Belize Guatemala Haiti Mexico Nicaragua Bahamas Barbados Guyana Suriname Trinidad and Tobago Venezuela, República Bolivariana de"
keep if strpos("`countries'", countryname)
sort countryname year

	bysort countryname: egen base2021 = mean(cond(year==2021, cpi, .))
	gen cpi2021_imf = (cpi / base2021)

	bysort countryname: egen base2017 = mean(cond(year==2017, cpi, .))
	gen cpi2017_imf = (cpi / base2017)
	
	bysort countryname: egen base2011 = mean(cond(year==2011, cpi, .))
	gen cpi2011_imf = (cpi / base2011)
	rename cpi cpi_imf
	replace countryname = "Venezuela, RB" if countryname == "Venezuela, República Bolivariana de"
	
	
	
	save "C:\Users\mrodriguezm\OneDrive - Inter-American Development Bank Group\Documents\povertylines\imf_cpi.dta", replace
	
use "C:/Users/mrodriguezm/OneDrive - Inter-American Development Bank Group/Documents/povertylines/review2.dta", clear
drop _merge
merge 1:1 countryname year using "C:\Users\mrodriguezm\OneDrive - Inter-American Development Bank Group\Documents\povertylines\imf_cpi.dta"

*** CPI COUNTRY LOOP
local country1 "Argentina"
local country2 "Bahamas"
local country3 "Barbados"
local country4 "Belize"
local country5 "Bolivia"
local country6 "Brazil"
local country7 "Chile"
local country8 "Colombia"
local country9 "Costa Rica"
local country10 "Dominican Republic"
local country11 "Ecuador"
local country12 "El Salvador"
local country13 "Guatemala"
local country14 "Guyana"
local country15 "Haiti"
local country16 "Honduras"
local country17 "Jamaica"
local country18 "Mexico"
local country19 "Nicaragua"
local country20 "Panama"
local country21 "Paraguay"
local country22 "Peru"
local country23 "Suriname"
local country24 "Trinidad and Tobago"
local country25 "Uruguay"
local country26 "Venezuela, RB"

sort countryname year 
forvalues i = 1/26 {
    local country `country`i''
    
    // Create CPI line graph
twoway scatter cpi2021_imf cpi2021 cpi_2021 year if countryname=="`country'" & year >= 2000 & year <= 2024, ///
    legend(label(1 "cpi2021_imf") label(2 "cpi2021") label(3 "cpi_2021") label(4 "cpi_2017")) ///
    title("`country'") ///
    ytitle("CPI") xtitle("Year") ///
    xlabel(2000(1)2024, angle(45) labsize(vsmall)) ///
    xscale(range(2000 2024)) ///
    xlabels(2000(1)2024)
graph export "C:\Users\mrodriguezm\OneDrive - Inter-American Development Bank Group\Documents\povertylines\linegraphs\\`country'_cpi.png", replace
}

gen lp19_2011_imf = 1.9*(365/12)*cpi2011_imf*ppp_2011
gen lp31_2011_imf = 3.1*(365/12)*cpi2011_imf*ppp_2011
gen lp5_2011_imf  = 5*(365/12)*cpi2011_imf*ppp_2011

gen lp365_2017_imf = 3.65*(365/12)*cpi2017_imf*ppp_2017
gen lp685_2017_imf = 6.85*(365/12)*cpi2017_imf*ppp_2017
gen lp14_2017_imf  = 14.15*(365/12)*cpi2017_imf*ppp_2017
gen lp81_2017_imf = 81.22*(365/12)*cpi2017_imf*ppp_2017
	
gen lp420_2021_imf = 4.20*(365/12)*cpi2021_imf*ppp_2021
gen lp830_2021_imf = 8.30*(365/12)*cpi2021_imf*ppp_2021

gen iso_3 = ""
replace iso_3 = "ARG" if countryname == "Argentina"
replace iso_3 = "BHS" if countryname == "Bahamas"
replace iso_3 = "BRB" if countryname == "Barbados"
replace iso_3 = "BLZ" if countryname == "Belize"
replace iso_3 = "BOL" if countryname == "Bolivia"
replace iso_3 = "BRA" if countryname == "Brazil"
replace iso_3 = "CHL" if countryname == "Chile"
replace iso_3 = "COL" if countryname == "Colombia"
replace iso_3 = "CRI" if countryname == "Costa Rica"
replace iso_3 = "DOM" if countryname == "Dominican Republic"
replace iso_3 = "ECU" if countryname == "Ecuador"
replace iso_3 = "SLV" if countryname == "El Salvador"
replace iso_3 = "GTM" if countryname == "Guatemala"
replace iso_3 = "GUY" if countryname == "Guyana"
replace iso_3 = "HTI" if countryname == "Haiti"
replace iso_3 = "HND" if countryname == "Honduras"
replace iso_3 = "JAM" if countryname == "Jamaica"
replace iso_3 = "MEX" if countryname == "Mexico"
replace iso_3 = "NIC" if countryname == "Nicaragua"
replace iso_3 = "PAN" if countryname == "Panama"
replace iso_3 = "PRY" if countryname == "Paraguay"
replace iso_3 = "PER" if countryname == "Peru"
replace iso_3 = "SUR" if countryname == "Suriname"
replace iso_3 = "TTO" if countryname == "Trinidad and Tobago"
replace iso_3 = "URY" if countryname == "Uruguay"
replace iso_3 = "VEN" if countryname == "Venezuela, RB"



order countryname iso_3 year 

use "C:/Users/mrodriguezm/OneDrive - Inter-American Development Bank Group/Documents/povertylines/masterdata.dta", clear 
gen lp420_2021_imf_icp = 4.20*(365/12)*cpi2021_imf*icp2021
gen lp830_2021_imf_icp = 8.30*(365/12)*cpi2021_imf*icp2021

save "C:/Users/mrodriguezm/OneDrive - Inter-American Development Bank Group/Documents/povertylines/masterdata.dta", replace


*** POVERT LINE COUNTRY LOOP
local country1 "Argentina"
local country2 "Bahamas"
local country3 "Barbados"
local country4 "Belize"
local country5 "Bolivia"
local country6 "Brazil"
local country7 "Chile"
local country8 "Colombia"
local country9 "Costa Rica"
local country10 "Dominican Republic"
local country11 "Ecuador"
local country12 "El Salvador"
local country13 "Guatemala"
local country14 "Guyana"
local country15 "Haiti"
local country16 "Honduras"
local country17 "Jamaica"
local country18 "Mexico"
local country19 "Nicaragua"
local country20 "Panama"
local country21 "Paraguay"
local country22 "Peru"
local country23 "Suriname"
local country24 "Trinidad and Tobago"
local country25 "Uruguay"
local country26 "Venezuela, RB"

forvalues i = 1/26 {
    local country `country`i''
    
    // Create lp420 scatter plot
    twoway (scatter lp420_2021 year if countryname=="`country'" & year>=2000 & year<=2024) ///
       (scatter lp420_2021_old year if countryname=="`country'" & year>=2000 & year<=2024) ///
       (scatter lp420_2021WB year if countryname=="`country'" & year>=2000 & year<=2024) ///
       (scatter lp420_2021_imf year if countryname=="`country'" & year>=2000 & year<=2024, msize(large) msymbol(O)), ///
    legend(label(1 "lp420_2021") label(2 "lp420_2021_old") label(3 "lp420_2021WB") label(4 "lp420_2021_imf")) ///
    title("`country'") ///
    ytitle("Poverty Line") xtitle("") ///
    xlabel(2000(1)2024, angle(45) labsize(vsmall))
    
    graph export "C:\Users\mrodriguezm\OneDrive - Inter-American Development Bank Group\Documents\povertylines\linegraphs\\`country'_lp420.png", replace
    
    // Create lp830 scatter plot
    twoway (scatter lp830_2021 year if countryname=="`country'" & year>=2000 & year<=2024) ///
       (scatter lp830_2021_old year if countryname=="`country'" & year>=2000 & year<=2024) ///
       (scatter lp830_2021WB year if countryname=="`country'" & year>=2000 & year<=2024) ///
       (scatter lp830_2021_imf year if countryname=="`country'" & year>=2000 & year<=2024, msize(large) msymbol(O)), ///
    legend(label(1 "lp830_2021") label(2 "lp830_2021_old") label(3 "lp830_2021WB") label(4 "lp830_2021_imf")) ///
    title("`country'") ///
    ytitle("Poverty Line") xtitle("") ///
    xlabel(2000(1)2024, angle(45) labsize(vsmall))
	
    graph export "C:\Users\mrodriguezm\OneDrive - Inter-American Development Bank Group\Documents\povertylines\linegraphs\\`country'_lp830.png", replace
}
