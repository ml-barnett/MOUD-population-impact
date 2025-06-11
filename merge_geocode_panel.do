import delimited using "geocoded_data/linked_combined_prescriber_geocodio_06eddae37bce454cebd7df5f8f9d5aa179a108f2.csv", clear

ren *, proper
ren Prscrbr_Npi Prscrbr_NPI
ren Prscrbr_State_Fips Prscrbr_State_FIPS
ren Prscrbr_Zip5 Prscrbr_zip5 
keep Prscrbr_NPI Prscrbr_Last_Org_Name Prscrbr_First_Name Prscrbr_St1 Prscrbr_St2 Prscrbr_zip5 Prscrbr_City Prscrbr_State_Abrvtn Prscrbr_State_FIPS Geocodio* Countyfips

merge 1:m Prscrbr_NPI Prscrbr_Last_Org_Name Prscrbr_First_Name Prscrbr_St1 Prscrbr_St2 Prscrbr_zip5 Prscrbr_City Prscrbr_State_Abrvtn Prscrbr_State_FIPS ///
	using "temp/linked_panel.dta", assert(3) nogen 

preserve
collapse (min) firstyear = year (max) lastyear = year, by(Prscrbr_NPI)
save "temp/entry_exit.dta", replace
restore
	
merge m:1 Prscrbr_NPI using "temp/entry_exit.dta", assert(3) nogen 

ren Countyfips county_fips
	
keep Prscrbr_NPI county_fips Tot_Clms top5_flag year firstyear lastyear
	
save "temp/moud_analytic_data_05292025.dta", replace