import delimited using "geocoded_data/linked_combined_prescriber_geocodio_58367def8304c8b79a951dfea55d6c66db563daf.csv", clear

ren *, proper
ren Prscrbr_Npi Prscrbr_NPI
ren Prscrbr_Zip5 Prscrbr_zip5 
keep Prscrbr_NPI Prscrbr_Last_Org_Name Prscrbr_First_Name Prscrbr_St1 Prscrbr_St2 Prscrbr_zip5 Prscrbr_City Prscrbr_State_Abrvtn Geocodio* Countyfips

merge 1:m Prscrbr_NPI Prscrbr_Last_Org_Name Prscrbr_First_Name Prscrbr_St1 Prscrbr_St2 Prscrbr_zip5 Prscrbr_City Prscrbr_State_Abrvtn ///
	using "temp/linked_panel.dta", assert(3) nogen 

tab year 
	
preserve
collapse (min) firstyear = year (max) lastyear = year, by(Prscrbr_NPI)
tab firstyear 
tab lastyear 
save "temp/entry_exit.dta", replace
restore
	
merge m:1 Prscrbr_NPI using "temp/entry_exit.dta", assert(3) nogen 

ren Countyfips county_fips
	
keep Prscrbr_NPI county_fips Tot_Clms top5_flag top1_flag year firstyear lastyear
	
save "temp/moud_analytic_data_05292025.dta", replace