/* Save all excel sheets in Stata format
forvalues year = 2013/2023 {
    
	local file "`year'_linked.xlsx"
    
    * Import Excel file
    import excel using "`file'", firstrow clear
    
    * Add a variable for year 
    gen year = `year'
    
    * Save or append
    if `year' == 2013 {
        save "temp/linked_combined.dta", replace
    }
    else {
        append using "temp/linked_combined.dta"
        save "temp/linked_combined.dta", replace
    }
}
*/
* Load dataset
use "temp/linked_combined.dta", clear

* Collapse over different types of drugs
collapse (sum) Tot_Clms, by(Prscrbr_NPI Prscrbr_Last_Org_Name Prscrbr_First_Name Prscrbr_St1 Prscrbr_St2 Prscrbr_zip5 Prscrbr_City Prscrbr_State_Abrvtn year)

* Flag top 5th percentile prescribers each year
egen prescriber_rank = rank(Tot_Clms), by(year)
gen cons = 1
egen number_prescribers = total(cons), by(year)
gen prescriber_percentile = prescriber_rank / number_prescribers
gen top1_flag = prescriber_percentile>=0.99
gen top5_flag = prescriber_percentile>=0.95

save "temp/linked_panel.dta", replace 

collapse (sum) Tot_Clms (max) top5_flag, by(Prscrbr_NPI Prscrbr_Last_Org_Name Prscrbr_First_Name Prscrbr_St1 Prscrbr_St2 Prscrbr_zip5 Prscrbr_City Prscrbr_State_Abrvtn)

gen Prscrb_St1St2 = Prscrbr_St1 + ", " + Prscrbr_St2
order Prscrb_St1St2, after(Prscrbr_St2)

export excel using "temp/linked_combined_prescriber.xlsx", firstrow(variables) replace