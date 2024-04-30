fit_glm_for_ipw <- function(asthmaster_mod_df) {
    
    set.seed(1989)
    
    asthmaster_select <-
        asthmaster_mod_df |> 
        select(
            Albuterol_Cont_Dose_Factor,
            Year,
            AgeAtEncounter, 
            # Weight.VPS, 
            # Weight,
            # Race,
            Race_Black, 
            # Ethnicity,
            # Ethnicity_Hispanic,
            # Sex,
            # Patient.Origin,
            # Ipratropium,
            # PneumoniaDx, 
            # ChronicLungdzDx, 
            # CCC, 
            # BPD,
            HR_Initial, 
            # RR_Initial, 
            # O2_Initial, 
            # FiO2_Initial,
            # PASS_Initial,
            # Temp_Initial,
            # Resp_Initial
        ) |> 
        na.omit()
    
    asthmaster_split <- 
        rsample::initial_split(
            asthmaster_select, 
            prop = 0.75, 
            strata = Albuterol_Cont_Dose_Factor
        )
    
    fitted_glm <-
        workflow() |> 
        add_formula(Albuterol_Cont_Dose_Factor ~ Year + AgeAtEncounter + Race_Black + HR_Initial) |> 
        add_model(parsnip::logistic_reg(mode = "classification", engine = "glm")) |> 
        tune::last_fit(asthmaster_split)
        
    return(fitted_glm$.workflow[[1]])
    
    
}