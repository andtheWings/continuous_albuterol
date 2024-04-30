wrangle_asthmaster_modified <- function(asthmaster_raw_df, source_med_admin_raw_df) {
    
    asthmaster_raw_df |> 
        add_first_albuterol(source_med_admin_raw_df) |> 
        add_albuterol_dose_factor() |> 
        filter(!is.na(Albuterol_Cont_Dose_Factor)) |> 
        add_albuterol_duration(source_med_admin_raw_df) |> 
        wrangle_race() |> 
        wrangle_ethnicity() |> 
        add_percent_pass_change() |> 
        add_discrete_duration_outcomes()
    
}