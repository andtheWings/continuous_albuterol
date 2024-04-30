add_first_albuterol <- function(asthmaster_raw_df, source_med_admin_raw_df) {
    
    source_med_admin_raw_df |> 
        filter(catalogcvdsc == "albuterol") |> 
        filter(StrengthDoseUnit == "mg/hr") |> 
        group_by(FIN) |> 
        slice_min(DT) |> 
        ungroup() |> 
        select(FIN, Albuterol_Cont_Dose_First = StrengthDose) |> 
        distinct(FIN, .keep_all = TRUE) |> 
        mutate(
            Albuterol_Cont_Dose_First = 
                Albuterol_Cont_Dose_First |> 
                as.numeric() |> 
                round(1)
        ) |> 
        right_join(asthmaster_raw_df, by = "FIN") |> 
        relocate(Albuterol_Cont_Dose_First, .before = Albuterol_Cont_Dose) |> 
        rename(Albuterol_Cont_Dose_Max = Albuterol_Cont_Dose)
    
}