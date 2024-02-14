add_albuterol_duration <- function(asthmaster_raw_df, source_med_admin_raw_df) {
    
    just_albuterol <-
        source_med_admin_raw_df |> 
        filter(catalogcvdsc == "albuterol") |> 
        mutate(is_continuous = if_else(StrengthDoseUnit == "mg/hr", TRUE, FALSE))
    
    continuous_times <-
        just_albuterol |> 
        filter(is_continuous == TRUE) |> 
        group_by(FIN) |> 
        summarize(
            dt_first_continuous = min(DT),
            dt_last_continuous = max(DT),
            Albuterol_Cont_days_from_first_to_last = 
                (dt_last_continuous - dt_first_continuous) |> 
                as.numeric(units = "days") |> 
                round(digits = 3)
        ) |> 
        ungroup()
    
    times <-
        just_albuterol |> 
        left_join(continuous_times, by = "FIN") |> 
        filter(is_continuous == FALSE & DT > dt_last_continuous) |> 
        group_by(FIN) |> 
        summarise(
            dt_first_after_continuous = min(DT),
            dt_first_continuous = first(dt_first_continuous)
        ) |> 
        ungroup() |> 
        mutate(
            Albuterol_Cont_days_from_first_to_first_non = 
                (dt_first_after_continuous - dt_first_continuous) |> 
                as.numeric(units = "days") |> 
                round(digits = 3)
        ) |> 
        select(-dt_first_continuous) |> 
        right_join(continuous_times, by = "FIN")
    
    final_df <-
        times |> 
        select(FIN, Albuterol_Cont_days_from_first_to_last, Albuterol_Cont_days_from_first_to_first_non) |>
        mutate(
            Albuterol_Cont_avg_duration = (Albuterol_Cont_days_from_first_to_last + Albuterol_Cont_days_from_first_to_first_non) / 2,
            Albuterol_Cont_duration_method_diff = Albuterol_Cont_days_from_first_to_first_non - Albuterol_Cont_days_from_first_to_last
        ) |> 
        right_join(asthmaster_raw_df, by = "FIN") |> 
        relocate(
            Albuterol_Cont_days_from_first_to_last, 
            Albuterol_Cont_days_from_first_to_first_non,
            Albuterol_Cont_avg_duration,
            Albuterol_Cont_duration_method_diff, 
            .after = Albuterol_Cont_Dose
        )
    
    return(final_df)
    
}