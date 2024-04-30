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
            first_to_last = 
                (dt_last_continuous - dt_first_continuous) |> 
                as.numeric(units = "hours")
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
            first_to_first_non = 
                (dt_first_after_continuous - dt_first_continuous) |> 
                as.numeric(units = "hours") 
        ) |> 
        select(-dt_first_continuous) |> 
        right_join(continuous_times, by = "FIN") |> 
        mutate(
            duration_diff = first_to_first_non - first_to_last
        )
    
    mean_diff_halved <- mean(times$duration_diff[times$first_to_last != 0], na.rm = TRUE) / 2
    
    ## Figure out way to prevent negative "avgs"
    
    final_df <-
        times |> 
        rowwise() |> 
        mutate(
            avg_duration = 
                case_when(
                    # first_to_last == 0 
                    #     & duration_diff < mean_diff_halved 
                    #     ~ first_to_first_non / 2,
                    # first_to_last == 0 
                    #     & duration_diff > mean_diff_halved 
                    #     & (first_to_first_non / 2) > (first_to_first_non - mean_diff_halved)
                    #     ~ first_to_first_non / 2,
                    # first_to_last == 0 
                    #     & duration_diff > mean_diff_halved 
                    #     & (first_to_first_non / 2) < (first_to_first_non - mean_diff_halved)
                    #     ~ first_to_first_non - mean_diff_halved,
                    is.na(first_to_first_non) ~ first_to_last + mean_diff_halved,
                    TRUE ~ mean(c(first_to_last, first_to_first_non))
                )
        ) |> 
        ungroup() |>
        # mutate(
        #     across(
        #         c(first_to_last, first_to_first_non, duration_diff, avg_duration),
        #         ~round(.x, 0)
        #     )
        # ) |> 
        select(
            FIN, 
            Albuterol_Cont_hrs_first_to_last = first_to_last, 
            Albuterol_Cont_hrs_first_to_first_non = first_to_first_non,
            Albuterol_Cont_hrs_duration_diff = duration_diff,
            Albuterol_Cont_hrs_duration = avg_duration
        ) |>
        right_join(asthmaster_raw_df, by = "FIN") |> 
        relocate(
            Albuterol_Cont_hrs_first_to_last, 
            Albuterol_Cont_hrs_first_to_first_non,
            Albuterol_Cont_hrs_duration_diff,
            Albuterol_Cont_hrs_duration,
            .after = Albuterol_Cont_Dose_Logit
        )
    
    return(final_df)
    
}