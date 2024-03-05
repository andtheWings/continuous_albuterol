add_albuterol_dose_factor <- function(asthmaster_df) {
    
    asthmaster_df |> 
        mutate(
            Albuterol_Cont_Dose_Dichot = case_when(
                Albuterol_Cont_Dose %in% c(10, 15) ~ Albuterol_Cont_Dose,
                TRUE ~ NA
            ),
            Albuterol_Cont_Dose_Factor = factor(
                Albuterol_Cont_Dose_Dichot,
                levels = c(10, 15),
                ordered = TRUE
            ),
            Albuterol_Cont_Dose_Logit = case_match(
                Albuterol_Cont_Dose,
                10 ~ FALSE,
                15 ~ TRUE,
                NA ~ NA
            )
        ) |> 
        select(
            -Albuterol_Cont_Dose_Dichot
        ) |> 
        relocate(
            Albuterol_Cont_Dose_Factor, Albuterol_Cont_Dose_Logit,
            .after = Albuterol_Cont_Dose
        )
    
}