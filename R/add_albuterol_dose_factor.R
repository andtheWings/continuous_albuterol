add_albuterol_dose_factor <- function(asthmaster_df) {
    
    asthmaster_df |> 
        mutate(
            Albuterol_Cont_Dose_Factor = 
                factor(
                    case_match(
                        Albuterol_Cont_Dose_First,
                        10 ~ "10 mg/hr",
                        15 ~ "15 mg/hr",
                        .default = NA
                    ),
                    levels = c("10 mg/hr", "15 mg/hr"),
                    ordered = TRUE
                ),
            Albuterol_Cont_Dose_Logit = 
                case_match(
                    Albuterol_Cont_Dose_First,
                    10 ~ FALSE,
                    15 ~ TRUE,
                    .default = NA
                )
        ) |> 
        relocate(
            Albuterol_Cont_Dose_Factor, 
            Albuterol_Cont_Dose_Logit,
            .after = Albuterol_Cont_Dose_First
        )
    
}