wrangle_race <- function(asthmaster_df) {
    
    asthmaster_df |> 
        mutate(
            Race = 
                factor(
                    case_match(
                        Race,
                        c("Native Hawaiian or Other Pacific Islande", "None of these apply") ~ "Other",
                        c("Patient Unavailable", "Refused", "Unknown", NA) ~ NA,
                        .default = Race
                    ),
                    levels = c("Asian", "Black or African American", "White", "Other"),
                    ordered = TRUE
                ),
            Race_Black = case_match(
                Race,
                "Black or African American" ~ TRUE,
                NA ~ NA,
                .default = FALSE
            )
        ) |> 
        relocate(
            Race,
            Race_Black,
            .after = AgeAtEncounter 
        )
    
}