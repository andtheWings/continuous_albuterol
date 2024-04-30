wrangle_ethnicity <- function(asthmaster_df) {
    
    asthmaster_df |> 
        mutate(
            Ethnicity = 
                factor(
                    case_match(
                        Ethnicity,
                        c("Declined", "Patient Unavailable", "Unknown", NA) ~ NA,
                        .default = Ethnicity
                    ),
                    levels = c("Hispanic or Latino", "Not Hispanic or Latino"),
                    ordered = TRUE
                ),
            Ethnicity_Hispanic = 
                case_match(
                    Ethnicity,
                    "Not Hispanic or Latino" ~ FALSE,
                    "Hispanic or Latino" ~ TRUE,
                    NA ~ NA
                )
        ) |> 
        relocate(
            Ethnicity,
            Ethnicity_Hispanic,
            .after = AgeAtEncounter
        )
    
}