add_ethnicity_hispanic <- function(asthmaster_df) {
    
    asthmaster_df |> 
        mutate(
            Ethnicity_Hispanic = case_when(
                Ethnicity == "Not Hispanic or Latino" ~ FALSE,
                Ethnicity == "Hispanic or Latino" ~ TRUE,
                TRUE ~ NA
            )
        ) |> 
        relocate(
            Ethnicity_Hispanic,
            .after = Ethnicity
        )
    
}