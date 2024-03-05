add_black_race <- function(asthmaster_df) {
    
    asthmaster_df |> 
        mutate(
            Race_Black = case_when(
                Race == "Black or African American" ~ TRUE,
                is.na(Race) ~ NA,
                TRUE ~ FALSE
            )
        ) |> 
        relocate(
            Race_Black,
            .after = Race 
        )
    
}