add_percent_pass_change <- function(asthmaster_df) {
    
    asthmaster_df |> 
        mutate(
            across(
                starts_with("PASSchange"),
                ~ .x / PASS_Initial * 100,
                .names = "{.col}_percent"
            )
        )
    
}