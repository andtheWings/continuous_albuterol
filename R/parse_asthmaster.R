parse_asthmaster <- function(asthmaster_csv) {
    
    parse_spec <- "iiTTiTicccccicllllllllddllillliddllildddllddllddllddllddlillddllllllddiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiidddddddddddddddddddddddddddddddddddddiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiidddddddddddddddiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiilllllicccccccccccccccldddddddddddddddccccccccccccccciiiiiiiiiiiiiiicccccccccccccccccccccccccccccccccccccccccccccdddddddddddddddcccccccccccccccccccccccccccccccccccccccccccccdddddddddddddddccccccccccccccc"
    
    df1 <-
        read_csv(asthmaster_csv, col_types = parse_spec) |> 
        mutate(
            across(
                .cols = c(
                    starts_with("Lymphocytes"), 
                    starts_with("Neutrophils")
                ),
                .fns = ~case_when(
                    is.na(.x) ~ NA,
                    .x == "see man diff" ~ NA,
                    TRUE ~ as.integer(.x)
                )
            )
        ) 
    
    return(df1)
}