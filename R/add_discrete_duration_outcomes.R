add_discrete_duration_outcomes <- function(asthmaster_df) {
    
    asthmaster_df |> 
        mutate(
            # across(
            #     .cols = c(
            #         PICU.LOS.VPS, 
            #         Hospital_LOS
            #     ),
            #     .fns = ~ .x * 24
            # ),
            across(
                .cols = c(
                    PICU.LOS.VPS, 
                    Hospital_LOS
                ),
                .fns = ~round2(.x * 24, digits = 0),
                .names = "{.col}_discrete_hrs"
            )
        ) |> 
        mutate(
            Albuterol_Cont_discrete_hrs = round2(Albuterol_Cont_hrs_duration, digits = 0)
        )
    
    
}