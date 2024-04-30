add_ipw <- function(ipw_model_obj, asthmaster_compact_tbl_df, asthmaster_modified_tbl_df) {
    
    df1 <-
        ipw_model_obj$.workflow[[1]] |> 
        augment(new_data = asthmaster_compact_tbl_df) |>   
        select(FIN, .pred_15 = `.pred_15 mg/hr`) |> 
        inner_join(asthmaster_modified_tbl_df, by = "FIN")
    
    df2 <-
        df1 |> 
        mutate(
            ipw_ate = (Albuterol_Cont_Dose_Logit / .pred_15) + ((1 - Albuterol_Cont_Dose_Logit) / (1 - .pred_15)),
            ipw_stabilized =  ipw_ate * mean(df1$Albuterol_Cont_Dose_Logit == 1) / (mean(ipw_ate * (df1$Albuterol_Cont_Dose_Logit == 1)) + mean(ipw_ate * (df1$Albuterol_Cont_Dose_Logit == 0))),
            .after = .pred_15
        )
    
    return(df2)
    
}