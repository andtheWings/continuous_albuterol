plot_outcome_box <- 
    function(
        asthmaster_tbl_df, 
        outcome_var, 
        outcome_label = "", 
        comparisons_char = c("10 mg/hr", "15 mg/hr"),
        y_position_num = NULL,
        extend_line_num = 0,
        tip_length_num = 0.03
    ) {
        
        df1 <- filter(asthmaster_tbl_df, !is.na({{ outcome_var }}))
        
        df1 |> 
            ggplot(aes(
                y = {{ outcome_var }}, 
                x = Albuterol_Cont_Dose_Factor
            )) +
            geom_boxplot(width = 0.5) +
            ggsignif::geom_signif(
                comparisons = list(comparisons_char),
                map_signif_level = TRUE,
                y_position = y_position_num,
                extend_line = extend_line_num,
                tip_length = tip_length_num
            ) +
            labs(
                title = outcome_label,
                x = "Initial Dose of Continuous Albuterol",
                y = NULL
            ) +
            theme_bw() +
            theme(
                plot.title = element_text(
                    size = 11,
                    hjust = 0.5
                )
            )
        
    }
