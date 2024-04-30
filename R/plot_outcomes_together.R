plot_outcomes_together <- function(asthmaster_tbl_df) {
    
    plot_pass <- 
        asthmaster_tbl_df |> 
        plot_outcome_box(
            PASSchange0_24_percent, 
            "PASS (% change at 24 hrs)",
            extend_line_num = -0.48,
            y_position_num = c(65, 65),
        ) +
        scale_y_continuous(
            breaks = c(-50, -25, 0, 25, 50, 75)
        )
    
    plot_len_albuterol <-
        asthmaster_tbl_df |> 
        plot_outcome_box(
            Albuterol_Cont_hrs_duration,
            outcome_label = "Length of Continuous Albuterol (hrs)",
            y_position_num = c(45, 45),
            extend_line_num = -0.48,
            tip_length_num = 0.005
        ) +
        scale_y_continuous(
            breaks = c(0, 15, 30, 45, 60, 75)
        ) +
        coord_cartesian(ylim = c(0, 76))
    
    plot_len_picu <-
        asthmaster_tbl_df |> 
        plot_outcome_box(
            PICU.LOS.VPS,
            outcome_label = "Length of PICU Stay (days)",
            y_position_num = c(5.5, 5.5),
            extend_line_num = -0.48,
            tip_length_num = 0.01
        ) +
        scale_y_continuous(
            breaks = c(0, 1.5, 3, 4.5, 6, 7.5)
        ) +
        coord_cartesian(ylim = c(0, 7.6))
    

    plot_len_hospital <-
        asthmaster_tbl_df |> 
        plot_outcome_box(
            Hospital_LOS,
            outcome_label = "Length of Hospital Stay (days)",
            y_position_num = c(4.25, 4.25),
            extend_line_num = -0.48,
            tip_length_num = 0.005
        ) +
        scale_y_continuous(
            breaks = c(0, 1.5, 3, 4.5, 6, 7.5)
        ) +
        coord_cartesian(ylim = c(0, 7.6))

    together <- 
        plot_pass + plot_len_albuterol + plot_len_picu + plot_len_hospital +
        plot_annotation(
            title = "Comparing Outcomes by Albuterol Dose",
            caption = "NS = 'not significant', * = 'p < 0.05', ** = 'p < 0.01', *** = 'p < 0.001'",
            theme = theme(plot.title = element_text(hjust = 0.5))
        ) +
        plot_layout(axes = "collect_x")
    
    return(together)
    
}