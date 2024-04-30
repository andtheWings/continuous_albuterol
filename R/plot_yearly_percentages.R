plot_yearly_percentages <- function(asthmaster_modified_df) {
    
    year_counts <-
        asthmaster_modified_df |> 
        count(Year) |> 
        rename(year_count = n)
    
    plot1 <-
        asthmaster_modified_df |> 
        count(Year, Albuterol_Cont_Dose_Factor) |> 
        left_join(year_counts, by = "Year") |> 
        mutate(percentage = n / year_count * 100) |> 
        ggplot(aes(x = Year, linetype = Albuterol_Cont_Dose_Factor, y = percentage)) +
        geom_line() +
        coord_cartesian(ylim = c(0, 100)) +
        labs(
            title = "Percent Encounters Receiving Initial Albuterol Dosage by Year",
            y = "Percentage",
            linetype = "Initial Dose of Continuous \n Albuterol (mg/hr)"
        ) +
        theme_bw() +
        guides(
            linetype = 
                guide_legend(
                    title.position = "top", 
                    title.hjust = 0.5, 
                    nrow = 2, 
                    byrow = TRUE
                )
        ) +
        theme(
            plot.title = element_text(hjust = 0.5),
            legend.position = c(0.95, 0.95), 
            legend.justification = c("right", "top"), 
            legend.background = element_rect(color = "black", size = 0.5)
        )
    
    return(plot1)
    
}