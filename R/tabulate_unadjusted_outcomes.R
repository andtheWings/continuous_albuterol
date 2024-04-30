tabulate_unadjusted_outcomes <- function(asthmaster_df) {
    
    theme_gtsummary_journal("jama")
    
    gt_summary_labels <- list(
        # Treatments
        Resp_Initial = "Initial Oxygen Support",
        FiO2_Initial = "Initial Fraction of Inspired Oxygen",
        Ipratropium_admin = "Ipratropium Ever Given",
        Magnesium_admin = "Magnesium Ever Given",
        Epinephrine_admin = "Epinephrine Ever Given",
        # Independent Variable
        ## Albuterol_Cont_Dose_Factor,
        # Outcomes
        PASSchange0_24_percent = "PASS (% Change at 24 hrs)",
        Albuterol_Cont_hrs_duration = "Length of Continuous Albuterol Therapy (hrs)",
        PICU.LOS.VPS = "Length of PICU Stay (days)",
        Hospital_LOS = "Length of Hospital Admission (days)"
        ## DischargeDisposition
    )
    
    gt_treatments <-
        asthmaster_df |> 
        select(
            Albuterol_Cont_Dose_Factor,
            Resp_Initial,
            FiO2_Initial,
            Ipratropium_admin,
            Magnesium_admin,
            Epinephrine_admin
        ) |> 
        tbl_summary(
            by = "Albuterol_Cont_Dose_Factor",
            label = gt_summary_labels,
            missing_text = "Missing"
        ) |> 
        add_overall() |> 
        add_p()
    
    gt_outcomes <-
        asthmaster_df |> 
        select(
            Albuterol_Cont_Dose_Factor,
            PASSchange0_24_percent,
            Albuterol_Cont_hrs_duration,
            PICU.LOS.VPS,
            Hospital_LOS
        ) |> 
        # mutate(
        #     
        # ) |> 
        tbl_summary(
            by = "Albuterol_Cont_Dose_Factor",
            label = gt_summary_labels,
            missing_text = "Missing",
            digits = list(
                PICU.LOS.VPS ~ 1,
                Hospital_LOS ~ 1
            )
        ) |> 
        add_overall() |> 
        add_p()
    
    gt_all <-
        tbl_stack(
            list(gt_treatments, gt_outcomes),
            c("TREATMENTS", "OUTCOMES")
        ) |> 
        modify_spanning_header(
            update = c(stat_1, stat_2) ~ "**Initial Continuous Albuterol Dose**",
        ) |> 
        as_gt()
    
    
    gtsave(gt_all, "media/outcome_table.html")
    
}