tabulate_summary_stats <- function(asthmaster_df) {
    
    theme_gtsummary_journal("jama")
    
    gt_summary_labels <- list(
        Year = "Year of Admission",
        AgeAtEncounter = "Age (yrs)",
        Weight.VPS = "Weight (kg)",
        ChronicLungdzDx = "Chronic Lung Disease",
        BPD = "Bronchopulmonary Dysplasia",
        CCC = "Number of Complex Chronic Conditions",
        PneumoniaDx = "Comorbid Pneumonia",
        Temp_Initial = "Temperature",
        HR_Initial = "Heart Rate",
        RR_Initial = "Respiratory Rate",
        O2_Initial = "Oxygen Saturation",
        PASS_Initial = "Pediatric Asthma Severity Score",
        SBP_Initial = "Systolic Blood Pressure",
        DBP_Initial = "Diastolic Blood Pressure"
    )
    
    
    gt_demographics <-
        asthmaster_df |> 
        select(
            Albuterol_Cont_Dose_Factor,
            # Demographics
            ## Year,
            AgeAtEncounter,
            ## Weight,
            ## Weight.VPS,
            Sex,
            Race,
            Ethnicity
        ) |> 
        # mutate(
        #     
        # ) |> 
        tbl_summary(
            by = "Albuterol_Cont_Dose_Factor",
            label = gt_summary_labels,
            missing_text = "Missing"
        ) |> 
        add_overall() |> 
        add_p()
    
    
    gt_clinicals <-
        asthmaster_df |> 
        select(
            Albuterol_Cont_Dose_Factor,
            ChronicLungdzDx,
            BPD,
            # CCC,
            PneumoniaDx
        ) |> 
        tbl_summary(
            by = "Albuterol_Cont_Dose_Factor",
            label = gt_summary_labels,
            missing_text = "Missing"
        ) |> 
        add_overall() |> 
        add_p() 
    
    gt_vitals <-
        asthmaster_df |> 
        select(
            Albuterol_Cont_Dose_Factor,
            Temp_Initial,
            HR_Initial,
            RR_Initial,
            O2_Initial,
            PASS_Initial,
            SBP_Initial,
            DBP_Initial
        ) |> 
        tbl_summary(
            by = "Albuterol_Cont_Dose_Factor",
            label = gt_summary_labels,
            missing_text = "Missing",
            digits = list(
                Temp_Initial ~ 1
            )
        ) |> 
        add_overall() |> 
        add_p() 
    
    gt_all <-
        tbl_stack(
            list(gt_demographics, gt_clinicals, gt_vitals),
            c("DEMOGRAPHICS", "CLINICAL FACTORS", "INITIAL VITAL SIGNS")
        ) |> 
        modify_spanning_header(
            update = c(stat_1, stat_2) ~ "**Initial Continuous Albuterol Dose**",
        ) |> 
        as_gt()
    
    
    gtsave(gt_all, "media/summary_table.html")
    
}