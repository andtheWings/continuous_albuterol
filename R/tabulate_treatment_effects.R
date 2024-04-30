tabulate_treatment_effects <- 
    function(
        model_pass_ipw_stable_obj,
        model_len_albuterol_ipw_stable_obj,
        model_len_picu_ipw_stable_obj,
        model_len_hospital_ipw_stable_obj
    ) {
        pass_result <-
            model_pass_ipw_stable_obj |> 
            parameters() |> 
            as_tibble() |>
            filter(Parameter == "Albuterol_Cont_Dose_LogitTRUE") |> 
            mutate(
                label = "PASS (% Change at 24 hrs)",
                model_type = "Simple Linear",
                effect_type = "Additive"
            ) 
        
        albuterol_result <-
            model_len_albuterol_ipw_stable_obj |> 
            parameters(exponentiate = TRUE) |> 
            as_tibble() |> 
            filter(Parameter == "Albuterol_Cont_Dose_LogitTRUE") |>
            mutate(
                label = "Length of Continuous Albuterol",
                model_type = "Negative Binomial",
                effect_type = "Multiplicative"
            )
        
        picu_result <-
            model_len_picu_ipw_stable_obj |> 
            parameters(exponentiate = TRUE) |> 
            as_tibble() |> 
            filter(Parameter == "Albuterol_Cont_Dose_LogitTRUE") |>
            mutate(
                label = "Length of PICU Stay",
                model_type = "Negative Binomial",
                effect_type = "Multiplicative"
            )
        
        hospital_result <-
            model_len_hospital_ipw_stable_obj |> 
            parameters(exponentiate = TRUE) |> 
            as_tibble() |> 
            filter(Parameter == "Albuterol_Cont_Dose_LogitTRUE") |>
            mutate(
                label = "Length of Hospital Stay",
                model_type = "Negative Binomial",
                effect_type = "Multiplicative"
            )
        
        table1 <-
            reduce(
                list(pass_result, albuterol_result, picu_result, hospital_result),
                bind_rows
            ) |> 
            mutate(
                Coefficient = round2(Coefficient, 2),
                p = case_when(
                    p < 0.001 ~ "<0.001",
                    TRUE ~ as.character(round2(p, 3))
                )
            ) |> 
            select(
                Outcome = label,
                `Effect` = Coefficient,
                `p-value` = p,
                `Effect Type` = effect_type,
                `Model Type` = model_type,
            ) |> 
            gt::gt() |> 
            tab_header("Treatment Effect of 15 mg/hr Initial Dose (vs. 10 mg/hr)") |> 
            tab_options(
                column_labels.font.weight = "bold"
            )
        
        gtsave(table1, "media/treatment_effects_table.html")
    }