library(targets)

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.
sapply(
    paste0("R/", list.files("R/")),
    source
)

# Set target-specific options such as packages:
tar_option_set(
    packages = c(
        "dplyr", "lubridate", "purrr", "readr", # Data Wrangling
        "parsnip", "ranger", "recipes", "rsample", "tune", "workflows", # Modeling
        "broom", "parameters", "performance",
        "ggplot2", "gt", "gtsummary", "patchwork" # Results Reporting
    )
) 

# End this file with a list of target objects.
list(
  
    # Asthmaster
    tar_target(
        asthmaster_csv,
        "data/RileyAsthmaster.csv",
        format = "file"
    ),
    tar_target(
        asthmaster_raw,
        parse_asthmaster(asthmaster_csv)
    ),
    tar_target(
        asthmaster_parquet,
        arrow::write_parquet(asthmaster_raw, "data/RileyAsthmaster.parquet")
    ),
    
    # Source Data
    tar_target(
        source_xlsx,
        "data/IRB#17481.xlsx",
        format = "file"
    ),
    tar_target(
        source_med_admin_raw,
        readxl::read_xlsx(source_xlsx, sheet = "MedAdmin")
    ),
    
    # Integration
    tar_target(
        asthmaster_modified,
        wrangle_asthmaster_modified(asthmaster_raw, source_med_admin_raw)
    ),
    
    # Data for modeling
    ## Dataset containing just covariates of interest and the outcome without missingness
    tar_target(
        asthmaster_compact,
        asthmaster_modified |>
            select(
                FIN,
                Albuterol_Cont_Dose_Factor,
                Year,
                AgeAtEncounter,
                Race_Black,
                Sex,
                Ipratropium,
                PneumoniaDx,
                ChronicLungdzDx,
                CCC,
                BPD,
                HR_Initial,
                RR_Initial,
                O2_Initial,
                Temp_Initial,
                Resp_Initial
            ) |>
            na.omit()
    ),
    ## Splitting the data
    tar_target(
        asthmaster_split,
        initial_split(
            asthmaster_compact,
            prop = 0.75,
            strata = Albuterol_Cont_Dose_Factor
        )
    ),
    tar_target(
        asthmaster_train,
        training(asthmaster_split)
    ),
    tar_target(
        asthmaster_test,
        testing(asthmaster_split)
    ),
    # Making bootstrap samples for tuning and comparing performance
    tar_target(
        asthmaster_boot,
        bootstraps(asthmaster_train)
    ),

    # Modeling IPW
    ## Engine Specifications
    # tar_target(
    #     engine_logistic,
    #     logistic_reg(
    #         mode = "classification",
    #         engine = "glm"
    #     )
    # ),
    tar_target(
        engine_rf,
        rand_forest(
            mode = "classification",
            engine = "ranger",
            trees = 500,
            mtry = tune(),
            min_n = tune()
        )
    ),
    ## Formulas
    # tar_target(
    #     formula_ipw_all,
    #     formula("Albuterol_Cont_Dose_Factor ~ . - FIN")
    # ),
    tar_target(
        formula_ipw_subset,
        formula("Albuterol_Cont_Dose_Factor ~ Year + AgeAtEncounter + Race_Black + HR_Initial + RR_Initial")
    ),
    ## Fitting candidates on resamples and tuning hyperparameters
    # tar_target(
    #     tunes_ipw_all_logistic,
    #     workflow() |>
    #         add_formula(formula_ipw_all) |>
    #         add_model(engine_logistic) |>
    #         fit_resamples(
    #             resamples = asthmaster_boot,
    #             control = control_resamples(save_pred = TRUE)
    #         )
    # ),
    # tar_target(
    #     tunes_ipw_subset_logistic,
    #     workflow() |>
    #         add_formula(formula_ipw_subset) |>
    #         add_model(engine_logistic) |>
    #         fit_resamples(
    #             resamples = asthmaster_boot,
    #             control = control_resamples(save_pred = TRUE)
    #         )
    # ),
    # tar_target(
    #     tunes_ipw_all_rf,
    #     workflow() |>
    #         add_formula(formula_ipw_all) |>
    #         add_model(engine_rf) |>
    #         tune_grid(
    #             resamples = asthmaster_boot,
    #             grid = 25
    #         )
    # ),
    tar_target(
        tunes_ipw_subset_rf,
        workflow() |>
            add_formula(formula_ipw_subset) |>
            add_model(engine_rf) |>
            tune_grid(
                resamples = asthmaster_boot,
                grid = 25
            )
    ),
    ## Extracting tuned engine specification
    tar_target(
        engine_rf_ipw_tuned,
        finalize_model(
            engine_rf,
            parameters = select_best(tunes_ipw_subset_rf, metric = "roc_auc")
        )
    ),
    ## Fitting best model
    tar_target(
        final_model_ipw,
        workflow() |>
            add_formula(formula_ipw_subset) |>
            add_model(engine_rf_ipw_tuned) |>
            last_fit(asthmaster_split)
    ),
    ## Adding IPW's back to original data
    tar_target(
        asthmaster_ipw,
        final_model_ipw |>
            add_ipw(asthmaster_compact, asthmaster_modified)
    ),
    
    # Modeling Outcomes
    ## PASS 
    tar_target(
        model_pass_ipw_stable,
        lm(
            PASSchange0_24_percent ~ Albuterol_Cont_Dose_Logit,
            data = asthmaster_ipw,
            weights = ipw_stabilized,
            na.action = na.exclude
        )
    ),
    ## Length Continuous Albuterol
    tar_target(
        model_len_albuterol_ipw_stable,
        MASS::glm.nb(
            Albuterol_Cont_discrete_hrs ~ Albuterol_Cont_Dose_Logit,
            data = asthmaster_ipw,
            na.action = na.exclude,
            weights = ipw_stabilized
        )
    ),
    ## Length PICU
    tar_target(
        model_len_picu_ipw_stable,
        MASS::glm.nb(
            PICU.LOS.VPS_discrete_hrs ~ Albuterol_Cont_Dose_Logit,
            data = asthmaster_ipw,
            na.action = na.exclude,
            weights = ipw_stabilized
        )
    ),
    ## Length Hospital
    tar_target(
        model_len_hospital_ipw_stable,
        MASS::glm.nb(
            Hospital_LOS_discrete_hrs ~ Albuterol_Cont_Dose_Logit,
            data = asthmaster_ipw,
            na.action = na.exclude,
            weights = ipw_stabilized
        )
    ),
    
    # Results
    ## Summary Stats
    tar_target(
        table_summary_stats,
        tabulate_summary_stats(asthmaster_modified)
    ),
    ## Yearly Percentages
    tar_target(
        plotted_yearly_percentages,
        plot_yearly_percentages(asthmaster_modified)
    ),
    ## Plot Outcomes
    tar_target(
        plotted_outcomes_together,
        plot_outcomes_together(asthmaster_modified)
    ),
    ## Treatment Effects
    tar_target(
        table_treatment_effects,
        tabulate_treatment_effects(
            model_pass_ipw_stable,
            model_len_albuterol_ipw_stable,
            model_len_picu_ipw_stable,
            model_len_hospital_ipw_stable
        )
    )
)
