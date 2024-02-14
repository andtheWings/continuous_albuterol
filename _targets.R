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
        "dplyr", "lubridate", "readr" # Data Wrangling
    )
) 

# End this file with a list of target objects.
list(
  
    # Asthmaster
    tar_target(
        name = asthmaster_csv,
        command = "data/RileyAsthmaster.csv",
        format = "file"
    ),
    tar_target(
        name = asthmaster_raw,
        command = parse_asthmaster(asthmaster_csv)
    ),
    tar_target(
        name = asthmaster_parquet,
        command = arrow::write_parquet(asthmaster_raw, "data/RileyAsthmaster.parquet")
    ),
    
    # Source Data
    tar_target(
        name = source_xlsx,
        command = "data/IRB#17481.xlsx",
        format = "file"
    ),
    tar_target(
        name = source_med_admin_raw,
        command = readxl::read_xlsx(source_xlsx, sheet = "MedAdmin")
    ),
    
    # Integration
    tar_target(
        name = asthmaster_mod,
        command = asthmaster_raw |> 
            add_albuterol_duration(source_med_admin_raw) |> 
            filter(Albuterol_Cont_Dose %in% c(10, 15)) |> 
            mutate(Albuterol_Cont_Dose = as.factor(Albuterol_Cont_Dose))
    )
    
)
