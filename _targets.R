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
        "dplyr", "readr" # Data Wrangling
    )
) 

# End this file with a list of target objects.
list(
  
    tar_target(
        name = riley_asthmaster_csv,
        command = "data/RileyAsthmaster.csv",
        format = "file"
    ),
    tar_target(
        name = riley_asthmaster_raw,
        command = parse_asthmaster(riley_asthmaster_csv)
    ),
    tar_target(
        name = riley_asthmaster_parquet,
        command = arrow::write_parquet(riley_asthmaster_raw, "data/RileyAsthmaster.parquet")
    )
    
)
