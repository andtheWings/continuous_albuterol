tabyl_modified <- function(a_tbl_df, a_var) {
    
    a_tbl_df |> 
        janitor::tabyl( {{ a_var}} ) |> 
        select( {{ a_var }}, n, percent) |> 
        mutate(percent = round(percent*100, 1))
    
}