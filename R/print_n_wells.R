#' check how many wells per experiment
#'
#' This function xxxxxxx
#'
#' @param xxxx param
#' @return xxxx return
#' @import tidyverse
#' @export
print_n_wells <- function(df){
    
    df %>% 
    select(exp_name, cond_id, rep_id) %>% 
    distinct %>% 
    group_by(exp_name) %>% 
    summarize(n_wells = n(), .groups = "drop") %>% 
    print(n=10000, width=10000)
    
    cat("\n")
    
}