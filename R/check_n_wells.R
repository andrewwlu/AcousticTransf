#' check how many wells per experiment
#'
#' This function xxxxxxx
#'
#' @param xxxx param
#' @return xxxx return
#' @import tidyverse
#' @export
check_n_wells <- function(exp_df){
    
    exp_df %>% 
    select(exp_name, cond_id, rep_id) %>% 
    distinct %>% 
    group_by(exp_name) %>% 
    summarize(n_wells = n(), .groups = "drop")
    
}