#' check DNA volumes
#'
#' This function xxxxxxx
#'
#' @param xxxx param
#' @return xxxx return
#' @import tidyverse
#' @export
check_dna_vol <- function(exp_df){
    
    exp_df %>% 
    group_by(exp_name, dna_id, dna_desc, dna_conc) %>% 
    summarize(total_ul_needed = sum(vol_transfer) / 1000, .groups = "drop") %>% 
    arrange(desc(total_ul_needed))
    
}