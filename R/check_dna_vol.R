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
        
    # dna_id is a factor column that I already ordered based on the original ng file order, so here i'm just rearranging back to og order
    arrange(dna_id) 
        
}