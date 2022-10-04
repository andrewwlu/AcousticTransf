#' check DNA volumes
#'
#' This function xxxxxxx
#'
#' @param xxxx param
#' @return xxxx return
#' @import tidyverse
#' @export
checkDNAvol <- function(ng_csv_dir){
    
    read_csv(ng_csv_dir, col_types = cols()) %>% 
    pivot_longer(!c("dna_id","dna_desc","polytransf_id","dna_conc"), values_to = "ng") %>% 
    filter(!is.na(ng)) %>% 
    mutate(transf_ul = ng / dna_conc) %>% 
    group_by(dna_id, dna_desc) %>% 
    summarize(total_ul = sum(transf_ul), .groups = "drop") %>% 
    arrange(desc(total_ul)) %>% 
    print
    
}