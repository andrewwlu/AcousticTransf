#' joins the ng and db DFs, and checks / resolves any conflicts
#'
#' This function xxxxxxx
#'
#' @param xxxx param
#' @return xxxx return
#' @import tidyverse
#' @export
join_ng_and_db <- function(ng, db){
    
    joined = left_join(ng, db, by=c("dna_id","dna_desc","dna_conc"))
    
    ###### check that DB has only unique entries
    max_entries = db %>% 
    group_by(dna_id, dna_desc, dna_conc) %>% 
    summarize(n_entries = n(), .groups = "drop") %>% 
    pull(n_entries) %>% max
    if(max_entries > 1) stop("\n\nERROR\n\n**** MULTIPLE, NON-UNIQUE ENTRIES IN DATABASE! i.e. ")
    
    ###### check that all plasmids needed are in DB
    missing_in_db = joined %>% filter(is.na(db_ul) | is.na(db_well) | is.na(source_plate))
    if(nrow(missing_in_db) != 0) stop(paste0("\n\nERROR\n\n**** SOME PLASMIDS THAT ARE NEEDED ARE NOT FOUND IN DATABASE! i.e. ", paste0(unique(missing_in_db$dna_id), collapse = " and "), "\n"))
    
    return(joined %>% select(-db_ul))
    
}