#' reads in DNA database into dataframe
#'
#' This function xxxxxxx
#'
#' @param xxxx param
#' @return xxxx return
#' @import tidyverse
#' @export
read_db <- function(db_dir){
        
    db = read_csv(db_dir, col_types = cols()) %>% 
    
    # remove rows with empty DNA concentrations
    filter(!is.na(dna_conc)) %>%
    
    # use csv name to name source plate
    mutate(source_plate = gsub(".csv","",basename(db_dir))) 
        
    # check that there aren't multiple rows of the same dna_desc and dna_conc
    if(db %>% 
    group_by(dna_id, dna_desc, dna_conc) %>% 
    summarize(n_wells = n(), .groups = "drop") %>% 
    pull(n_wells) %>%
    max != 1){
        stop("ERROR\n\n***MULTIPLE IDENTICAL ROWS (same dna_id, dna_desc, dna_conc combo) in database\n\n")
    }
    
    return(db)

}