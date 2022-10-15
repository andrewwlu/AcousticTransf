#' makes a list of experiment parameters
#'
#' This function xxxxxxx
#'
#' @param xxxx param
#' @return xxxx return
#' @import tidyverse
#' @export
new_exp = function(name, 
                   ng_csv_name, 
                   db_csv_name, 
                   n_replicates, 
                   n_cell_types){
    
    list(name = name, 
         ng_csv_name = ng_csv_name, 
         db_csv_name = db_csv_name, 
         n_replicates = n_replicates,
         n_cell_types = n_cell_types)
    
}
