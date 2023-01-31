#' xx
#'
#' This function xxxxxxx
#'
#' @param xxxx param
#' @return xxxx return
#' @import tidyverse
#' @export
get_list_of_list_of_wells = function(pt){
    
    # just hardcoding 10x plates (i don't think I will go above this limit any time soon)
    
    if (pt == "24w"){
        
        wells_24 = get_plate_wells(n_rows = 4, n_cols = 6)
        return(list(wells_24,wells_24,wells_24,wells_24,wells_24,wells_24,wells_24,wells_24,wells_24,wells_24))
        
    } else if (pt == "48w"){
        
        wells_48 = get_plate_wells(n_rows = 6, n_cols = 8)
        return(list(wells_48,wells_48,wells_48,wells_48,wells_48,wells_48,wells_48,wells_48,wells_48,wells_48))
        
    } else if (pt == "96w"){
        
        wells_96 = get_plate_wells(n_rows = 8, n_cols = 12)
        return(list(wells_96,wells_96,wells_96,wells_96,wells_96,wells_96,wells_96,wells_96,wells_96,wells_96))
        
    } else if (pt == "24w_setup_in_96w_4x"){
        
        return(list(get_plate_wells(n_rows = 4, n_cols = 6),
                    get_plate_wells(n_rows = 4, n_cols = 6, rows_offset = 0, cols_offset = +6),
                    get_plate_wells(n_rows = 4, n_cols = 6, rows_offset = +4, cols_offset = 0),
                    get_plate_wells(n_rows = 4, n_cols = 6, rows_offset = +4, cols_offset = +6)))
        
    } else{
        stop("ERROR\n\n****UNRECOGNIZED PLATE TYPE")
    }
    
    
}