#' gets plate wells
#'
#' This function xxxxxxx
#'
#' @param xxxx param
#' @return xxxx return
#' @import tidyverse
#' @export
get_plate_wells = function(n_rows, n_cols, rows_offset = 0, cols_offset = 0){
    
    expand.grid(LETTERS[(1+rows_offset) : (n_rows+rows_offset)],
                seq(1+cols_offset, n_cols+cols_offset)) %>% 
    arrange(Var1,Var2) %>%
    mutate(comb=paste0(Var1,Var2)) %>%
    pull(comb)

}