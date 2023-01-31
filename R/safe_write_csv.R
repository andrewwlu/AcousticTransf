#' safely writes csv (doesn't overwrite existing files)
#'
#' This function xxxxxxx
#'
#' @param xxxx param
#' @return xxxx return
#' @import tidyverse
#' @export
safe_write_csv <- function(df, dir_save, verbose=FALSE){
    
    # if(add_date_time){
    #     dt = paste0(Sys.Date(),format(Sys.time(), "-%I%M%S%p--"))
    #     dir_save = paste0(dirname(dir_save),"/",dt,basename(dir_save))
    # }
    
    if(file.exists(dir_save)){
        stop("ERROR\n\nFILE ALREADY EXISTS!\n\nFor database safety reasons, there is no overriding of files allowed\n")
    }
    
    if(verbose) cat(paste0("FILE SAVING TO: ", dir_save,'\n\n'))
    
    write_csv(df, dir_save)
    
}