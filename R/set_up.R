#' sets up experiment variables
#'
#' This function xxxxxxx
#'
#' @param xxxx param
#' @return xxxx return
#' @import tidyverse
#' @export
set_up <- function(runs_dir = "/Users/andrewlu/Dropbox/Documents/Labowitz/Experiments/AL-AcousticTransf/runs/",
                   run_name,
                   db_dir = "/Users/andrewlu/Dropbox/Documents/Labowitz/Experiments/AL-AcousticTransf/data/DNA_libraries/",
                   transf_plate_type,
                   write_files){
    
    cat("\n****Welcome to AcousticTransf, an R package written by Andrew Lu for high-throughput Echo-based acoustic liquid handling transfections.\n\n")
    
    runs_dir <<- runs_dir
    cat(paste0("Home directory for all echo run notebooks: ", runs_dir, "\n"))
    
    run_name <<- run_name
    cat(paste0("Name of Echo run (folder must have matching name): ", run_name, "\n"))
    
    db_dir <<- db_dir
    cat(paste0("Home directory for all nucleic acid databases: ", db_dir, "\n"))
    
    transf_plate_type <<- transf_plate_type
    cat(paste0("Transfection plate type: ", transf_plate_type, "\n"))
    
    write_files <<- write_files
    cat(paste0("\n**** ",
               ifelse(write_files, 
                      "BE CAREFUL - WRITING OUTPUT FILES ENABLED!", 
                      "TESTING MODE - NO OUTPUT FILES WILL BE WRITTEN"),
               " ****\n"))
        
}


# db_csv_name <<- db_csv_name
# cat(paste0("Name of database csv file (name must contain .csv): ", db_csv_name, "\n"))
# ng_csv_name <<- ng_csv_name
# cat(paste0("Name of ng csv file (name must contain .csv): ", ng_csv_name, "\n"))

# exp_name = *this should be name of folder*
# exp_date = 
# ng_csv_name = 
# db_dir = *WITH DEFAULT VALUE*
# db_name = 
# transf_plate_type