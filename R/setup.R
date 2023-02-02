#' sets up experiment variables
#'
#' This function xxxxxxx
#'
#' @param xxxx param
#' @return xxxx return
#' @import tidyverse
#' @export
setup <- function(home_dir, 
                  run_name, 
                  ng_csv, 
                  n_reps, 
                  n_celltypes,
                  source_plate_max_v, 
                  source_plate_dv,
                  dna_echo_conc,
                  dest_plate_type){
    
    options(width = 260)
    
    cat("\n****Welcome to AcousticTransf, an R package written by Andrew Lu for high-throughput Echo-based acoustic liquid handling transfections.\n\n")
    
    home_dir <<- home_dir
    cat(paste0("Home directory: ", home_dir, "\n"))
    
    run_name <<- run_name
    cat(paste0("Name of Echo run (folder must have matching name): ", run_name, "\n"))
    
    ng_csv <<- ng_csv
    cat(paste0("Name of ng input csv file: ", ng_csv, "\n"))
    
    n_reps <<- n_reps
    cat(paste0("Number of replicates per experimental condition: ", n_reps, "\n"))
    
    n_celltypes <<- n_celltypes
    cat(paste0("Number of cell types: ", n_celltypes, "\n"))
    
    source_plate_max_v <<- source_plate_max_v
    cat(paste0("Source plate max volume: ", source_plate_max_v, "\n"))
    
    source_plate_dv <<- source_plate_dv
    cat(paste0("Source plate dead volume: ", source_plate_dv, "\n"))
    
    dna_echo_conc <<- dna_echo_conc
    cat(paste0("Concentration of DNA that the echo is going to transfer: ", dna_echo_conc, " ng/ul\n"))
    
    dest_plate_type <<- dest_plate_type
    cat(paste0("Transfection destination plate type: ", dest_plate_type, "\n\n****\n\n"))
        
}
