#' runs checks
#'
#' This function xxxxxxx
#'
#' @param xxxx param
#' @return xxxx return
#' @import tidyverse
#' @export
run_checks = function(all_experiments, check_db, save_plasmids_needed_csv){
    
    plasmids_needed = tibble()
    
    for(exp in all_experiments){
        
        cat(paste0("****",exp$name,"****\n"))
        cat(paste0("ng_csv_name: ", exp$ng_csv_name, "\n"))
        cat(paste0("db_csv_name: ", exp$db_csv_name, "\n"))
        cat(paste0("n_replicates: ", exp$n_replicates, "\n"))
        cat(paste0("n_cell_types: ", exp$n_cell_types, "\n"))
        
        # read in ng file for this experiment
        ng = process_ng_input(experiment_name = exp$name,
                              ng_dir = paste0(runs_dir,run_name,"/",exp$ng_csv_name),
                              n_replicates = exp$n_replicates,
                              num_cell_types = exp$n_cell_types)
        
        # check number of wells
        check_n_wells(ng) %>% print
        
        # check volumes of nucleic acid needed
        dnavols = check_dna_vol(ng)
        plasmids_needed = bind_rows(plasmids_needed, dnavols)
        
        # check that there is enough volume in DB
        if(check_db){
            db_subtract(exp_df = ng, 
                        db_df = read_db(paste0(db_dir, exp$db_csv_name)),
                        db_name = "run_checks-this_shouldnt_be_saved_anywhere",
                        just_checking = TRUE)
        }
        cat("\n\n")

    }
    
    if(save_plasmids_needed_csv) safe_write_csv(plasmids_needed, paste0(runs_dir,run_name,"/",run_name,"-plasmids-needed.csv"), add_date_time=FALSE, verbose=TRUE)
    
    cat("**** Found no issues! Proceed to next step.. ****\n")
    
}