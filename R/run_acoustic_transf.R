#' main script that runs everything
#'
#' This function xxxxxxx
#'
#' @param xxxx param
#' @return xxxx return
#' @import tidyverse
#' @export
run_acoustic_transf = function(all_experiments, gen_anot = TRUE){
      
    cat("***** Reading in and processing experiment ng files... \n")
    all_experiments_df = bind_rows(lapply(all_experiments, function(exp){
                        
        # read in ng files for each experiment, and add a bunch of information (IDs for condition, reps, polytransf..etc)
        ng = process_ng_input(experiment_name = exp$name,
                              ng_dir = paste0(runs_dir,run_name,"/",exp$ng_csv_name),
                              n_replicates = exp$n_replicates,
                              num_cell_types = exp$n_cell_types)
        
        # join with database
        db = read_db(paste0(db_dir, exp$db_csv_name))
        ng_with_db = join_ng_and_db(ng, db)
        
        # subtract volumes from database
        cat("***** Subtracting volumes from databases... \n")
        db_subtract(exp_df = ng, 
                    db_df = db,
                    db_name = gsub(".*--","",gsub(".csv","",exp$db_csv_name)))
        
        return(ng_with_db)
                
    }))
    
    # add final destination well and write final output files
    cat("***** Generating echo instruction csv file... \n")
    echo = gen_echo_csv(all_experiments_df)
    
    # make annotation file
    if(gen_anot){
        cat("***** Generating experiment annotation file... \n")
        gen_exp_anot(echo)
    }
    
    return(echo)
}