#' builds a DB file, either from scratch or append to existing one
#'
#' This function xxxxxxx
#'
#' @param xxxx param
#' @return xxxx return
#' @import tidyverse
#' @export
build_db <- function(db_name, 
                     add_csv_name, 
                     verbose = TRUE,
                     db_dir = "/Users/andrewlu/Dropbox/Documents/Labowitz/Experiments/AL-AcousticTransf/data/DNA_libraries/",
                     add_dir = "/Users/andrewlu/Dropbox/Documents/Labowitz/Experiments/AL-AcousticTransf/data/DNA_library_additions/"){
    
    wells_384w = expand.grid(LETTERS[1:16],seq(1,24)) %>% arrange(Var1,Var2) %>% mutate(comb=paste0(Var1,Var2)) %>% pull(comb)
    
    # read all DBs in db_dir
    cur_dbs = tibble(dir = list.files(path = db_dir)) %>% 
    separate(col = dir, into = c("dt","name"), sep = "--", remove = FALSE) %>% 
    mutate(name = gsub(pattern = ".csv", replacement = "", x = name))
    # if(verbose) cur_dbs %>% print
    
    if(db_name %in% cur_dbs$name){

        # if there are mutliple db files for same database, get the database with most amount of plasmids (== most up to date)
        db_to_append_to = cur_dbs %>% 
        filter(name == db_name) %>% 
        mutate(n_plasmids = unname(sapply(X = dir, FUN = function(f) nrow(read_csv(paste0(db_dir,f),col_types=cols()))))) %>% 
        slice_max(n_plasmids)                                        
        if(nrow(db_to_append_to) != 1) stop("ERROR\n\n***Multiple database files with same number of plasmids..\n")

        # read in most up to date db
        old = read_csv(paste0(db_dir,db_to_append_to$dir), col_types = cols()) %>% 
                mutate(db_well = factor(db_well, levels = wells_384w))                                      
        old_max_well = old %>% slice_max(db_well) %>% pull(db_well) %>% as.character()
        old_max_well_i = match(old_max_well, wells_384w)

        # read in things to add to db, and add well annotations that start where the old one left off
        new = read_csv(paste0(add_dir,add_csv_name), col_types = cols()) %>% 
        mutate(db_well = wells_384w[(old_max_well_i+1) : (old_max_well_i+nrow(.))]) 

        final = bind_rows(old, new)
        

    } else{

        # make brand new db file from scratch    
        final = read_csv(paste0(add_dir,add_csv_name), col_types = cols()) %>% 
        mutate(db_well = wells_384w[1:nrow(.)])
    
    }
                                          
    if(verbose) print(final)
    
    safe_write_csv(df = final, 
                   dir_save = paste0(db_dir,db_name,".csv"), 
                   add_date_time=TRUE,
                   verbose=verbose)
    
}