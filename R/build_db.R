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
                     add_dir = "/Users/andrewlu/Dropbox/Documents/Labowitz/Experiments/AL-AcousticTransf/data/DNA_library_additions/",
                     skip_wells = FALSE){
    
    wells_384w = expand.grid(LETTERS[1:16],seq(1,24)) %>% arrange(Var1,Var2) %>% mutate(comb=paste0(Var1,Var2)) %>% pull(comb)
    
    if(skip_wells) wells_384w = wells_384w[c(TRUE, FALSE)]
    
    # read all DBs in db_dir
    cur_dbs = tibble(dir = list.files(path = db_dir)) %>% 
    separate(col = dir, into = c("dt","name"), sep = "--", remove = FALSE) %>% 
    mutate(name = gsub(pattern = ".csv", replacement = "", x = name))
    # if(verbose) cur_dbs %>% print
    
    if(db_name %in% cur_dbs$name){

        # sort databases with this name by year, then month, then day, then time
        db_to_append_to = cur_dbs %>% 
        filter(name == db_name) %>% 
        separate(col = dt, into = c("year","month","day","time"), sep = "-", remove = FALSE) %>% 
        # select(year, month, day, time) %>% 
        arrange(desc(year), desc(month), desc(day), desc(time)) %>% 
        slice_head(n = 1)
        
        cat(paste0("\nFound existing database with the same name!\nAppending to it now: ",db_to_append_to$dir,"\n\n"))
        
        ##### this is dumb: there can be multiple databases (new -- old) that have same amount of plasmids
        # # if there are mutliple db files for same database, get the database with most amount of plasmids (== most up to date)
        # db_to_append_to = cur_dbs %>% 
        # filter(name == db_name) %>% 
        # mutate(n_plasmids = unname(sapply(X = dir, FUN = function(f) nrow(read_csv(paste0(db_dir,f),col_types=cols()))))) %>% 
        # slice_max(n_plasmids)                                        
        # if(nrow(db_to_append_to) != 1) stop("ERROR\n\n***Multiple database files with same number of plasmids..\n")

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
    
    # check that DB has unique rows
    unique_rows_df = final %>% 
    group_by(dna_id, dna_desc, dna_conc) %>% 
    summarize(n_rows = n(), .groups = "drop")
    if(max(unique_rows_df$n_rows)>1){
        print(unique_rows_df %>% filter(n_rows > 1))
        stop("ERROR\n\n***Database contains non-unique rows..\n")
    }
                                          
    safe_write_csv(df = final, 
                   dir_save = paste0(db_dir,db_name,".csv"), 
                   add_date_time=TRUE,
                   verbose=verbose)
    
}