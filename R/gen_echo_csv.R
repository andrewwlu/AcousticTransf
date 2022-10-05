#' given db and experiment dataframes, generate echo instructions csv
#'
#' This function xxxxxxx
#'
#' @param xxxx param
#' @return xxxx return
#' @import tidyverse
#' @export
gen_echo_csv <- function(exp_df, db_df, transf_plate_type, echo_csv_save_dir, info_csv_save_dir){
        
    # print(exp_df)
    # print(db_df)
    
    if (transf_plate_type == "96w") {
        list_of_wells = expand.grid(LETTERS[1:8],seq(1,12)) %>% arrange(Var1,Var2) %>% mutate(comb=paste0(Var1,Var2)) %>% pull(comb)
    } else if (transf_plate_type == "24w") {
        list_of_wells = expand.grid(LETTERS[1:4],seq(1,6)) %>% arrange(Var1,Var2) %>% mutate(comb=paste0(Var1,Var2)) %>% pull(comb)
    } else{
        stop("ERROR\n\n****UNRECOGNIZED PLATE TYPE")
    }
    
    
    ##### make dataframe
    well_i = 1
    joined = exp_df %>% 
    # join database
    left_join(db, by=c("dna_id","dna_desc","dna_conc")) %>% 
    # add destination well id
    group_by(exp_name, cond_id, rep_id) %>% 
    group_modify(function(tib,key){
        newtib = tib %>% mutate(destination_well = list_of_wells[well_i])
        well_i <<- well_i + 1
        return(newtib)
    }) %>% ungroup 
    
    # print(joined)
    
    ###### check that all plasmids needed are in DB
    missing_in_db = joined %>% 
    filter(is.na(db_ul) | is.na(db_well) | is.na(source_plate))
    if(nrow(missing_in_db) != 0){
        cat("\n\n")
        stop(paste0("ERROR\n\n**** SOME PLASMIDS THAT ARE NEEDED ARE NOT FOUND IN DATABASE! i.e. ", paste0(unique(missing_in_db$dna_id), collapse = " and "), "\n"))
    }    
    
    safe_write_csv(joined, info_csv_save_dir, add_date_time=FALSE, verbose=TRUE)
    
    # clean up column names and write to csv
    echo_clean = joined %>% 
    select(source_plate, db_well, polytransf_id, destination_well, vol_transfer) %>% 
    rename(SourcePlate = source_plate,
           SourceWell = db_well,
           DestinationWell = destination_well,
           VolTransfer = vol_transfer) %>% 
    mutate(DestinationPlate = paste0("transf-plate-",polytransf_id)) %>% select(-polytransf_id) %>% 
    mutate(VolTransfer = round(VolTransfer,0)) %>% 
    select(SourcePlate, SourceWell, DestinationPlate, DestinationWell, VolTransfer)
    
    safe_write_csv(echo_clean, echo_csv_save_dir, add_date_time=FALSE, verbose=TRUE)
    
    return(joined)

}