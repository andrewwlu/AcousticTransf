#' subtracts out volumes used during experiment and saves a new DB
#'
#' This function xxxxxxx
#'
#' @param xxxx param
#' @return xxxx return
#' @import tidyverse
#' @export
db_subtract <- function(exp_df, db_df, v_subtract_multiplier=1.1, db_name, just_checking=FALSE){
    
    wells_384w = expand.grid(LETTERS[1:16],seq(1,24)) %>% arrange(Var1,Var2) %>% mutate(comb=paste0(Var1,Var2)) %>% pull(comb)
    
    tot_v_need = exp_df %>% 
    select(dna_id, dna_desc, dna_conc, vol_transfer) %>% 
    group_by(dna_id, dna_desc, dna_conc) %>% 
    summarize(total_v_needed = sum(vol_transfer) / 1000, .groups = "drop")
        
    new_db = full_join(db_df, tot_v_need, by = c("dna_id", "dna_desc", "dna_conc")) %>%
    select(-source_plate) %>% 
    
    # if plasmid not used (= NA), make it 0
    mutate(total_v_needed = ifelse(is.na(total_v_needed), 0, total_v_needed)) %>% 
    
    # subtract out used volumes * multiplier
    mutate(db_ul = floor(db_ul - total_v_needed * v_subtract_multiplier)) %>% select(-total_v_needed) %>% 
    
    # arrange wells
    mutate(db_well = factor(db_well, levels = wells_384w)) %>% 
    arrange(db_well) 
    
    if(min(new_db$db_ul) <= 22.5){
        cat("\n\n\n******ERROR\n\nNOT ENOUGH PLASMIDS!\n\nDatabase is going to have volumes less than the allowed Echo dead volume (22.5 uL)\n")
        print(new_db %>% filter(db_ul <= 22.5))
        stop("ERROR")
    } 
    
    if(just_checking){
        return(NULL)
    }
    
    if(write_files){
        safe_write_csv(df = new_db, 
                   dir_save = paste0(db_dir,db_name,".csv"), 
                   add_date_time=TRUE,
                   verbose=TRUE)
    }
}




# ng %>% 




# #### check that there is enough volume of DNA in specified database
# if(min(new_db$dna_ul) <= 25){
    
#     print(new_db)
#     stop("ERROR\n\n***NOT ENOUGH DNA VOLUME IN DATABASE\n")
# }

# new_db

# if(save_new_db){
#      safe_write_csv(df = new_db, 
#                    dir_save = paste0(dirname(db_dir),"/",unique(db$SourcePlate),".csv"), 
#                    add_date_time = TRUE,
#                    verbose = TRUE)
# }