#' given db and experiment dataframes, generate echo instructions csv
#'
#' This function xxxxxxx
#'
#' @param xxxx param
#' @return xxxx return
#' @import tidyverse
#' @export
gen_echo_csv <- function(exp_df){
        
    get_plate_wells = function(n_rows, n_cols, rows_offset = 0, cols_offset = 0) expand.grid(LETTERS[(1+rows_offset):(n_rows+rows_offset)],seq(1+cols_offset,n_cols+cols_offset)) %>% arrange(Var1,Var2) %>% mutate(comb=paste0(Var1,Var2)) %>% pull(comb)

    # print(exp_df)
    # print(db_df)
    
    if (transf_plate_type == "96w") {
        list_of_list_of_wells = list(expand.grid(LETTERS[1:8],seq(1,12)) %>% arrange(Var1,Var2) %>% mutate(comb=paste0(Var1,Var2)) %>% pull(comb))
    } else if (transf_plate_type == "24w") {
        list_of_list_of_wells = list(get_plate_wells(n_rows = 4, n_cols = 6),
                                     get_plate_wells(n_rows = 4, n_cols = 6, rows_offset = 0, cols_offset = +6),
                                     get_plate_wells(n_rows = 4, n_cols = 6, rows_offset = +4, cols_offset = 0),
                                     get_plate_wells(n_rows = 4, n_cols = 6, rows_offset = +4, cols_offset = +6))    
    } else if(transf_plate_type == "48w"){
        list_of_list_of_wells = list(expand.grid(LETTERS[1:6],seq(1,8)) %>% arrange(Var1,Var2) %>% mutate(comb=paste0(Var1,Var2)) %>% pull(comb))
    } else{
        stop("ERROR\n\n****UNRECOGNIZED PLATE TYPE")
    }
    
    
    ##### make dataframe
    well_i = 1
    plate_i = 1
    
    with_wells = exp_df %>% 
    
    # add destination well id
    group_by(exp_name, cond_id, rep_id) %>% 
    group_modify(function(tib,key){
                
        newtib = tib %>% 
        mutate(destination_well = list_of_list_of_wells[[plate_i]][well_i])
        
        well_i <<- well_i + 1
        
        # if next well doesn't exist, switch p=lates
        if(is.na(list_of_list_of_wells[[plate_i]][well_i])){
            plate_i <<- plate_i + 1
            well_i <<- 1
            print(paste0("** going to destination plate ", as.character(plate_i)))
        }
        return(newtib)
        
    }) %>% ungroup 
    
    # print(with_wells)
    
    # print(paste0("next plate: ", plate_i))
    # print(paste0("next well: ", well_i))
    
    if(write_files){
        safe_write_csv(with_wells, paste0(runs_dir,run_name,"/",run_name,"-experiment-all-info.csv"), add_date_time=FALSE, verbose=TRUE)
    }
    # clean up column names and write to csv
    echo_clean = with_wells %>% 
    select(source_plate, db_well, polytransf_id, destination_well, vol_transfer) %>% 
    mutate(vol_transfer = round(vol_transfer,0)) %>% 
    rename("Source Plate Name" = source_plate,
           "Source Well" = db_well,
           "Destination Well" = destination_well,
           "Transfer Volume" = vol_transfer) %>% 
    mutate("Destination Plate Name" = paste0("transf-plate-",polytransf_id)) %>% select(-polytransf_id) %>% 
    select("Source Plate Name", "Source Well", "Destination Plate Name", "Destination Well", "Transfer Volume")
    
    if(write_files){
        safe_write_csv(echo_clean, paste0(runs_dir,run_name,"/",run_name,"-echo-instructions.csv"), add_date_time=FALSE, verbose=TRUE)
    }
    
    return(with_wells)

}


# Piece Name	Assembly Name	Source Plate Name	Source Plate Type	Source Well	Sample ID	Sample Name	Sample Group	Sample Comment	Destination Plate Name	Destination Well	Transfer Volume

 # echo_csv_save_dir = paste0(exp_dir, exp_date, "-echo-instructions.csv"), 
             # info_csv_save_dir = paste0(exp_dir, exp_date, "-experiment-all-info.csv")