#' generates echo instructions csv
#'
#' This function xxxxxxx
#'
#' @param xxxx param
#' @return xxxx return
#' @import tidyverse
#' @export
gen_echo_csv <- function(df){
        
    # print(df)
    
    list_of_list_of_wells = get_list_of_list_of_wells(dest_plate_type)    
    
    ##### make dataframe
    well_i = 1
    plate_i = 1
    
    with_wells = df %>% 
    
    mutate(dest_plate_type = dest_plate_type) %>% 
    
    # add destination well id
    group_by(exp_name, cond_id, rep_id) %>% 
    group_modify(function(tib,key){
        
        # print(key)
        # print(tib, width = 10000)
        
        newtib = tib %>% 
        mutate(destination_well = list_of_list_of_wells[[plate_i]][well_i]) %>% 
        mutate(dest_plate_id = plate_i)
        
        well_i <<- well_i + 1
        
        # if next well doesn't exist, switch plates
        if(is.na(list_of_list_of_wells[[plate_i]][well_i])){
            plate_i <<- plate_i + 1
            well_i <<- 1
            print(paste0("** if there is a next well, the next well will be on a new plate: ", as.character(plate_i)))
        }
        # cat("\n\n")
        return(newtib)
        
        
    }) %>% ungroup 
    
    # print(with_wells, width=10000, n=1000)
    # cat("\n\n\n\n")
    
    ### I TESTED: ECHO IS OKAY WITH VOLUMES > 5 UL, I THINK IT DOES THE SPLITTING UP ITSELF
#     # split up transfers that exceed max echo transfer limit
#     with_wells = with_wells %>% 
#     mutate(transfer_id = seq(1, nrow(.))) %>% 
#     group_by(transfer_id) %>% 
#     group_modify(function(tib,key){
        
#         if(nrow(tib)>1) stop("ERROR\n\n**MORE THAN ONE ROW WHEN SPLITTING UP TRANSFERS THAT EXCEED MAX ECHO TRANSFER LIMIT")
        
#         if(tib$vol_transfer > 5000){
            
#             n_times_split = ceiling(tib$vol_transfer/5000)

#             newtib = tib %>% 
#             slice(rep(1, each = n_times_split)) %>% 
#             mutate(ng_split = round(tib$ng_split / n_times_split)) %>% 
#             mutate(vol_transfer = round(tib$vol_transfer / n_times_split))
            
#             return(newtib)
            
#         } else{
            
#             return(tib)
            
#         }
        
#     }) %>% ungroup %>% select(-transfer_id)
    
    # print(with_wells, width=10000, n=1000)
    
    # print(paste0("next plate: ", plate_i))
    # print(paste0("next well: ", well_i))
    
    safe_write_csv(with_wells, paste0(home_dir, "/", run_name, "/", run_name, "---OUT-experiment-all-info.csv"), verbose=TRUE)
    
    # check that the minimum volume being transfered is >= min echo allowed transfer volume
    if(min(with_wells$vol_transfer) < 25){
        print(with_wells %>% filter(vol_transfer < 25))
        stop("ERROR\n\n****MINIMUM TRANSFER VOLUME IS 25 NL FOR ECHO 525!")
    }
            
    # clean up column names and write to csv
    echo_clean = with_wells %>% 
    mutate(source_plate = run_name) %>% 
    select(source_plate, source_well, dest_plate_id, polytransf_id, polytransf_desc, destination_well, vol_transfer) %>% 
    mutate(vol_transfer = round(vol_transfer,0)) %>% 
    rename("Source Plate Name" = source_plate,
           "Source Well" = source_well,
           "Destination Well" = destination_well,
           "Transfer Volume" = vol_transfer) %>% 
    mutate("Destination Plate Name" = paste0("CellPlate_",dest_plate_id,"----LipidPlate_",polytransf_id,"_",polytransf_desc)) %>% 
    select("Source Plate Name", "Source Well", "Destination Plate Name", "Destination Well", "Transfer Volume")
    
    safe_write_csv(echo_clean, paste0(home_dir, "/", run_name, "/", run_name, "---OUT-echo-instructions.csv"), verbose=TRUE)
    
    return(list(with_wells, echo_clean))

}

