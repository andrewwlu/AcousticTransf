#' remake ng csv file from script outputs
#'
#' This function xxxxxxx
#'
#' @param xxxx param
#' @return xxxx return
#' @import tidyverse
#' @export
remake_ng_csv = function(){
    
    source = read_csv(paste0(home_dir, "/", run_name, "/", run_name, "---OUT-source-plate-prep-instructions.csv"), col_types = cols()) %>% 
    select(source_well, dna_id, dna_desc)
    
    remade_csv = read_csv(paste0(home_dir, "/", run_name, "/", run_name, "---OUT-echo-instructions.csv"), col_types = cols()) %>% 
    rename(source_well = "Source Well",
           dest_plate = "Destination Plate Name",
           dest_well = "Destination Well",
           vol = "Transfer Volume") %>% 

    select(dest_plate, dest_well, source_well, vol) %>% 
    group_by(dest_plate, dest_well, source_well) %>% 
    summarize(vol = sum(vol), .groups = "drop") %>% 

    arrange(dest_well, dest_plate) %>% 

    mutate(ng = vol / 1000 * dna_echo_conc) %>% 
    select(-vol) %>% 

    left_join(source, by = "source_well") %>% 
    select(-source_well) %>% 

    pivot_wider(names_from = dest_well, values_from = ng) %>% 

    arrange(dest_plate) %>% 
    select(dna_id, dna_desc, dest_plate, everything()) %>% 

    mutate(across(everything(), as.character)) %>% 
    replace(is.na(.), "") 
    
    safe_write_csv(remade_csv, paste0(home_dir, "/", run_name, "/", run_name, "---OUT-remade-ng-csv-with-script-outputs.csv"), verbose=TRUE)

}