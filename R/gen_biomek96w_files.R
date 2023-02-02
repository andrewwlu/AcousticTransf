#' generates instructions for biomek to transfection 96w plate
#'
#' This function xxxxxxx
#'
#' @param xxxx param
#' @return xxxx return
#' @import tidyverse
#' @export
gen_biomek96w_files = function(df, stamp_multiplier = 1.2, ul_opti_to_DNA, v_stampplate_to_optiDNAplate, ul_fugene_per_microgram){
    
    ####### volumes
    # echo DNA
    # constant: add 5 uL opti to DNA plate
    # constant: from epi tube, add 12 uL to stamp plate
    # constant: from stamp plate, add 10 uL to DNA plate
    # constant: add 20 uL (itll be a little less but 20 is okay)

    clean = df[[2]] %>% 
    rename(dest_plate = "Destination Plate Name",
           dest_well = "Destination Well",
           transf_v = "Transfer Volume") %>% 
    
    select(dest_plate, dest_well, transf_v) %>% 
    group_by(dest_plate, dest_well) %>% 
    summarize(tot_v = sum(transf_v), .groups = "drop") %>% 
    
    mutate(ng = tot_v / 1000 * dna_echo_conc) %>% 
    
    select(dest_plate, dest_well, ng, tot_v) %>% 
    group_by(dest_plate) %>% 
    summarize(max_ng = max(ng), 
              max_v = max(tot_v) / 1000,
              .groups = "drop") %>% 
        
    mutate(ul_fugene = max_ng / 1000 * ul_fugene_per_microgram) %>% 
    mutate(ul_opti = (v_stampplate_to_optiDNAplate - ul_fugene)) %>% 
    
    mutate(ul_fugene = ul_fugene * stamp_multiplier,
           ul_opti = ul_opti * stamp_multiplier) %>% 
    
    mutate(ul_to_stamp_plate = ul_fugene + ul_opti)
    
    
    # print(clean, n = 50)
    # cat("\n\n")
    
    i_plate = 1
    fugene_opti_prep_instruct = tibble()
    
    clean %>% 
    group_by(dest_plate) %>% 
    group_walk(function(tib,key){
        
        # print(key)
        # print(tib)
        
        # generate csv hitlist to transfer opti+fugene from epi tubes to stamp plate
        hit = tibble(
            
            Source = "Reagent",
            SourceWell = i_plate,
            Dest = paste0("stamp", i_plate),
            DestWell = c(matrix(seq(1,96), nrow = 8, byrow = TRUE)),
            Volume = tib$ul_to_stamp_plate
        
        ) 
        
        hit %>%
        safe_write_csv(paste0(home_dir, "/", run_name, "/", run_name, "---OUT-biomek96w-transf-to-stamp-plate-hitlist-",key$dest_plate,".csv"), verbose=TRUE)
        
        # add to fugene+opti mastermix instructions tibble
        fugene_opti_prep_instruct <<- fugene_opti_prep_instruct %>% 
        bind_rows(
            
            tib %>% 
            
            mutate(MM_ul_fugene = round(ul_fugene * 96 * 1.2,0)) %>% 
            mutate(MM_ul_opti = round(ul_opti * 96 * 1.2,0)) %>% 
            mutate(MM_total = MM_ul_fugene + MM_ul_opti) %>% 
            
            mutate(tube_i = unique(hit$SourceWell)) %>% 
            mutate(stamp_plate = unique(hit$Dest)) %>% 
            
            mutate(dest_plate = key$dest_plate) %>% 
            
            select(tube_i, stamp_plate, dest_plate, MM_total, MM_ul_fugene, MM_ul_opti)
                 
        )
        
        
        i_plate <<- i_plate + 1
        
        # cat("\n\n")
        
    })
    
    fugene_opti_prep_instruct %>% 
    safe_write_csv(paste0(home_dir, "/", run_name, "/", run_name, "---OUT-biomek96w-fugene-opti-MM-prep-instructions.csv"), verbose=TRUE)


}

