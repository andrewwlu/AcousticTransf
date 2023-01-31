#' generates instructions for filling source plate
#'
#' This function xxxxxxx
#'
#' @param xxxx param
#' @return xxxx return
#' @import tidyverse
#' @export
gen_source_plate = function(df){
    
    source_v_multiplier = 1.08 
    # 60 * 1.08 rounded up is 65, which is the real max volume
    
    source = df %>% 

    select(dna_id, dna_desc, dna_stock_conc, dna_echo_conc, source_well, vol_transfer) %>% 
    group_by(dna_id, dna_desc, dna_stock_conc, dna_echo_conc, source_well) %>% 

    summarize(total_ul = sum(vol_transfer) / 1000, .groups = "drop") %>% 
    mutate(total_ul = round((total_ul + source_plate_dv) * source_v_multiplier, 0)) %>% 

    mutate(dna_stock_ul_add = total_ul * dna_echo_conc / dna_stock_conc) %>% 
    mutate(water_ul_add = total_ul - dna_stock_ul_add) %>% 
    
    select(dna_id, dna_desc, dna_stock_conc, dna_echo_conc, total_ul, source_well, dna_stock_ul_add, water_ul_add, everything())
    
    safe_write_csv(source, paste0(home_dir, "/", run_name, "/", run_name, "---OUT-source-plate-prep-instructions.csv"), verbose=TRUE)

    # get how much v ul of stock plasmid needed
    source_summed = source %>% 
    group_by(dna_id, dna_desc, dna_stock_conc) %>% 
    summarize(total_stock_needed = sum(dna_stock_ul_add), .groups = "drop") %>% 
    rename(dna = dna_id, conc = dna_stock_conc, total_needed = total_stock_needed) 
    
    # cat("\n**** Amount (ul) of each stock plasmid needed for this experiment ****\n\n")
    # source_summed %>% print(n=1000, width=10000)
    # cat('\n')
    
    safe_write_csv(source_summed, paste0(home_dir, "/", run_name, "/", run_name, "---OUT-DNA-stock-volumes-needed.csv"), verbose=TRUE)
    
    return(source)
    
}