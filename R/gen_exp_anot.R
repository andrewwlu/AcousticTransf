#' generated annotation file for downstream experiment analysis
#'
#' This function xxxxxxx
#'
#' @param xxxx param
#' @return xxxx return
#' @import tidyverse
#' @export
gen_exp_anot <- function(echo_df, transf_plate_type, anot_save_dir){
    
    nicknames = read_csv("/Users/andrewlu/Dropbox/Documents/Labowitz/Experiments/AL-AcousticTransf/data/helper_files/plasmid_nicknames.csv", col_types = cols())

   if (transf_plate_type == "96w") {
        list_of_wells = expand.grid(LETTERS[1:8],seq(1,12)) %>% arrange(Var1,Var2) %>% mutate(comb=paste0(Var1,Var2)) %>% pull(comb)
    } else if (transf_plate_type == "24w") {
        list_of_wells = expand.grid(LETTERS[1:4],seq(1,6)) %>% arrange(Var1,Var2) %>% mutate(comb=paste0(Var1,Var2)) %>% pull(comb)
    } else{
        stop("ERROR\n\n****UNRECOGNIZED PLATE TYPE")
    }

    anot = echo_df %>% 
    # add plasmid nicknames
    left_join(nicknames, by=c("dna_id","dna_desc")) %>% 
    rename(well = destination_well) %>% 
    select(well, nickname, polytransf_desc, rep_id) %>% 
    # turn into correct format
    pivot_wider(names_from = polytransf_desc, 
                values_from = nickname, 
                values_fn = function(x) paste0(x, collapse="--")) %>% 
    relocate(rep_id, .after = last_col()) %>% 
    # order wells properly
    mutate(well = factor(well, levels = list_of_wells))
                
    # write to csv
    safe_write_csv(anot, anot_save_dir, add_date_time=FALSE, verbose=TRUE)
    
}