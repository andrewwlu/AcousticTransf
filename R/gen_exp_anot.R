#' generated annotation file for downstream experiment analysis
#'
#' This function xxxxxxx
#'
#' @param xxxx param
#' @return xxxx return
#' @import tidyverse
#' @export
gen_exp_anot <- function(echo_df){
    
    nicknames = read_csv("/Users/andrewlu/Dropbox/Documents/Labowitz/Experiments/AL-AcousticTransf/data/helper_files/plasmid_nicknames.csv", col_types = cols())
    
    # to accuont for experiments with multiple 24w plates
    list_of_wells = expand.grid(LETTERS[1:8],seq(1,12)) %>% arrange(Var1,Var2) %>% mutate(comb=paste0(Var1,Var2)) %>% pull(comb)
    
   # if (transf_plate_type == "96w") {
   #      list_of_wells = expand.grid(LETTERS[1:8],seq(1,12)) %>% arrange(Var1,Var2) %>% mutate(comb=paste0(Var1,Var2)) %>% pull(comb)
   #  } else if (transf_plate_type == "24w") {
   #      list_of_wells = expand.grid(LETTERS[1:4],seq(1,6)) %>% arrange(Var1,Var2) %>% mutate(comb=paste0(Var1,Var2)) %>% pull(comb)
   #  } else{
   #      stop("ERROR\n\n****UNRECOGNIZED PLATE TYPE")
   #  }

    # check that all plasmids have nicknames
             
    anot = echo_df %>% 
    rename(well = destination_well) %>% 
    select(exp_name, well, polytransf_desc, rep_id, dna_id, dna_desc) %>% 
    distinct %>% 
    # add plasmid nicknames
    left_join(nicknames, by=c("dna_id","dna_desc")) 
    
    # print(anot)
    
    if(any(is.na(anot))){
        print(is.na(anot))
        stop("ERROR\n\n****SOME PLASMIDS DON'T HAVE NICKNAMES")
    }
    
    anot_long = anot %>% 
    select(exp_name, well, nickname, polytransf_desc, rep_id) %>% 
    pivot_wider(names_from = polytransf_desc, 
                values_from = nickname, 
                values_fn = function(x) paste0(x, collapse="--")) %>% 
    # order wells properly
    mutate(well = factor(well, levels = list_of_wells)) %>% 
    select(exp_name, well, rep_id, everything())
               
    # print(anot_long)
                
    # write to csv                
    if(write_files){
        safe_write_csv(anot_long, paste0(runs_dir,run_name,"/",run_name,"-experiment-annotations.csv"), add_date_time=FALSE, verbose=TRUE)
    }
                    
}