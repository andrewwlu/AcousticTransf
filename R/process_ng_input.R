#' processes ng input file (wide to long, clean up..etc)
#'
#' This function xxxxxxx
#'
#' @param xxxx param
#' @return xxxx return
#' @import tidyverse
#' @export
process_ng_input <- function(experiment_name, ng_dir, n_replicates, dest_plate_type){
        
    pid = 1 # for keeping track of polytransfection ID
    
    # read in ng csv
    ng = read_csv(ng_dir, col_types = cols()) %>% 
    
    # make longer and clean out useless rows
    pivot_longer(!c("dna_id","dna_desc","polytransf_desc","dna_conc"), values_to = "ng") %>% 
    filter(!is.na(ng)) %>% 
    
    # give numeric id to conditions
    mutate(cond_id = as.numeric(gsub("[...]", "", name)) - ncol(.) + 2) %>% 
    select(-name) %>% 
    
    # calculate transfer volume
    mutate(vol_transfer = ng / dna_conc * 1000) %>% 

    # add reps and rep ID
    arrange(cond_id) %>% 
    group_by(cond_id) %>% 
    group_modify(function(tib,key){

        # add reps and rep ID
        new_tib = tibble()
        for (r in seq(1,n_replicates)){

            new_tib = bind_rows(new_tib, tib %>% mutate(rep_id = r))
        }

        return(new_tib)

    }) %>% ungroup %>% 

    # add polytransfection id (integer)
    group_by(polytransf_desc) %>% 
    group_modify(function(tib,key){
        new=tib %>% mutate(polytransf_id = pid)
        pid <<- pid + 1
        return(new)
    }) %>% ungroup %>% 
    arrange(cond_id, rep_id) %>% 
    
    # add experiment name
    mutate(exp_name = experiment_name) %>% 
    
    # final rearranging of columns
    select(exp_name, cond_id, rep_id, polytransf_id, polytransf_desc, dna_id, dna_desc, dna_conc, ng, vol_transfer, everything())

    return(ng)

}