#' prints plate view
#'
#' This function xxxxxxx
#'
#' @param xxxx param
#' @return xxxx return
#' @import tidyverse
#' @export
print_plate_view = function(info){
    
    cat("\n\n\n**** matrix shows volume being transferred by echo in uL ****\n")
    cat("(summing across multiple transfers, potentially from diff. source wells)\n\n\n")
    
    if(dest_plate_type == "24w"){
        
        dest_plate_n_cols = 6
        dest_plate_n_rows = 4
        
    } else if(dest_plate_type == "48w"){
        
        dest_plate_n_cols = 8
        dest_plate_n_rows = 6
        
        
    } else if(dest_plate_type == "96w"){
        
        dest_plate_n_cols = 12
        dest_plate_n_rows = 8
        
    }
    else{
        stop("view plate function doesn't have this plate type")
    }
    
    dest_plate_wells_vec = get_plate_wells(n_rows=dest_plate_n_rows, n_cols=dest_plate_n_cols)
        
    xx = info[[1]] %>% 

    select(polytransf_desc, dest_plate_id, dna_id, dna_echo_conc, destination_well, vol_transfer) %>% 

    group_by(polytransf_desc, dest_plate_id, dna_id, dna_echo_conc) %>% 

    group_walk(function(tib,key){

        print(key)

        df1 = tib %>% 
        group_by(destination_well) %>% 
        summarize(ul = sum(vol_transfer) / 1000, .groups = "drop") 

        # df1 %>% print(n=1000)

        # highest_well = df1 %>% 
        # separate(destination_well, into = c("text", "num"), sep = "(?<=[A-Za-z])(?=[0-9])") %>% 
        # arrange(desc(text),desc(num)) %>% slice_head(n=1) %>% mutate(wellz = paste0(text,num)) %>% pull(wellz)
        # all_wellz = tibble(destination_well = wells24[1: which(wells24 == highest_well)])
        # print(all_wellz)

        all_wellz = tibble(destination_well = dest_plate_wells_vec)

        mat = df1 %>% 
        full_join(all_wellz, by = "destination_well") %>% 
        mutate(ul = ifelse(is.na(ul), 0, ul)) %>% 
        mutate(destination_well = fct_relevel(destination_well, dest_plate_wells_vec)) %>% 
        arrange(destination_well) %>% 
        pull(ul) %>% 
        matrix(ncol = dest_plate_n_rows) %>% # this is not a bug; matrix is flipped when made
        t()

        colnames(mat) = seq(1,dest_plate_n_cols)
        rownames(mat) = LETTERS[1:dest_plate_n_rows]
        print(mat)
        cat("\n\n")

    }) %>% ungroup
}
