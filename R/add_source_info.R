#' add source info
#'
#' This function xxxxxxx
#'
#' @param xxxx param
#' @return xxxx return
#' @import tidyverse
#' @export
add_source_info = function(ng){
    
    # every other row, everyone other column
    wells_384w = expand.grid(LETTERS[1:16][c(TRUE, FALSE)],seq(1,24)) %>% arrange(Var1,Var2) %>% mutate(comb=paste0(Var1,Var2)) %>% pull(comb)    
    wells_384w = wells_384w[c(TRUE, FALSE)]

    source_i = 1

    ng_split = ng %>% 

    # dna_id is a factor column that I already ordered based on the original ng file order, so here i'm just rearranging back to og order
    arrange(dna_id) %>% 

    group_by(exp_name, dna_id, dna_desc) %>% 
    group_modify(function(tib,key){

        # print(key)
        # print(tib, width=10000)
        # cat("\n\n\n")

        split = tibble()

        source_v = c(source_plate_dv)

        for(transf_i in rownames(tib)){

            transf = tib[transf_i, ]
            # print(transf, width=10000)
            transf_v_ul = transf$vol_transfer/1000

            # while(transf_v_ul > 0){
            
            # if the remaining volume needed to shoot is less than 25 nL (the minimum echo shoot volume), then just don't worry about it
            # 25 nl of 50ng/ul = 1.25 ngs
            
            while(transf_v_ul > (25/1000)){

                # if what is left to transfer is enough to fit into one source well
                if(source_v[length(source_v)] + transf_v_ul <= source_plate_max_v){

                    source_v[length(source_v)] = source_v[length(source_v)] + transf_v_ul
                    split = bind_rows(split, transf %>% mutate(source_well = wells_384w[source_i],
                                                               vol_transfer_split = transf_v_ul * 1000,
                                                               ng_split = ng * (vol_transfer_split / vol_transfer)))
                    transf_v_ul = 0

                } else{

                    # if what is left to transfer isn't going to fit in the current well, and needs more source wells
                    remainder_ul = source_plate_max_v - source_v[length(source_v)]

                    # if currently at 40ul in well, and the max is 40, then no more remainining volumes
                    if(remainder_ul != 0){
                        
                        source_v[length(source_v)] = source_v[length(source_v)] + remainder_ul
                        transf_v_ul = transf_v_ul - remainder_ul
                        split = bind_rows(split, transf %>% mutate(source_well = wells_384w[source_i],
                                                               vol_transfer_split = remainder_ul * 1000,
                                                               ng_split = ng * (vol_transfer_split / vol_transfer)))
                    }
                    
                    source_v = c(source_v, source_plate_dv)
                    source_i <<- source_i + 1


                }

                # print(source_v)
                # cat("\n\n")
            }
            # cat("****\n")
            # print(split, width=1000)
            # cat("\n\n\n")


        }

        # next plasmid will need to go to next source well
        source_i <<- source_i + 1

        return(split)
    }) %>% ungroup %>% 
    
    rename(ng_total = ng) %>% 
    rename(total_vol_transfer = vol_transfer, 
           vol_transfer = vol_transfer_split)

    # order source wells
    suppressWarnings({ng_split = ng_split %>% mutate(source_well = fct_relevel(source_well, wells_384w))})
    
    return(ng_split)
    
}



