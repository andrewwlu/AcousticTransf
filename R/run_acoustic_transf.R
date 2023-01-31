#' main script that runs everything
#'
#' This function xxxxxxx
#'
#' @param xxxx param
#' @return xxxx return
#' @import tidyverse
#' @export
run_acoustic_transf = function(verbose=FALSE){

    cat("***** Starting main script of Acoustic Transfer \n")
    
    # setup(home_dir = "/Users/andrewlu/Dropbox/Documents/Labowitz/Experiments/AL-AcousticTransf", 
    #   run_name = "12-16-2022-test-no-DB", 
    #   ng_csv = "", 
    #   n_reps = "", 
    #   n_celltypes = "",
    #   transf_plate_type = "")
    
    # read in ng input file, and add a bunch of information (IDs for condition, reps, polytransf..etc)
    ng = process_ng_input(experiment_name = run_name,
                          ng_dir = paste0(home_dir,"/",run_name,"/",ng_csv),
                          n_replicates = n_reps,
                          num_cell_types = n_celltypes)
    # if(verbose) print(ng, n=1000, width=1000)
    
    # print # of wells in experiment
    print_n_wells(ng)
    
    # add source well, while splitting up source wells that exceed max volume allowed per well 
    ng_with_source = add_source_info(ng)
    if(verbose) print(ng_with_source, n=1000, width=1000)

    # generate instructions for source plate (summing up plasmids needed volumes across experiment), including volumes for dilutions
    source = gen_source_plate(ng_with_source)
    if(verbose) print(source, n=1000, width=1000)
    
    # generate echo instructions csv file
    cat("***** Generating echo instruction csv file... \n")
    echo = gen_echo_csv(ng_with_source)
    
    # print plate view
    # print_plate_view(echo)
    
    # remake ng csv file with script outputs (to double check)
    remake_ng_csv()
    
    return(echo)


}