library(AcousticTransf)

setup(

    # main folder
    home_dir = "/Users/John/Documents/Lab/Experiments",

    # must match run folder name, in main folder
    run_name = "run",

    # must match ng input csv file name
    ng_csv = "run.csv",

    # number of replicates per condition
    n_reps = 2,

    # number of cell types
    n_celltypes = 1,

    # echo source plate max volume
    source_plate_max_v = 60,

    # echo source plate min volume
    source_plate_dv = 20,

    # DNA ng/ul concentration to transfer
    dna_echo_conc = 50,

    # destination plate type
    dest_plate_type = "24w"

)

run_acoustic_transf()
