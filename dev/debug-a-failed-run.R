# ................................................................
# Debug a ClimMob report
#
# 1. Get the two files ClimMob sent for the report, either from the
#    server or by running ClimMob.R with a debug folder
#
#      CLIMMOB_DEBUG_DIR=/tmp/cm-debug Rscript ClimMob.R pars.json data.json
#
#    The debug folder then holds
#      parameters.json, data.json  copies of the request
#      state.rds                   every result of the run
#      failed-<step>.rds           one file per failed step
#
# 2. Run this script line by line from the root of the repository.
# ................................................................
source("load.R")
load_climmob()

debug_dir = "/tmp/cm-debug" # change to your folder

# A. Rerun the whole analysis in this session ----
# OutputPath and BackwardPath in parameters.json point to the server,
# so read the request and point them to local folders
req = read_climmob_request(file.path(debug_dir, "parameters.json"),
                           file.path(debug_dir, "data.json"))

settings = req$settings
settings$output_path = file.path(debug_dir, "output")
settings$code_path = getwd()

res = run_climmob_analysis(req$cmdata, req$pars, settings, cmdatajson = req$cmdatajson)

print_run_summary(res$log)

# B. Step into a failed step ----
# the ids of the steps are in the summary, for example "pl_tree"
f = readRDS(file.path(debug_dir, "failed-pl_tree.rds"))
f$message # the error
f$calls   # the calls that led to it

# reproduce the error with the exact same inputs, after asking R to
# stop inside the function you suspect
debugonce(get_PlackettLuce_tree)
f$reproduce()

# C. Or call any step function directly with the results of the run ----
# rank_dat = res$rank_dat
# cmdata = res$cmdata
# tree = get_PlackettLuce_tree(cmdata, rank_dat)
# kendall = make_kendall(cmdata, rank_dat)

# D. Write the outputs locally to check the report ----
# write_climmob_outputs(res, settings)
