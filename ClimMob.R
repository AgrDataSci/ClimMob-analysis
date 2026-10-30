# ................................................................
# ClimMob analysis, entry point called by the ClimMob server
#
#   Rscript ClimMob.R <parameters.json> <data.json>
#
# 1. <parameters.json> report settings and the variables to analyse.
#    Parameters$BackwardPath is the folder with this code and
#    Parameters$OutputPath is where the outputs are written.
# 2. <data.json> the trial data.
#
# Outputs, in OutputPath
#   climmob_main_report.<Format>
#   participant-report/participant_report_package_<i>.<Format>  (if InfoSheets)
#   extra-outputs/  figures (PDF) and tables (CSV)
#
# All the work is done by the functions in R/, see README.md.
# Set the environment variable CLIMMOB_DEBUG_DIR to a folder to save
# the inputs and every failed step for debugging.
# ................................................................
args = commandArgs(trailingOnly = TRUE)

if (length(args) < 2) {
  stop("Usage: Rscript ClimMob.R <parameters.json> <data.json>")
}

# where is the code? ClimMob sends it as BackwardPath, otherwise use
# the folder of this script
code_path = tryCatch(jsonlite::fromJSON(args[1])[["Parameters"]][["BackwardPath"]],
                     error = function(e) NULL)

if (is.null(code_path) || !dir.exists(code_path)) {
  this_file = sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  code_path = if (length(this_file) == 1) dirname(normalizePath(this_file)) else getwd()
}

source(file.path(code_path, "load.R"))
load_climmob(code_path)

debug_dir = Sys.getenv("CLIMMOB_DEBUG_DIR", unset = "")
debug_dir = if (nzchar(debug_dir)) debug_dir else NULL

run_climmob_from_files(params_file = args[1],
                       data_file = args[2],
                       code_path = code_path,
                       debug_dir = debug_dir)

# End of analysis
