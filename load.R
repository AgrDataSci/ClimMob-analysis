# ................................................................
# Load the ClimMob analysis code
#
# Sources every file in R/ (in alphabetical order) and attaches the
# packages used by the workflow. Use it from ClimMob.R, from the
# local runner, from the tests or from the R console:
#
#   source("load.R")
#   load_climmob()
# ................................................................
load_climmob = function(code_path = ".", packages = TRUE) {

  code_path = normalizePath(code_path, mustWork = TRUE)

  files = list.files(file.path(code_path, "R"),
                     pattern = "[.]R$",
                     full.names = TRUE)

  files = sort(files)

  if (length(files) == 0) {
    stop("No R files found in ", file.path(code_path, "R"))
  }

  for (f in files) {
    source(f, local = globalenv())
  }

  if (isTRUE(packages)) {
    load_climmob_packages()
  }

  # remember where the code lives, reports and templates are found from here
  options(climmob.code_path = code_path)

  invisible(files)

}
