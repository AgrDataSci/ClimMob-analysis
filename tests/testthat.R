# ................................................................
# Run the tests from the root of the repository
#   Rscript tests/testthat.R
# The tests cover the base R parts of the workflow (reading the
# request, cleaning and organizing the data, the step runner) and do
# not need the analysis packages.
# ................................................................
library("testthat")

source("load.R")
load_climmob(".", packages = FALSE)

test_dir("tests/testthat", reporter = "summary")
