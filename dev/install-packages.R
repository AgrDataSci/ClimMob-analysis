# ................................................................
# Install the packages used by the workflow
# Run from the root of the repository
# ................................................................
source("R/00_packages.R")

pkgs = c(climmob_packages, climmob_suggested)

missing = pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing) > 0) {
  install.packages(missing)
}

# used only by the tests
if (!requireNamespace("testthat", quietly = TRUE)) {
  install.packages("testthat")
}
