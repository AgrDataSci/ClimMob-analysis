# ................................................................
# Install the packages used by the workflow
# Run from the root of the repository
# ................................................................
source("R/00_packages.R")

pkgs = c(climmob_packages, climmob_suggested)

missing = pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]

# ggchicklet is not on CRAN
cran = setdiff(missing, "ggchicklet")

if (length(cran) > 0) {
  install.packages(cran)
}

if ("ggchicklet" %in% missing) {
  install.packages("ggchicklet", repos = "https://cinc.rud.is")
}

# used only by the tests
if (!requireNamespace("testthat", quietly = TRUE)) {
  install.packages("testthat")
}
