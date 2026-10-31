# ................................................................
# Packages used by the workflow
# This is the only place where packages are attached.
# To install them run dev/install-packages.R
# ................................................................
climmob_packages = c("ClimMobTools",
                     "gosset",
                     "PlackettLuce",
                     "partykit",
                     "psychotools",
                     "qvcalc",
                     "jsonlite",
                     "knitr",
                     "rmarkdown",
                     "gtools",
                     "ggplot2",
                     "igraph",
                     "ggparty",
                     "patchwork",
                     "multcompView",
                     "caret",
                     "lubridate",
                     "ggchicklet",
                     "sf",
                     "geodata",
                     "ggspatial",
                     "climatrends",
                     "nasapower")

# Used when installed, the workflow still runs without them
#   lme4  mixed models for the measured traits (falls back to lm)
climmob_suggested = c("lme4")

#' Attach the packages, failing early with a clear message if any is missing
load_climmob_packages = function(pkgs = climmob_packages) {

  ok = vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)

  if (!all(ok)) {
    stop("These R packages are missing: ",
         paste(pkgs[!ok], collapse = ", "),
         ". Install them with dev/install-packages.R")
  }

  for (p in pkgs) {
    suppressPackageStartupMessages(library(p, character.only = TRUE))
  }

  invisible(pkgs)

}
