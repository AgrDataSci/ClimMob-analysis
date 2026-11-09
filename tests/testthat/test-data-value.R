# need_analysis_packages() is in helper-data.R

# full CI job, as in test-pipeline-fixture.R.
need_analysis_packages = function(pkgs = c("PlackettLuce", "gosset", "ggplot2")) {
  ok = vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
  if (!all(ok)) {
    if (identical(Sys.getenv("CLIMMOB_FULL_TESTS"), "true")) {
      stop("Packages missing in CI: ", paste(pkgs[!ok], collapse = ", "))
    }
    skip("analysis packages not installed")
  }
  load_climmob_packages(pkgs)
}

