# End-to-end test on the simulated project in tests/testdata/simulated-maize.
# Needs every package of the workflow, so it runs in continuous
# integration and on machines set up with dev/install-packages.R.

test_that("the full analysis runs on the simulated project", {

  pkgs = c(climmob_packages)
  if (!all(vapply(pkgs, requireNamespace, logical(1), quietly = TRUE))) {
    skip("analysis packages not installed")
  }

  load_climmob_packages()

  fx = root_path("tests", "testdata", "simulated-maize")
  out = file.path(tempdir(), "climmob-fixture")
  unlink(out, recursive = TRUE)

  req = read_climmob_request(file.path(fx, "parameters.json"), file.path(fx, "data.json"))

  settings = req$settings
  settings$output_path = out
  settings$code_path = root_path()
  settings$groups = "gender1"
  settings$infosheets = TRUE
  settings$use_climate = FALSE   # no network calls in tests

  res = run_climmob_analysis(req$cmdata, req$pars, settings, cmdatajson = req$cmdatajson)

  failed = res$log$steps[res$log$steps$status == "failed" &
                           !res$log$steps$step %in% c("coords", "trial_map", "agroclimate"), ]
  expect_equal(nrow(failed), 0, info = paste(failed$step, failed$message, collapse = "; "))

  # the simulated best item comes first
  expect_equal(res$feedback$results$item[res$feedback$results$is_reference_trait][1], "Hybrid 03")

  # women are a small minority in the simulation and are not compared
  expect_equal(res$group_analysis$eligibility$reason[res$group_analysis$eligibility$group == "Woman"], "too_few")

  # outputs keep the names ClimMob expects
  for (lang in c("en", "fr", "sw")) {
    settings$language = lang
    write_climmob_outputs(res, settings)
    expect_true(file.exists(file.path(out, "climmob_main_report.docx")))
  }
  expect_true(file.exists(file.path(out, "participant-report", "participant_report_package_1.docx")))
  expect_true(file.exists(file.path(out, "participant-report", "feedback-sheets.pdf")))
  expect_true(file.exists(file.path(out, "extra-outputs", "worth-map.pdf")))
  expect_true(file.exists(file.path(out, "extra-outputs", "variety-release-summary.csv")))
  expect_true(file.exists(file.path(out, "extra-outputs", "ai-ready", "participants.csv")))
})
