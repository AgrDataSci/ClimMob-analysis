test_that("run_step returns the value of a step that works", {
  log = new_run_log()
  x = suppressMessages(run_step(log, "a", "Step A", function() 1 + 1))
  expect_equal(x, 2)
  expect_equal(log$steps$status, "ok")
})

test_that("run_step records a failure and returns the fallback", {

  log = new_run_log()

  x = suppressMessages(run_step(log, "a", "Step A", function() stop("boom"), fallback = "fb"))

  expect_equal(x, "fb")
  expect_equal(log$failed, "a")
  expect_equal(log$errors, "Step A: \n boom")
})

test_that("steps depending on a failed step are skipped, not run", {

  log = new_run_log()
  ran = FALSE

  suppressMessages({
    run_step(log, "a", "Step A", function() stop("boom"))
    run_step(log, "b", "Step B", function() ran <<- TRUE, needs = "a")
  })

  expect_false(ran)
  expect_equal(log$steps$status, c("failed", "skipped"))
  expect_length(log$errors, 1) # only the real error goes to the report
})

test_that("warnings are collected without stopping the step", {
  log = new_run_log()
  x = suppressMessages(run_step(log, "w", "W", function() { warning("careful"); 3 }))
  expect_equal(x, 3)
  expect_equal(log$details$w$warnings, "careful")
})

test_that("a failed step can be reproduced from the debug folder", {

  dir = tempfile()
  log = new_run_log(debug_dir = dir)
  value = 5

  suppressMessages(run_step(log, "a", "A", function() stop("value is ", value)))

  f = readRDS(file.path(dir, "failed-a.rds"))

  expect_equal(f$message, "value is 5")
  expect_error(f$reproduce(), "value is 5")
})

test_that("save_table skips empty tables", {
  f = tempfile(fileext = ".csv")
  expect_false(save_table(data.frame(), f))
  expect_false(save_table(NULL, f))
  expect_true(save_table(data.frame(a = 1), f))
  expect_true(file.exists(f))
})
