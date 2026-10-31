# ................................................................
# Pipeline runner
#
# Every analysis step is run through run_step(). This replaces the
# repeated tryCatch() blocks of the old ClimMob.R and gives, for each
# step, a status, the time it took, the error message, the R calls
# that led to the error and the warnings it raised.
#
# A step that depends on a failed step is skipped (not run with
# placeholder data), so one failure produces one clear message
# instead of a chain of confusing errors further down.
#
# When a debug directory is set, each failed step is saved as an
# .rds file holding a function that reproduces the failure with the
# exact same inputs (see dev/debug-a-failed-run.R).
# ................................................................

#' Create an empty run log
new_run_log = function(debug_dir = NULL) {

  log = new.env()
  log$steps = data.frame(step = character(),
                         label = character(),
                         status = character(),
                         seconds = numeric(),
                         message = character(),
                         stringsAsFactors = FALSE)
  log$failed = character()
  log$errors = character()
  log$details = list()
  log$debug_dir = debug_dir

  if (!is.null(debug_dir)) {
    dir.create(debug_dir, showWarnings = FALSE, recursive = TRUE)
  }

  log

}

#' Run one step of the analysis
#'
#' @param log a run log from new_run_log()
#' @param step short id of the step, used in the log and debug files
#' @param label human readable name, used in the error appendix of the report
#' @param fun a function without arguments that does the work
#' @param fallback the value returned when the step fails or is skipped
#' @param needs ids of the steps this step depends on
#' @param report_error logical, if TRUE a failure is listed in the report
#' @return the value of fun() or the fallback
run_step = function(log, step, label = step, fun, fallback = NULL,
                    needs = character(0), report_error = TRUE) {

  blocked = intersect(needs, log$failed)

  if (length(blocked) > 0) {
    msg = paste0("skipped because '", paste(blocked, collapse = "', '"), "' failed")
    record_step(log, step, label, "skipped", 0, msg)
    log$failed = union(log$failed, step)
    return(fallback)
  }

  calls = NULL
  warns = character()
  t0 = Sys.time()

  result = tryCatch(
    withCallingHandlers(
      fun(),
      error = function(e) {
        # keep the R calls that led to the error, without the runner itself
        calls <<- utils::head(sys.calls(), -2)
      },
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
      }),
    error = function(e) {
      structure(list(message = conditionMessage(e)), class = "climmob_step_error")
    })

  secs = round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)

  log$details[[step]] = list(warnings = unique(warns))

  if (!inherits(result, "climmob_step_error")) {
    record_step(log, step, label, "ok", secs, "")
    return(result)
  }

  # the step failed
  log$failed = union(log$failed, step)
  log$details[[step]]$calls = format_calls(calls)

  record_step(log, step, label, "failed", secs, result$message)

  if (isTRUE(report_error)) {
    # same format used in the report appendix since v1.1
    log$errors = c(log$errors, paste0(label, ": \n ", result$message))
  }

  if (!is.null(log$debug_dir)) {
    saveRDS(list(step = step,
                 message = result$message,
                 calls = log$details[[step]]$calls,
                 reproduce = fun),
            file.path(log$debug_dir, paste0("failed-", step, ".rds")))
  }

  fallback

}

#' Add one line to the run log and print it
record_step = function(log, step, label, status, seconds, message) {

  log$steps = rbind(log$steps,
                    data.frame(step = step,
                               label = label,
                               status = status,
                               seconds = seconds,
                               message = message,
                               stringsAsFactors = FALSE))

  icon = c(ok = "[ok]     ", failed = "[FAILED] ", skipped = "[skipped]")[[status]]

  line = paste0(icon, " ", step, " (", seconds, "s)")

  if (nzchar(message)) {
    line = paste0(line, " ", message)
  }

  message(line)

  invisible(log)

}

#' Turn the calls captured at an error into readable lines
format_calls = function(calls, max_calls = 12) {

  if (length(calls) == 0) {
    return(character())
  }

  out = vapply(calls, function(x) {
    paste(utils::head(deparse(x, width.cutoff = 100), 1), collapse = "")
  }, character(1))

  utils::tail(out, max_calls)

}

#' Print a summary of the run
print_run_summary = function(log) {

  s = log$steps

  message("\n---- ClimMob analysis summary ----")
  message(sum(s$status == "ok"), " steps ok, ",
          sum(s$status == "failed"), " failed, ",
          sum(s$status == "skipped"), " skipped")

  for (st in s$step[s$status == "failed"]) {

    message("\nStep '", st, "' failed: ", s$message[s$step == st])

    calls = log$details[[st]]$calls

    if (length(calls) > 0) {
      message("  last calls before the error:")
      message(paste0("    ", calls, collapse = "\n"))
    }

  }

  invisible(s)

}
