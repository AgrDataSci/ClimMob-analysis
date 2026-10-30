# ................................................................
# Render the reports
#
# The templates in report/ are copied to a temporary folder for each
# run. This keeps knitr intermediate files out of the code folder and
# lets several reports render at the same time on the server.
#
# The templates only see the objects put in their environment by
# main_report_env() and participant_report_env(), so it is clear what
# each template depends on.
# ................................................................

#' Copy the report templates to a fresh temporary folder
prepare_render_dir = function(code_path) {

  src = file.path(code_path, "report")

  if (!dir.exists(src)) {
    stop("Report templates not found in ", src)
  }

  dest = tempfile("climmob-render-")
  dir.create(dest, recursive = TRUE)

  files = list.files(src, full.names = TRUE)
  files = files[!dir.exists(files)]

  file.copy(files, dest)

  dest

}

#' Objects available to report/mainreport.Rmd
main_report_env = function(state, settings) {

  rank_dat = state$rank_dat
  nt = length(rank_dat$technologies)
  ntraits = length(rank_dat$trait_list)

  # sizes of the worth map and the favourability plot
  worthmap_h = min(max(ntraits, 7), 8)
  favplot_h = min(max(nt * 0.4, 5), 7.5)

  env = list2env(list(
    project_name = settings$project_name %||% "",
    noptions = length(rank_dat$technologies_index),
    ntechnologies = nt,
    option = rank_dat$option,
    options = pluralize(rank_dat$option),
    participant = rank_dat$ranker,
    participants = pluralize(rank_dat$ranker),
    nparticipants = nrow(state$cmdata),
    ntraits = ntraits,
    reference_trait = title_case(rank_dat$reference_trait),
    reference_tech = rank_dat$reference_tech,
    rank_dat = rank_dat,
    overview_and_summaries = state$overview_and_summaries,
    trial_map = state$trial_map,
    agroclimate = state$agroclimate,
    PL_models = state$PL_models,
    PL_tree = state$PL_tree,
    quanti_dat = state$quanti_dat,
    quantitative_traits = state$quantitative_traits,
    error = if (length(state$log$errors) > 0) state$log$errors else NULL,
    dpi = 400,
    out_width = "100%",
    worthmap_h = worthmap_h,
    worthmap_w = worthmap_h + 0.5,
    favplot_h = favplot_h),
    parent = globalenv())

  # the option labels come from the request when the data failed
  if (!nzchar(env$option)) env$option = settings$option
  if (!nzchar(env$participant)) env$participant = settings$ranker
  env$options = pluralize(env$option)
  env$participants = pluralize(env$participant)

  env

}

#' Render one Rmd file, returning TRUE on success
render_rmd = function(input, output_dir, output_format, output_file, envir) {

  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  res = tryCatch({
    rmarkdown::render(input,
                      output_dir = output_dir,
                      output_format = output_format,
                      output_file = output_file,
                      intermediates_dir = dirname(input),
                      knit_root_dir = dirname(input),
                      envir = envir,
                      quiet = TRUE)
    TRUE
  }, error = function(e) {
    message("Rendering ", basename(input), " failed: ", conditionMessage(e))
    structure(FALSE, message = conditionMessage(e))
  })

  res

}

#' Render the main report, or a short report with the errors if it fails
#'
#' The output is always <output_path>/climmob_main_report.<extension>
render_main_report = function(state, settings) {

  render_dir = prepare_render_dir(settings$code_path)
  on.exit(unlink(render_dir, recursive = TRUE), add = TRUE)

  output_file = paste0("climmob_main_report.", settings$extension)

  env = main_report_env(state, settings)

  ok = render_rmd(file.path(render_dir, "mainreport.Rmd"),
                  output_dir = settings$output_path,
                  output_format = settings$output_format,
                  output_file = output_file,
                  envir = env)

  if (isTRUE(ok)) {
    return(invisible(TRUE))
  }

  # fallback, so ClimMob always receives a report
  state$log$errors = c(state$log$errors,
                       paste0("Main report: \n ", attr(ok, "message")))

  env$error = state$log$errors

  ok2 = render_rmd(file.path(render_dir, "failed_report.Rmd"),
                   output_dir = settings$output_path,
                   output_format = settings$output_format,
                   output_file = output_file,
                   envir = env)

  invisible(isTRUE(ok2))

}

#' Render one info sheet per participant
#'
#' Files are <output_path>/participant-report/participant_report_package_<i>.<extension>
render_participant_reports = function(state, settings) {

  participant_report = state$participant_report

  n = nrow(participant_report$partitable)

  if (is.null(n) || n == 0) {
    return(invisible(0))
  }

  render_dir = prepare_render_dir(settings$code_path)
  on.exit(unlink(render_dir, recursive = TRUE), add = TRUE)

  out_dir = file.path(settings$output_path, "participant-report")

  done = 0

  for (i in seq_len(n)) {

    env = list2env(list(i = i,
                        participant_report = participant_report,
                        rank_dat = state$rank_dat),
                   parent = globalenv())

    ok = render_rmd(file.path(render_dir, "participant_report_main.Rmd"),
                    output_dir = out_dir,
                    output_format = settings$output_format,
                    output_file = paste0("participant_report_package_", i, ".", settings$extension),
                    envir = env)

    done = done + isTRUE(ok)

  }

  if (done < n) {
    message(n - done, " participant report(s) could not be rendered")
  }

  invisible(done)

}
