# ................................................................
# The pipeline
#
# run_climmob_from_files()  what ClimMob.R calls on the server
#   read_climmob_request()  read the two JSON files
#   run_climmob_analysis()  all the analysis, returns `state`
#   write_climmob_outputs() main report, feedback, extra outputs
#
# run_climmob_analysis() can be called directly in a local session
# (see run-local/) and its result inspected step by step.
# ................................................................

#' Run the whole workflow from the files sent by ClimMob
#'
#' @param params_file path to the parameters JSON
#' @param data_file path to the data JSON
#' @param code_path folder with this code (ClimMob sends it as BackwardPath)
#' @param debug_dir optional folder for debug files, see dev/debug-a-failed-run.R
#' @return invisibly, the state with all the results
run_climmob_from_files = function(params_file, data_file,
                                  code_path = getOption("climmob.code_path", getwd()),
                                  debug_dir = NULL) {

  log = new_run_log(debug_dir)

  request = run_step(log, "read_request", "Read ClimMob request",
                     function() read_climmob_request(params_file, data_file))

  if (is.null(request)) {
    # without the parameters there is no output folder to write to
    print_run_summary(log)
    stop("Could not read the request sent by ClimMob: ",
         log$steps$message[log$steps$step == "read_request"])
  }

  settings = request$settings
  settings$code_path = code_path

  dir.create(settings$output_path, showWarnings = FALSE, recursive = TRUE)

  if (!is.null(debug_dir)) {
    file.copy(c(params_file, data_file),
              file.path(debug_dir, c("parameters.json", "data.json")),
              overwrite = TRUE)
  }

  state = run_climmob_analysis(cmdata = request$cmdata,
                               pars = request$pars,
                               settings = settings,
                               cmdatajson = request$cmdatajson,
                               log = log)

  write_climmob_outputs(state, settings)

  print_run_summary(state$log)

  if (!is.null(debug_dir)) {
    saveRDS(state, file.path(debug_dir, "state.rds"))
  }

  invisible(state)

}

#' Run all the analysis steps
#'
#' @param cmdata the trial data as data.frame
#' @param pars the decoded parameters, see decode_pars() or
#'  build_pars_from_cmlist() for local runs
#' @param settings the report settings, see parse_report_settings() or
#'  climmob_settings() for local runs
#' @param cmdatajson optional, the CM_list sent by ClimMob, used to find
#'  the GPS coordinates. When NULL, coordinates are searched in cmdata
#' @param log a run log, a new one is created by default
#' @param tech_index the columns with the items in each package
#' @return a list (state) with every result and the run log
run_climmob_analysis = function(cmdata, pars, settings, cmdatajson = NULL,
                                log = new_run_log(),
                                tech_index = paste0("package_item_", LETTERS[1:3])) {

  set_language(settings$language %||% "en", settings$code_path %||% getOption("climmob.code_path", "."))

  s = list(log = log, cmdata = cmdata, pars = pars, settings = settings)

  # 1. Data ----
  s$rank_dat = run_step(log, "rank_dat", "Organize Ranking Data", function() {
    organize_ranking_data(cmdata, pars,
                          groups = settings$groups,
                          option_label = settings$option,
                          ranker_label = settings$ranker,
                          reference_tech = settings$reference_names,
                          project_name = settings$project_name,
                          tech_index = tech_index,
                          minN = 5,
                          minitem = 2)
  }, fallback = error_data_rank_dat)

  # from here on every step uses the cleaned data
  if (!is.null(s$rank_dat$cmdata)) {
    s$cmdata = s$rank_dat$cmdata
  }

  cm = s$cmdata
  rd = s$rank_dat

  id_col = participant_columns(cmdata)$id

  s$quanti_dat = run_step(log, "quanti_dat", "Organize Quantitative Data", function() {
    organize_quantitative_data(cm, pars,
                               groups = settings$groups,
                               id = if (is.na(id_col)) "REG_qst162" else id_col,
                               tech_index = tech_index)
  }, fallback = error_data_quanti_dat)

  # 2. Monitoring, works before any trait has enough answers ----
  s$monitoring = run_step(log, "monitoring", "Trial monitoring", function() {
    get_monitoring(cmdata, pars,
                   group = build_groups(cmdata, settings$groups, min_per_group = 0))
  }, fallback = error_data_monitoring)

  # 3. Overview, map and climate ----
  s$overview_and_summaries = run_step(log, "overview", "Overview and Summary",
    function() get_overview_summaries(cm, rd),
    fallback = error_data_overview_and_summaries,
    needs = "rank_dat")

  s$coords = run_step(log, "coords", "Trial coordinates",
    function() get_trial_coordinates(if (is.null(cmdatajson)) cmdata else cmdatajson),
    fallback = NULL,
    report_error = FALSE)

  s$trial_map = run_step(log, "trial_map", "Trial map",
    function() get_testing_sites_map(s$coords, country = settings$country),
    fallback = error_data_trial_map,
    needs = "coords")

  s$participation_map = run_step(log, "participation_map", "Participation map",
    function() get_participation_map(s$coords, s$monitoring$responses, adm = s$trial_map$adm),
    fallback = error_data_participation_map,
    needs = c("coords", "monitoring"))

  s$agroclimate = run_step(log, "agroclimate", "Agroclimatic data",
    function() get_agroclimatic_data(cm, coords = s$coords, ndays = season_length(pars)),
    fallback = error_data_agroclimate,
    needs = "coords")

  # 4. Plackett-Luce models ----
  s$rankings = run_step(log, "rankings", "Plackett-Luce rankings",
    function() build_trait_rankings(cm, rd),
    needs = "rank_dat")

  s$models = run_step(log, "models", "Plackett-Luce model",
    function() fit_trait_models(s$rankings),
    needs = "rankings")

  logworth = run_step(log, "logworth", "Log-worth plots",
    function() make_logworth_plots(s$models, rd),
    fallback = error_data_logworth, needs = "models")

  worthmap = run_step(log, "worthmap", "Worth map",
    function() make_worthmap(s$models, rd),
    needs = "models")

  anova_tbl = run_step(log, "anova", "Analysis of variance",
    function() make_anova_table(s$models, rd),
    fallback = data.frame(), needs = "models")

  netwinning = run_step(log, "netwinning", "Favourability",
    function() make_netwinning_table(s$rankings, rd),
    fallback = data.frame(), needs = "rankings")

  reliability = run_step(log, "reliability", "Reliability",
    function() make_reliability(s$models, rd),
    fallback = error_data_reliability, needs = "models")

  kendall = run_step(log, "kendall", "Kendall tau",
    function() make_kendall(cm, rd, nboot = settings$kendall_nboot %||% 200),
    fallback = error_data_kendall, needs = "rank_dat")

  s$PL_models = assemble_PL_models(s$models %||% list(), logworth, worthmap,
                                   anova_tbl, netwinning, reliability, kendall)

  s$data_value = run_step(log, "data_value", "Value of more answers",
    function() get_data_value(s$models[[rd$reference_trait_index]],
                              n_now = sum(rd$trait_list[[rd$reference_trait_index]]$keep),
                              n_registered = s$monitoring$registered,
                              nsim = settings$data_value_nsim %||% 50),
    fallback = error_data_data_value, needs = "models")

  # 5. Groups of participants ----
  s$group_analysis = run_step(log, "groups", "Analysis by group",
    function() get_group_analysis(rd, s$rankings,
                                  min_n = settings$min_n_group %||% 20,
                                  balance_threshold = settings$balance_threshold %||% 0.25),
    fallback = error_data_group_analysis, needs = c("rankings", "models"))

  # 6. Trees ----
  s$PL_tree = run_step(log, "pl_tree", "Plackett-Luce Tree",
    function() get_PlackettLuce_tree(cm, rd, s$agroclimate,
                                     use_climate = settings$use_climate %||% TRUE),
    fallback = error_data_PL_tree, needs = "rank_dat")

  # 7. Measured traits ----
  s$metric = run_step(log, "metric", "Measured traits",
    function() get_metric_analysis(s$quanti_dat, check = rd$reference_tech[1]),
    fallback = error_data_metric, needs = "quanti_dat")

  # 8. Evidence for variety release ----
  s$release = run_step(log, "release", "Variety release summary",
    function() get_release_summary(cm, rd, s$models, s$rankings, s$metric),
    fallback = error_data_release, needs = "models")

  # 9. Feedback to participants ----
  s$feedback = run_step(log, "feedback", "Participant feedback data",
    function() get_feedback_data(cm, rd, s$models),
    fallback = error_data_feedback, needs = "models")

  s

}

#' Write everything that goes back to ClimMob
write_climmob_outputs = function(state, settings) {

  dir.create(settings$output_path, showWarnings = FALSE, recursive = TRUE)

  set_language(settings$language %||% "en", settings$code_path)

  log = state$log %||% new_run_log()

  run_step(log, "main_report", "Main report",
           function() render_main_report(state, settings), report_error = FALSE)

  if (isTRUE(settings$infosheets)) {
    run_step(log, "participant_reports", "Participant reports",
             function() render_participant_reports(state, settings), report_error = FALSE)
  }

  run_step(log, "extra_outputs", "Extra outputs",
           function() write_extra_outputs(state, settings), report_error = FALSE)

  if (nrow(state$feedback$participants %||% data.frame()) > 0) {
    run_step(log, "ai_ready", "AI-ready export",
             function() write_ai_ready(state$feedback, state, settings), report_error = FALSE)
  }

  invisible(settings$output_path)

}
