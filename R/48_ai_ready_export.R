# ................................................................
# AI-ready export
#
# Plain CSV tables with the results of the trial, plus a short guide,
# so that a person working with an AI assistant can write feedback
# for participants, flyers or summaries in any language and format.
# Written to <output_path>/extra-outputs/ai-ready/
#   README.md                 how to use the files, for people and AI assistants
#   trial-summary.csv         one line with the main facts of the trial
#   results-by-trait.csv      worth and rank of every item in every trait
#   item-vs-check.csv         every item against the check, trait by trait
#   participants.csv          one line per participant, own ranking and points
#   participant-answers.csv   best and worst option of every participant by trait
# The column definitions are in docs/outputs.md.
# ................................................................

#' Write the AI-ready tables
#'
#' @param feedback output of get_feedback_data()
#' @param state the state of the run
#' @param settings the report settings
#' @param include_names logical, keep participant names in the files
write_ai_ready = function(feedback, state, settings, include_names = TRUE) {

  dir = file.path(settings$output_path, "extra-outputs", "ai-ready")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  rd = state$rank_dat

  summary = data.frame(project = rd$projname %||% NA,
                       crop = feedback$crop,
                       language = current_language(),
                       option_label = rd$option,
                       participant_label = rd$ranker,
                       participants_registered = state$monitoring$registered %||% NA,
                       participants_reference_trait = feedback$n_participants,
                       items_tested = length(rd$technologies),
                       items = paste(rd$technologies, collapse = "; "),
                       check_items = paste(rd$reference_tech, collapse = "; "),
                       reference_trait = feedback$reference_trait,
                       traits = paste(rd$trait_names, collapse = "; "),
                       traits_dropped = paste(rd$trait_dropped, collapse = "; "),
                       report_date = as.character(settings$date %||% Sys.Date()),
                       stringsAsFactors = FALSE)

  participants = feedback$participants
  answers = feedback$answers

  if (!isTRUE(include_names)) {
    participants$name = NULL
  }

  write = function(x, f) {
    if (is.data.frame(x) && nrow(x) > 0) {
      utils::write.csv(x, file.path(dir, f), row.names = FALSE, fileEncoding = "UTF-8", na = "")
    }
  }

  write(summary, "trial-summary.csv")
  write(feedback$results, "results-by-trait.csv")
  write(state$release$by_trait, "item-vs-check.csv")
  write(participants, "participants.csv")
  write(answers, "participant-answers.csv")

  guide = file.path(settings$code_path, "report", "ai", "README.md")
  if (file.exists(guide)) {
    file.copy(guide, file.path(dir, "README.md"), overwrite = TRUE)
  }

  invisible(list.files(dir))

}
