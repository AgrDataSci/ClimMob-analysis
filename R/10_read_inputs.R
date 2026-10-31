# ................................................................
# Read and decode what ClimMob sends
#
# ClimMob calls ClimMob.R with two JSON files
#   1. the request parameters (report settings and the variables to analyse)
#   2. the trial data
# This file is the only place that knows the structure of those files.
# ................................................................

#' Read the two JSON files sent by ClimMob
#'
#' @param params_file path to (or text of) the JSON with the parameters
#' @param data_file path to (or text of) the JSON with the trial data
#' @return a list with settings, pars (decoded variables), cmdatajson
#'  (the CM_list sent by ClimMob) and cmdata (the same data as a data.frame)
read_climmob_request = function(params_file, data_file) {

  pars_raw = jsonlite::fromJSON(params_file)

  settings = parse_report_settings(pars_raw[["Parameters"]])

  pars = decode_pars(pars_raw)

  cmdatajson = jsonlite::fromJSON(data_file)
  class(cmdatajson) = union("CM_list", class(cmdatajson))

  cmdata = as.data.frame(x = cmdatajson,
                         tidynames = FALSE,
                         pivot.wider = TRUE)

  settings$project_name = ClimMobTools:::.safe_extract(cmdatajson,
                                                       c("project", "project_name"),
                                                       default = NA)

  settings$country = ClimMobTools:::.safe_extract(cmdatajson,
                                                  c("project", "project_cnty"),
                                                  default = NA)

  settings$reference_names = get_reference_names(cmdatajson, settings$reference)

  list(settings = settings,
       pars = pars,
       cmdatajson = cmdatajson,
       cmdata = cmdata)

}

#' Turn the "Parameters" block of the request into a clean settings list
#'
#' Field names are the ones used by ClimMob, including the original
#' spelling of "Languaje".
parse_report_settings = function(p) {

  p = p %||% list()

  get = function(name, default = NA) {
    x = p[[name]]
    if (is.null(x) || length(x) == 0 || all(is.na(x))) default else x
  }

  reference = suppressWarnings(as.integer(split_csv(get("Reference", ""))))
  reference = reference[!is.na(reference)]

  # documented behaviour since v1.1, when no reference is given the
  # first item is the reference
  if (length(reference) == 0) {
    reference = 1L
  }

  # no more than four checks
  reference = utils::head(reference, 4)

  extension = tolower(get("Format", "docx"))

  list(output_path   = get("OutputPath", file.path(getwd(), "output")),
       infosheets    = isTRUE(as.logical(get("InfoSheets", FALSE))),
       language      = normalize_language(get("Languaje", get("Language", "en"))),
       extension     = extension,
       output_format = if (extension == "docx") "word_document" else paste0(extension, "_document"),
       ranker        = get("ReferToParticipants", "participant"),
       option        = get("ReferToTechnologies", "technology"),
       code_path     = get("BackwardPath", getOption("climmob.code_path", getwd())),
       groups        = split_csv(get("Split", "")),
       reference     = reference,
       # settings not sent by ClimMob yet, read here so ClimMob can add them later
       min_n_group   = as.integer(get("MinGroupSize", 20)),
       balance_threshold = as.numeric(get("BalanceThreshold", 0.25)),
       use_climate   = !isFALSE(as.logical(get("ClimateCovariates", TRUE))),
       feedback_colour = !isFALSE(as.logical(get("FeedbackColour", TRUE))),
       feedback_recommendations = as.character(get("Recommendations", "")),
       feedback_contact = as.character(get("Contact", "")),
       date          = Sys.Date())

}

#' Names of the reference (check) items from their position in the project
get_reference_names = function(cmdatajson, reference) {

  elements = tryCatch(cmdatajson$combination$elements, error = function(e) NULL)

  if (is.null(elements)) {
    return(character(0))
  }

  reference = reference[reference <= length(elements)]

  out = lapply(elements[reference], function(x) x$alias_name)

  as.character(unlist(out))

}

#' Default settings for local runs (see run-local/)
#'
#' Any argument overrides the default
climmob_settings = function(...) {

  s = parse_report_settings(list())
  s$project_name = NA
  s$country = NA
  s$reference_names = character(0)

  dots = list(...)
  s[names(dots)] = dots

  if (!is.null(dots$extension)) {
    s$output_format = if (s$extension == "docx") "word_document" else paste0(s$extension, "_document")
  }

  s

}

#' Decode the variables to analyse sent by ClimMob
#'
#' @param x the list read from the parameters JSON
#' @return a list with data frames for
#'  traits: characteristics to be analysed
#'  tricotVSlocal: the comparison between tested items and the local item
#'  covariates: the explanatory variables
#'  linear: the quantitative (continuous) traits
decode_pars = function(x) {

  result = list()

  # ranking traits
  traits = x[["Characteristics"]]

  if (length(traits) > 0) {
    questions = decode_questions(traits)
    questions$traitOrder = rep("otherTraits", nrow(questions))
    questions$traitOrder[pick_reference_trait(questions$codeQst)] = "referenceTrait"
    result[["traits"]] = questions
  } else {
    result[["traits"]] = character(0L)
  }

  # comparison with the local item
  tricotVSlocal = x[["Performance"]]

  if (length(tricotVSlocal) > 0) {
    questions = vars_to_frame(tricotVSlocal$vars)
    questions$nQst = ncol(questions)
    questions$name = tricotVSlocal$name
    questions$codeQst = tricotVSlocal$codeQst
    result[["tricotVSlocal"]] = questions
  } else {
    result[["tricotVSlocal"]] = character(0L)
  }

  # covariates
  covariates = x[["Explanatory"]]

  if (length(covariates) > 0) {

    covar = data.frame(codeQst = covariates$codeQst,
                       id = covariates$id %||% NA,
                       nameString = vapply(covariates$vars,
                                           function(v) as.character(unlist(v))[1],
                                           character(1)),
                       name = covariates$name,
                       questionAsked = covariates$questionAsked %||% NA,
                       stringsAsFactors = FALSE)

    if (all(is.na(covariates$code))) {
      ass = data.frame(assessmentId = rep("000000000000", nrow(covar)),
                       assessmentName = "Registration")
    } else {
      ass = covariates$code[, c("ass_cod", "ass_desc")]
      names(ass) = c("assessmentId", "assessmentName")
      ass$assessmentId[is.na(ass$assessmentId)] = "000000000000"
      ass$assessmentName[is.na(ass$assessmentName)] = "Registration"
    }

    covar = cbind(ass, covar)
    covar$assessmentName = gsub("[[:punct:]]", "", covar$assessmentName)
    result[["covariates"]] = covar

  } else {
    result[["covariates"]] = character(0L)
  }

  # quantitative traits
  linear = x[["linearRegression"]]

  if (length(linear) > 0) {
    result[["linear"]] = decode_questions(linear)
  } else {
    result[["linear"]] = character(0L)
  }

  result

}

#' Turn the "vars" list sent by ClimMob into a data frame with nameString1, 2...
vars_to_frame = function(vars) {

  q = do.call(rbind, lapply(vars, function(y) unlist(y)))
  q = as.data.frame(q, stringsAsFactors = FALSE)
  names(q) = paste0("nameString", seq_len(ncol(q)))
  q

}

#' Decode a block of questions (ranking traits or quantitative traits)
decode_questions = function(block) {

  questions = vars_to_frame(block$vars)
  nq = ncol(questions)

  questions$nQst = nq
  questions$name = block$name
  questions$codeQst = block$codeQst

  asked = as.data.frame(do.call(rbind, block$questionAsked), stringsAsFactors = FALSE)
  names(asked) = paste0("questionAsked", seq_len(ncol(asked)))
  questions = cbind(questions, asked)

  if (!all(is.na(block$code))) {
    questions$assessmentId = block$code$ass_cod
    questions$assessmentName = block$code$ass_desc
    questions$assessmentDay = block$code$ass_days
  } else {
    questions$assessmentId = "0000000000"
    questions$assessmentName = "Data collection"
    questions$assessmentDay = 0
  }

  questions$assessmentName = gsub("[[:punct:]]", "", questions$assessmentName)

  rownames(questions) = seq_len(nrow(questions))

  questions

}

#' Choose the reference trait for the main analysis
#'
#' The rules are applied in order and the first one that matches wins.
#' This reproduces the behaviour of the original decode_pars().
#'   1. first trait containing "overallperf"
#'   2. last trait containing "overall"
#'   3. first trait containing "performance"
#'   4. first trait containing "generalappreciation"
#'   5. first trait containing "yield"
#'   6. first trait containing "market"
#'   7. otherwise the last trait
#'
#' @param codes the trait codes (codeQst)
#' @return the index of the reference trait
#' @examples pick_reference_trait(c("taste", "yield", "overallperf"))
pick_reference_trait = function(codes) {

  codes = tolower(codes)

  rules = list(c("overallperf", "first"),
               c("overall", "last"),
               c("performance", "first"),
               c("generalappreciation", "first"),
               c("yield", "first"),
               c("market", "first"))

  for (r in rules) {
    hit = which(grepl(r[1], codes, fixed = TRUE))
    if (length(hit) > 0) {
      return(if (r[2] == "first") hit[1] else hit[length(hit)])
    }
  }

  length(codes)

}
