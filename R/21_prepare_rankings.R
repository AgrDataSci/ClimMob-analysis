# ................................................................
# Prepare the tricot ranking data
#
# Cleans the answers of each ranking trait, drops traits with too
# few valid answers and returns rank_dat, the object used by all the
# ranking analyses and by the report. The cleaned data is returned in
# rank_dat$cmdata so every later step works on the same data.
# ................................................................

#' Clean the tricot answers of one trait
#'
#' Rows are set to NA when
#'   - the answer is "Not observed"
#'   - all answers are "Tie"
#'   - best and worst are the same letter
#'   - any answer is not one of A, B, C (this also removes partial ties)
#'
#' @param x a data.frame with the answer columns of one trait
#'  (best then worst)
#' @return x with invalid rows set to NA
#' @examples
#' x = data.frame(best = c("A", "B", "Tie", "C", "A"),
#'                worst = c("B", "B", "Tie", "Not observed", "D"))
#' clean_tricot_answers(x)
clean_tricot_answers = function(x) {

  x = as.data.frame(lapply(x, as.character), stringsAsFactors = FALSE)

  x[answer_status(x) != "valid", ] = NA

  x

}

#' Organize the tricot ranking data from a ClimMob project
#'
#' @param cmdata the trial data as data.frame
#' @param pars the decoded parameters, see decode_pars()
#' @param groups columns used to make segments of participants
#' @param option_label how the report refers to the tested items
#' @param ranker_label how the report refers to the participants
#' @param reference_tech names of the reference (check) items
#' @param project_name the project name
#' @param tech_index the columns in cmdata with the items in each package
#' @param minN minimum number of valid answers for a trait to be kept
#' @param minitem minimum number of times each item must be tested in a trait
#' @return the rank_dat list
organize_ranking_data = function(cmdata,
                                 pars,
                                 groups = NULL,
                                 option_label = "technology",
                                 ranker_label = "participant",
                                 reference_tech = character(0),
                                 project_name = NA,
                                 tech_index = paste0("package_item_", LETTERS[1:3]),
                                 minN = 5,
                                 minitem = 2) {

  if (length(pars$traits) == 0) {
    stop("No ranking traits were sent in the request")
  }

  missing_items = setdiff(tech_index, names(cmdata))

  if (length(missing_items) > 0) {
    stop("Columns with the tested items not found: ", paste(missing_items, collapse = ", "))
  }

  nranker = nrow(cmdata)
  itemnames = tech_index
  items = sort(unique(as.vector(unlist(cmdata[, itemnames]))))
  items = items[!is.na(items)]

  covar = pars$covariates
  nquest = pars$traits$nQst[1]

  # reference items, the first item is used when the names are not valid
  reference_tech = intersect(reference_tech, items)
  if (length(reference_tech) == 0) {
    reference_tech = items[1]
  }

  # all groups are kept here, get_group_analysis() decides which ones
  # have enough data to be analysed
  group = build_groups(cmdata, groups, na_label = "Not specified",
                       max_groups = 8, min_per_group = 0)

  # the same trait can be assessed in different data collection moments
  trait = pars$traits
  trait$codeQst = rename_duplicates(trait$codeQst)

  # a name is numbered only when it repeats within the same data
  # collection moment, otherwise the moment already tells them apart
  dup_key = paste(trait$name, trait$assessmentName)
  trait$name[duplicated(dup_key) | duplicated(dup_key, fromLast = TRUE)] =
    rename_duplicates(trait$name[duplicated(dup_key) | duplicated(dup_key, fromLast = TRUE)], sep = " ")

  reference_trait = which(trait$traitOrder == "referenceTrait")[1]

  if (is.na(reference_trait)) {
    reference_trait = nrow(trait)
  }

  tricotVSlocal = length(pars$tricotVSlocal) > 0

  trait_list = list()
  trait_dropped = character()

  for (i in seq_len(nrow(trait))) {

    patterns = as.character(trait[i, paste0("nameString", seq_len(nquest))])

    trait_i = find_columns(patterns, names(cmdata))

    if (any(is.na(trait_i))) {
      message("Trait '", trait$name[i], "' dropped, columns not found: ",
              paste(patterns[is.na(trait_i)], collapse = ", "))
      trait_dropped = c(trait_dropped, paste0(trait$name[i], " [", trait$assessmentName[i], "]"))
      next
    }

    cmdata[trait_i] = clean_tricot_answers(cmdata[trait_i])

    keep = stats::complete.cases(cmdata[trait_i])

    # enough valid answers and every item tested at least minitem times
    enough_n = sum(keep) > minN
    enough_items = all(table(unlist(cmdata[keep, itemnames])) >= minitem)

    if (!isTRUE(enough_n && enough_items)) {
      trait_dropped = c(trait_dropped, paste0(trait$name[i], " [", trait$assessmentName[i], "]"))
      next
    }

    result = list()

    # comparison with the local item, only for the reference trait
    if (i == reference_trait && tricotVSlocal) {
      local = check_tricot_vs_local(cmdata, pars$tricotVSlocal, itemnames, keep)
      if (!local$use) {
        tricotVSlocal = FALSE
        if (local$tested) trait_dropped = c(trait_dropped, "Comparison with local")
      }
      result[["keep2"]] = local$keep2
      result[["tricotVSlocal"]] = local$strings
    }

    result[["keep"]] = keep
    result[["strings"]] = trait_i
    result[["name"]] = trait$name[i]
    result[["code"]] = trait$codeQst[i]
    result[["question"]] = paste(trait[i, paste0("questionAsked", seq_len(nquest))], collapse = ", ")
    result[["assessment"]] = trait$assessmentName[i]
    result[["day"]] = trait$assessmentDay[i]
    result[["assessmentid"]] = trait$assessmentId[i]

    trait_list[[trait$codeQst[i]]] = result

  }

  if (length(trait_list) == 0) {
    stop("None of the ", nrow(trait), " traits has enough valid answers yet ",
         "(more than ", minN, " complete answers and each ", option_label,
         " tested at least ", minitem, " times)")
  }

  # index of the reference trait among the traits kept
  reference_trait_index = which(names(trait_list) == trait$codeQst[reference_trait])

  # if the reference trait was dropped, use the last trait kept
  if (length(reference_trait_index) == 0) {
    reference_trait_index = length(trait_list)
    tricotVSlocal = FALSE
  }

  ovname = tolower(trait_list[[reference_trait_index]]$name)

  if (length(unique(trait$assessmentName)) > 1) {
    traits_names = vapply(trait_list, function(x) {
      paste0(x$name, " [", title_case(x$assessment), "]")
    }, character(1), USE.NAMES = FALSE)
  } else {
    traits_names = vapply(trait_list, function(x) x$name, character(1), USE.NAMES = FALSE)
  }

  traits_code = vapply(trait_list, function(x) x$code, character(1), USE.NAMES = FALSE)
  traits_code = gsub("[[:punct:]]", "", traits_code)

  list(projname = project_name,
       option = option_label,
       ranker = ranker_label,
       nranker = nranker,
       technologies = items,
       technologies_index = itemnames,
       assessment_order = unique(trait$assessmentName),
       reference_trait = ovname,
       reference_trait_index = reference_trait_index,
       reference_tech = reference_tech,
       trait_names = traits_names,
       trait_code = traits_code,
       trait_list = trait_list,
       trait_dropped = trait_dropped,
       covarTRUE = length(covar) > 0,
       covar = covar,
       group = group,
       comparison_with_local = tricotVSlocal,
       cmdata = cmdata)

}

#' Check if the comparison with the local item can be used
check_tricot_vs_local = function(cmdata, tricotVSlocal, itemnames, keep,
                                 min_prop = 0.90) {

  cols = intersect(paste0("nameString", 1:3), names(tricotVSlocal))
  strings = as.character(unlist(tricotVSlocal[1, cols]))
  strings = strings[strings %in% names(cmdata)]

  if (length(strings) != 3) {
    return(list(use = FALSE, tested = FALSE,
                keep2 = rep(FALSE, nrow(cmdata)), strings = character()))
  }

  keep2 = keep & stats::complete.cases(cmdata[c(itemnames, strings)])

  use = sum(keep2) / sum(keep) >= min_prop

  list(use = use, tested = TRUE, keep2 = keep2, strings = strings)

}

#' Placeholder used when the ranking data could not be prepared
error_data_rank_dat = list(projname = "",
                           option = "",
                           ranker = "",
                           nranker = 0,
                           technologies = character(),
                           technologies_index = character(),
                           assessment_order = "",
                           reference_trait = "",
                           reference_trait_index = 1,
                           reference_tech = character(),
                           trait_names = character(),
                           trait_code = character(),
                           trait_list = list(),
                           trait_dropped = character(),
                           covarTRUE = FALSE,
                           covar = data.frame(),
                           group = NULL,
                           comparison_with_local = FALSE,
                           cmdata = NULL)
