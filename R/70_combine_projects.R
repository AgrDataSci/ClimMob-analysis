# ................................................................
# Analyse one or more ClimMob projects outside ClimMob
#
# On the ClimMob server the parameters of the analysis (which questions
# are traits, covariates or measurements) come with the request. When
# you fetch data yourself with ClimMobTools::getDataCM() there is no
# request, so these functions build the parameters from the metadata
# that comes with the data.
#
#   project_catalog()        list every variable of a project and its role
#   build_pars_from_cmlist() parameters for one project
#   suggest_harmonization()  a table to align variables across projects
#   combine_projects()       one data set from several projects
#   pars_from_harmonization() parameters for the combined data
#
# See run-local/ for complete examples and docs/combine-projects.md.
# ................................................................

#' ODK system fields that are never analysed
system_field_pattern = paste0("^(surveyid|originid|_submitted_by|_xform_id_string|_submitted_date|",
                              "_geopoint|_longitude|_latitude|_elevation|_precision|_active|qst162|",
                              "qst163|cal_qst163|clm_.*|clc_.*|farmername|telephone|instancename|",
                              "rowuuid|assessmentdate|geotrial|pointofdelivery|.*_oth)$")

#' Labels for common ClimMob trait codes
default_trait_labels = function() {
  c(overallchar = "Overall preference", overallperf = "Overall performance", overall = "Overall",
    yield = "Yield", taste = "Taste", marketability = "Marketability", maturity = "Maturity",
    germination = "Germination", grainsize = "Grain size", graincolour = "Grain colour",
    diseasesresistance = "Disease resistance", droughttolerance = "Drought tolerance",
    floodtolerance = "Flood tolerance", fallarmyworm = "Fall armyworm resistance",
    pestresistance = "Pest resistance", vigor = "Vigour", vigour = "Vigour",
    plantpopulation = "Plant population", numberofcobs = "Number of cobs",
    yieldperse = "Yield (measured)", diseaseseverit = "Disease severity",
    gender = "Gender", gender1 = "Gender", age = "Age", education = "Education")
}

#' Make a code or a moment name safe to use in column names
#' @examples clean_code("Agronomic performance"); clean_code("qst_Yield-2")
clean_code = function(x) {
  x = tolower(as.character(x))
  x = sub("^qst_", "", x)
  gsub("[^a-z0-9]", "", x)
}

#' Label for a variable code
trait_label = function(code, labels = default_trait_labels()) {
  out = labels[code]
  out[is.na(out)] = title_case(code[is.na(out)])
  unname(out)
}

#' List every variable of a ClimMob project and its role in the analysis
#'
#' @param cmlist the CM_list returned by ClimMobTools::getDataCM(..., as.data.frame = FALSE)
#' @return a data.frame with one row per column of the data, with
#'  project, moment_id, moment, day, column, code, role
#'  (pos, neg, a, b, c, value, 1, 2, 3), type (ranking, quantitative,
#'  covariate, local), question and odktype
project_catalog = function(cmlist) {

  project = as.character(cmlist$project$project_cod %||% NA)

  field_rows = function(fields, prefix, moment_id, moment, day) {
    if (!is.data.frame(fields) || nrow(fields) == 0) return(NULL)
    data.frame(project = project,
               moment_id = moment_id,
               moment = moment,
               day = day,
               field = fields$name,
               column = paste0(prefix, fields$name),
               question = fields$desc %||% NA,
               odktype = fields$odktype %||% NA,
               stringsAsFactors = FALSE)
  }

  rows = list(field_rows(cmlist$registry$fields, "REG_", "registration", "Registration", 0))

  ass = cmlist$assessments

  if (is.data.frame(ass) && nrow(ass) > 0) {
    for (i in seq_len(nrow(ass))) {
      rows[[length(rows) + 1]] = field_rows(ass$fields[[i]], paste0("ASS", ass$code[i], "_"),
                                            ass$code[i], ass$desc[i], ass$intervalindays[i])
    }
  }

  cat = do.call("rbind", rows)

  f = cat$field
  cat$type = NA_character_
  cat$role = NA_character_
  cat$code = NA_character_

  # ranking traits, char_<code>_pos / _neg
  rk = grepl("^char_.+_(pos|neg)$", f)
  cat$type[rk] = "ranking"
  cat$role[rk] = sub("^.*_(pos|neg)$", "\\1", f[rk])
  cat$code[rk] = sub("^char_(.+)_(pos|neg)$", "\\1", f[rk])

  # comparison with the local item, perf_<code>_1/2/3
  lc = grepl("^perf_.+_[1-9]$", f)
  cat$type[lc] = "local"
  cat$role[lc] = sub("^.*_([1-9])$", "\\1", f[lc])
  cat$code[lc] = sub("^perf_(.+)_[1-9]$", "\\1", f[lc])

  # measurements per option, numeric fields ending in _a/_b/_c
  num = cat$odktype %in% c("integer", "decimal")
  qt = is.na(cat$type) & num & grepl("_[abc]$", f)
  cat$type[qt] = "quantitative"
  cat$role[qt] = sub("^.*_([abc])$", "\\1", f[qt])
  cat$code[qt] = sub("_[abc]$", "", f[qt])

  # covariates, one answer per participant
  cv = is.na(cat$type) & cat$odktype %in% c("integer", "decimal", "select one") &
    !grepl(system_field_pattern, f) & !grepl("_[abc]$", f)
  cat$type[cv] = "covariate"
  cat$role[cv] = "value"
  cat$code[cv] = f[cv]

  cat = cat[!is.na(cat$type), , drop = FALSE]
  cat$code = clean_code(cat$code)
  rownames(cat) = NULL

  cat

}

#' Build the analysis parameters from the catalog of one project
#'
#' The result has the same structure as decode_pars() gives for a
#' ClimMob request, so it can go straight to run_climmob_analysis().
#'
#' @param catalog output of project_catalog(), or the harmonised
#'  catalog used by pars_from_harmonization()
#' @param labels named vector with labels for the codes
#' @param covariates logical, include the covariates
build_pars_from_catalog = function(catalog, labels = default_trait_labels(), covariates = TRUE) {

  cat = catalog
  if (!"label" %in% names(cat)) cat$label = trait_label(cat$code, labels)

  key = paste(cat$moment_id, cat$code)

  # ranking traits
  rk = cat[cat$type == "ranking", ]
  traits = character(0L)
  if (nrow(rk) > 0) {
    k = unique(paste(rk$moment_id, rk$code))
    traits = do.call("rbind", lapply(k, function(z) {
      x = rk[paste(rk$moment_id, rk$code) == z, ]
      pos = x[x$role == "pos", ][1, ]
      neg = x[x$role == "neg", ][1, ]
      data.frame(nameString1 = pos$column, nameString2 = neg$column, nQst = 2,
                 name = pos$label, codeQst = pos$code,
                 questionAsked1 = pos$question, questionAsked2 = neg$question,
                 assessmentId = pos$moment_id, assessmentName = pos$moment,
                 assessmentDay = pos$day, stringsAsFactors = FALSE)
    }))
    traits = traits[stats::complete.cases(traits[, c("nameString1", "nameString2")]), ]
    traits = traits[order(traits$assessmentDay), ]
    traits$assessmentName = gsub("[[:punct:]]", "", traits$assessmentName)
    traits$traitOrder = "otherTraits"
    traits$traitOrder[pick_reference_trait(traits$codeQst)] = "referenceTrait"
    rownames(traits) = NULL
  }

  # comparison with the local item
  lc = cat[cat$type == "local", ]
  local = character(0L)
  if (nrow(lc) >= 3) {
    lc = lc[order(lc$role), ][1:3, ]
    local = data.frame(nameString1 = lc$column[1], nameString2 = lc$column[2],
                       nameString3 = lc$column[3], nQst = 3, name = "Tricot vs local",
                       codeQst = "tricotVSlocal", stringsAsFactors = FALSE)
  }

  # measurements
  qt = cat[cat$type == "quantitative", ]
  linear = character(0L)
  if (nrow(qt) > 0) {
    k = unique(paste(qt$moment_id, qt$code))
    linear = do.call("rbind", lapply(k, function(z) {
      x = qt[paste(qt$moment_id, qt$code) == z, ]
      x = x[match(c("a", "b", "c"), x$role), ]
      if (any(is.na(x$column))) return(NULL)
      data.frame(nameString1 = x$column[1], nameString2 = x$column[2], nameString3 = x$column[3],
                 nQst = 3, name = x$label[1], codeQst = x$code[1],
                 questionAsked1 = x$question[1], questionAsked2 = x$question[2],
                 questionAsked3 = x$question[3],
                 assessmentId = x$moment_id[1], assessmentName = gsub("[[:punct:]]", "", x$moment[1]),
                 assessmentDay = x$day[1], stringsAsFactors = FALSE)
    }))
    if (is.null(linear)) linear = character(0L)
  }

  # covariates
  cv = cat[cat$type == "covariate", ]
  covar = character(0L)
  if (isTRUE(covariates) && nrow(cv) > 0) {
    cv = cv[!duplicated(cv$column), ]
    covar = data.frame(assessmentId = ifelse(cv$moment_id == "registration", "000000000000", cv$moment_id),
                       assessmentName = gsub("[[:punct:]]", "", cv$moment),
                       codeQst = cv$code,
                       id = NA,
                       nameString = cv$column,
                       name = cv$label,
                       questionAsked = cv$question,
                       stringsAsFactors = FALSE)
  }

  list(traits = traits, tricotVSlocal = local, covariates = covar, linear = linear)

}

#' Analysis parameters for one ClimMob project
#'
#' @param cmlist the CM_list returned by ClimMobTools::getDataCM(..., as.data.frame = FALSE)
#' @examples
#' \dontrun{
#' cmlist = ClimMobTools::getDataCM(key, project = "MYPROJ", userowner = "me",
#'                                  server = "1000FARMS", as.data.frame = FALSE)
#' pars = build_pars_from_cmlist(cmlist)
#' }
build_pars_from_cmlist = function(cmlist, ...) {
  build_pars_from_catalog(project_catalog(cmlist), ...)
}

#' Suggest how to align the variables of several projects
#'
#' Returns one row per variable and project. Review the columns
#' "trait" (the common code), "moment" (the common data collection
#' moment, a short code), "moment_label" (its name in the report),
#' "label" (the trait name shown in the report) and "include",
#' for example in a spreadsheet, and pass the table to combine_projects().
#'
#' @param catalogs a list of outputs of project_catalog()
#' @param synonyms named vector, codes on the left are replaced by the code on the right
suggest_harmonization = function(catalogs,
                                 synonyms = c(overallperf = "overall", overallchar = "overall",
                                              overallpref = "overall", gender1 = "gender",
                                              yieldperse = "yieldmeasured")) {

  cat = do.call("rbind", catalogs)

  trait = cat$code
  trait[trait %in% names(synonyms)] = synonyms[trait[trait %in% names(synonyms)]]

  data.frame(project = cat$project,
             column = cat$column,
             type = cat$type,
             role = cat$role,
             original_code = cat$code,
             original_moment = cat$moment,
             question = cat$question,
             trait = trait,
             moment = ifelse(cat$type == "covariate", "covariate", clean_code(cat$moment)),
             moment_label = ifelse(cat$type == "covariate", "Registration", cat$moment),
             day = cat$day,
             label = trait_label(trait),
             include = cat$type != "covariate" | trait %in% c("gender", "age", "education"),
             stringsAsFactors = FALSE)

}

#' Combine several projects into one data set
#'
#' @param projects named list of data.frames, each the output of
#'  as.data.frame(cmlist, tidynames = FALSE, pivot.wider = TRUE), named by project code
#' @param harmonization output of suggest_harmonization(), reviewed
#' @param item_map optional data.frame with columns project, original and item
#'  to give the same name to the same item across projects
#' @return a data.frame with harmonised columns <moment>_<trait>_<role>
#'  for traits and measurements, cov_<trait> for covariates, and a unique
#'  participant id "<project>-<package id>"
combine_projects = function(projects, harmonization, item_map = NULL) {

  h = harmonization[as.logical(harmonization$include), , drop = FALSE]

  keep_always = "^(project_|package_item_|REG_farmername$|REG_village$|REG_district$|REG__submitted_date$|REG__submitted_by$|.*_longitude$|.*_latitude$|.*_plantingdate$|REG_clm_start$)"

  out = lapply(names(projects), function(pj) {

    d = projects[[pj]]
    hp = h[h$project == pj & h$column %in% names(d), , drop = FALSE]

    newname = ifelse(hp$type == "covariate", paste0("cov_", hp$trait),
                     paste0(hp$moment, "_", hp$trait, "_", hp$role))

    x = d[, hp$column, drop = FALSE]
    names(x) = newname
    x = x[, !duplicated(names(x)), drop = FALSE]

    base = d[, grepl(keep_always, names(d)), drop = FALSE]

    # submission dates and enumerators, renamed after the common moment
    mm = unique(hp[hp$type != "covariate", c("column", "moment")])
    mm$code = sub("^ASS([^_]+)_.*$", "\\1", mm$column)
    mm = mm[!duplicated(mm$moment), ]
    for (k in seq_len(nrow(mm))) {
      for (what in c("_submitted_date", "_submitted_by")) {
        src = paste0("ASS", mm$code[k], "_", what)
        if (src %in% names(d)) base[[paste0(mm$moment[k], "_", what)]] = d[[src]]
      }
    }

    id = if ("REG_qst162" %in% names(d)) d$REG_qst162 else seq_len(nrow(d))
    base$REG_qst162 = paste(pj, id, sep = "-")
    base$project_code = pj

    # common item names
    if (!is.null(item_map)) {
      im = item_map[item_map$project == pj, ]
      for (col in grep("^package_item_", names(base), value = TRUE)) {
        hit = match(base[[col]], im$original)
        base[[col]][!is.na(hit)] = im$item[hit[!is.na(hit)]]
      }
    }

    cbind(base, x)

  })

  bind_rows_fill(out)

}

#' Bind data.frames with different columns, filling with NA
bind_rows_fill = function(x) {

  cols = unique(unlist(lapply(x, names)))

  x = lapply(x, function(d) {
    for (m in setdiff(cols, names(d))) d[[m]] = NA
    d[, cols, drop = FALSE]
  })

  out = do.call("rbind", x)
  rownames(out) = NULL
  out

}

#' Analysis parameters for data combined with combine_projects()
#'
#' @param harmonization the reviewed output of suggest_harmonization()
#' @param add_project logical, add the project as a covariate
pars_from_harmonization = function(harmonization, add_project = TRUE) {

  h = harmonization[as.logical(harmonization$include), , drop = FALSE]

  day = stats::aggregate(day ~ moment, data = h[!is.na(h$day), ], FUN = stats::median)

  # the label of a moment is the first original name given to it
  if (!"moment_label" %in% names(h)) h$moment_label = h$moment
  labels = tapply(h$moment_label, h$moment, function(x) x[1])

  cat = unique(data.frame(moment_id = h$moment,
                          moment = as.character(labels[h$moment]),
                          day = day$day[match(h$moment, day$moment)],
                          column = ifelse(h$type == "covariate", paste0("cov_", h$trait),
                                          paste0(h$moment, "_", h$trait, "_", h$role)),
                          code = h$trait,
                          label = h$label,
                          role = h$role,
                          type = h$type,
                          question = h$question,
                          stringsAsFactors = FALSE))

  cat = cat[!duplicated(cat$column), ]
  cat$moment_id[cat$type == "covariate"] = "registration"
  cat$moment[cat$type == "covariate"] = "Registration"

  pars = build_pars_from_catalog(cat)

  if (isTRUE(add_project)) {
    p = data.frame(assessmentId = "000000000000", assessmentName = "Registration",
                   codeQst = "project", id = NA, nameString = "project_code",
                   name = "Project", questionAsked = "", stringsAsFactors = FALSE)
    pars$covariates = if (is.data.frame(pars$covariates)) rbind(pars$covariates, p) else p
  }

  pars

}
