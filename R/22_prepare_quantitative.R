# ................................................................
# Prepare the quantitative (continuous) traits
# ................................................................

#' Organize the quantitative data from a ClimMob project
#'
#' @param cmdata the trial data as data.frame
#' @param pars the decoded parameters, see decode_pars()
#' @param groups columns used to make segments of participants
#' @param id the column in cmdata with the package id
#' @param tech_index the columns in cmdata with the items in each package
#' @return a list, quantitative = FALSE when there is nothing to analyse
organize_quantitative_data = function(cmdata,
                                      pars,
                                      groups = NULL,
                                      id = "package_id",
                                      tech_index = paste0("package_item_", LETTERS[1:3])) {

  quanti_traits = pars[["linear"]]

  if (length(quanti_traits) == 0) {
    return(list(quantitative = FALSE))
  }

  ntech = length(tech_index)

  group = build_groups(cmdata, groups, na_label = "Others",
                       max_groups = 8, min_per_group = 0)

  if (is.null(group)) {
    group = NA
  }

  ids = if (id %in% names(cmdata)) as.vector(cmdata[[id]]) else seq_len(nrow(cmdata))

  quanti_traits$codeQst = rename_duplicates(quanti_traits$codeQst)

  # a name is numbered only when it repeats within the same data collection moment
  key = paste(quanti_traits$name, quanti_traits$assessmentName)
  dup = duplicated(key) | duplicated(key, fromLast = TRUE)
  quanti_traits$name[dup] = rename_duplicates(quanti_traits$name[dup], sep = " ")

  quanti_dat = list()

  for (i in seq_len(nrow(quanti_traits))) {

    patterns = as.vector(unlist(quanti_traits[i, paste0("nameString", seq_len(ntech))]))

    cols = find_columns(tolower(patterns), tolower(names(cmdata)))

    if (any(is.na(cols))) {
      message("Quantitative trait '", quanti_traits$name[i], "' skipped, columns not found")
      next
    }

    cols = names(cmdata)[match(cols, tolower(names(cmdata)))]

    # who submitted the data in this data collection moment
    enumerator = grep(paste0("ASS", quanti_traits$assessmentId[i]), names(cmdata), value = TRUE)
    enumerator = enumerator[grepl("submitted_by", enumerator)]
    enumerator = if (length(enumerator) > 0) as.vector(cmdata[[enumerator[1]]]) else NA

    quanti_dat[[length(quanti_dat) + 1]] =
      data.frame(id = rep(ids, ntech),
                 technology = as.vector(unlist(cmdata[, tech_index])),
                 group = rep(group, length.out = nrow(cmdata) * ntech),
                 value = suppressWarnings(as.numeric(unlist(cmdata[, cols]))),
                 trait = quanti_traits$name[i],
                 trait_code = quanti_traits$codeQst[i],
                 data_collection = quanti_traits$assessmentName[i],
                 enumerator = rep(enumerator, length.out = nrow(cmdata) * ntech),
                 stringsAsFactors = FALSE)

  }

  if (length(quanti_dat) == 0) {
    return(list(quantitative = FALSE))
  }

  # possible outliers, from the boxplot rule
  outliers = lapply(quanti_dat, function(x) {
    out = unique(grDevices::boxplot.stats(x$value)$out)
    x[x$value %in% out, ]
  })

  outliers = do.call("rbind", outliers)

  if (nrow(outliers) > 1) {
    outliers = outliers[order(suppressWarnings(as.integer(outliers$id))), ]
    rownames(outliers) = NULL
  }

  if (length(group) == 1) {
    group = NULL
  }

  list(quantitative = TRUE,
       quanti_dat = quanti_dat,
       group = group,
       outliers = outliers)

}

#' Placeholder used when the quantitative data could not be prepared
error_data_quanti_dat = list(quantitative = FALSE)
