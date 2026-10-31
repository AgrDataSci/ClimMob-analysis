# ................................................................
# Groups (segments) of participants
# Shared by the ranking and the quantitative data preparation.
# ................................................................

#' Build the grouping vector used to split the analysis
#'
#' @param cmdata the trial data as data.frame
#' @param groups character, names (or parts of names) of the columns
#'  in cmdata to group by, as sent in the "Split" parameter
#' @param na_label label for participants with missing group
#' @param max_groups the largest groups are kept, the rest become "Others"
#' @param min_per_group if any group has fewer participants than this,
#'  no grouping is used (NULL is returned). Use 0 to skip this check
#' @return a character vector with one value per row of cmdata, or NULL
build_groups = function(cmdata, groups, na_label = "Not specified",
                        max_groups = 8, min_per_group = 5) {

  if (length(groups) == 0) {
    return(NULL)
  }

  cols = find_columns(groups, names(cmdata))
  cols = unique(cols[!is.na(cols)])

  if (length(cols) == 0) {
    return(NULL)
  }

  if (length(cols) == 1) {

    group = as.character(cmdata[[cols]])

    # too many missing values, do not split
    if (sum(is.na(group)) > length(group) * 0.5) {
      return(NULL)
    }

    group[is.na(group)] = na_label

  } else {

    g = as.data.frame(lapply(cmdata[cols], as.character), stringsAsFactors = FALSE)
    g[is.na(g)] = na_label
    group = apply(g, 1, paste, collapse = " - ")

  }

  # keep the largest groups and put the others together
  if (length(unique(group)) > max_groups) {
    keep_group = names(sort(table(group), decreasing = TRUE))[seq_len(max_groups)]
    group[!group %in% keep_group] = "Others"
  }

  if (min_per_group > 0 && any(table(group) < min_per_group)) {
    return(NULL)
  }

  as.vector(group)

}
