# ................................................................
# Small helpers used across the workflow
# All of them are base R so they can be unit tested without the
# analysis packages (see tests/)
# ................................................................

#' Use a default value when x is NULL or empty
`%||%` = function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

#' Split a comma separated string into a clean character vector
#' @examples split_csv("a, b,,c") # "a" "b" "c"
split_csv = function(x) {

  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    return(character(0))
  }

  x = trimws(unlist(strsplit(as.character(x), ",")))

  x[!is.na(x) & nzchar(x)]

}

#' Put sentences in title case
#' @examples title_case("the apple tree")
title_case = function(x) {
  gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", x, perl = TRUE)
}

#' Pluralize the labels used in the report
#' @examples pluralize("variety"); pluralize("farmer")
pluralize = function(x, p = "s") {

  irregular = c(variety = "varieties",
                variedad = "variedades",
                opcion = "opciones",
                technology = "technologies")

  if (length(x) == 1 && x %in% names(irregular)) {
    return(unname(irregular[x]))
  }

  paste0(x, p)

}

#' Paste strings with a final conjunction
#' @examples paste3(LETTERS[1:3]); paste3(LETTERS[1:2], lan = "fr")
paste3 = function(x, lan = "en") {

  x = x[!is.na(x)]

  if (length(x) <= 1) {
    return(x)
  }

  conj = c(en = " and ", pt = " e ", fr = " et ",
           es = " y ", sw = " na ", no = " og ")

  conj = if (lan %in% names(conj)) conj[[lan]] else conj[["en"]]

  paste0(paste(x[-length(x)], collapse = ", "), conj, x[length(x)])

}

#' Rename duplicated values adding a sequential suffix
#' @examples rename_duplicates(c("Pear", "Apple", "Pear"))
rename_duplicates = function(x, rename.with = "numbers", sep = "") {

  dups = unique(x[duplicated(x)])

  for (d in dups) {

    is_d = x == d

    index = seq_len(sum(is_d))

    if (rename.with == "letters") {
      index = letters[index]
    }

    x[is_d] = paste(x[is_d], index, sep = sep)

  }

  x

}

#' Abbreviate long names
reduce = function(x, nchars = 8, ...) {

  long = nchar(x) > nchars

  x[long] = abbreviate(x[long], ...)

  x

}

# ................................................................
# Colours
#
# All charts use three ColorBrewer palettes, chosen to be readable by
# people with colour vision deficiency and in black and white:
#   diverging    BrBG    (worth maps, sentiment, anything around zero)
#   sequential   Greens  (ordered values, single-series bars, maps)
#   qualitative  Paired  (groups, series with no order)
# The hex codes are copied from RColorBrewer so no extra package is
# needed. Use these helpers, never a hard-coded colour, in new charts.
# ................................................................

BREWER = list(
  BrBG = c("#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", "#f5f5f5",
           "#c7eae5", "#80cdc1", "#35978f", "#01665e"),
  Greens = c("#f7fcf5", "#e5f5e0", "#c7e9c0", "#a1d99b", "#74c476",
             "#41ab5d", "#238b45", "#006d2c", "#00441b"),
  Paired = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99",
             "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a",
             "#ffff99", "#b15928"))

#' Diverging palette (BrBG), n colours from brown through grey to teal
#' @examples
#' pal_diverging(5)
pal_diverging = function(n) grDevices::colorRampPalette(BREWER$BrBG)(n)

#' Sequential palette (Greens), n colours from light to dark green.
#' The two lightest tones are left out so bars stay visible on white.
pal_sequential = function(n) grDevices::colorRampPalette(BREWER$Greens[3:9])(n)

#' Qualitative palette (Paired), n colours for groups without an order
pal_qualitative = function(n) {
  if (n <= length(BREWER$Paired)) BREWER$Paired[seq_len(n)]
  else grDevices::colorRampPalette(BREWER$Paired)(n)
}

#' Named colours for single elements, from the same palettes
#' @examples
#' pal_one("dark")
pal_one = function(what = c("dark", "mid", "light", "negative", "positive", "neutral")) {
  what = match.arg(what)
  c(dark = BREWER$Greens[8], mid = BREWER$Greens[6], light = BREWER$Greens[4],
    negative = BREWER$BrBG[2], positive = BREWER$BrBG[8], neutral = "#bababa")[[what]]
}

#' Kept for older code, same as pal_sequential()
col_pallet = pal_sequential

#' Find the column names that match a set of patterns
#'
#' An exact match is preferred. If there is none, the first column
#' whose name contains the pattern is used (this is how the original
#' workflow matched ClimMob variables). Returns NA when nothing matches.
#'
#' @param patterns character vector with the names sent by ClimMob
#' @param col_names the column names in the data
#' @param anchor_end logical, if TRUE the pattern must match the end of
#'  the column name (used for covariates)
#' @examples
#' find_columns(c("a_pos", "b"), c("x_a_pos", "b", "b_2"))
find_columns = function(patterns, col_names, anchor_end = FALSE) {

  vapply(patterns, function(p) {

    if (is.na(p) || !nzchar(p)) {
      return(NA_character_)
    }

    if (p %in% col_names) {
      return(p)
    }

    rx = if (isTRUE(anchor_end)) paste0(p, "$") else p

    hit = tryCatch(grep(rx, col_names, value = TRUE),
                   error = function(e) grep(p, col_names, value = TRUE, fixed = TRUE))

    if (length(hit) == 0) NA_character_ else hit[1]

  }, character(1), USE.NAMES = FALSE)

}

#' TRUE if the object can be drawn with plot()
is_plot = function(x) {
  inherits(x, c("gg", "ggplot", "patchwork", "igraph"))
}

#' Run code with a fixed random seed, restoring the previous state after
with_seed = function(seed, code) {

  had_seed = exists(".Random.seed", envir = globalenv(), inherits = FALSE)

  if (had_seed) {
    old_seed = get(".Random.seed", envir = globalenv())
    on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
  } else {
    on.exit(suppressWarnings(rm(".Random.seed", envir = globalenv())), add = TRUE)
  }

  set.seed(seed)

  code

}
