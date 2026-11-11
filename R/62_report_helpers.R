# ................................................................
# Helpers used inside the report templates
# ................................................................

#' Numbered table and figure captions
#'
#' The numbers follow the order in which captions are made, so they
#' stay right when sections are left out of a report.
new_caption_counter = function() {

  e = new.env()
  e$table = 0
  e$figure = 0

  e$tab = function(text) {
    e$table = e$table + 1
    paste0(tr("table"), " ", e$table, ". ", text)
  }

  e$fig = function(text) {
    e$figure = e$figure + 1
    paste0(tr("figure"), " ", e$figure, ". ", text)
  }

  e$next_tab = function() e$table + 1
  e$next_fig = function() e$figure + 1

  e

}

#' A table with translated column names
#'
#' @param x a data.frame
#' @param keys translation keys for the columns, in order, or NA to keep a name
#' @param caption the caption
#' Set the relative column widths of a pipe table
#'
#' Pandoc takes the relative widths of a wrapped table from the dashes in
#' the line under the header. kable makes them as wide as the header text,
#' which squeezes short headers over long cells. This rewrites that line.
#'
#' @param k a knitr_kable object in pipe format
#' @param widths relative widths, one per column
#' @examples
#' set_col_widths(knitr::kable(head(mtcars[, 1:3])), c(3, 1, 1))
set_col_widths = function(k, widths) {
  i = which(grepl("^\\|?:?-{2,}", as.character(k)))[1]
  if (is.na(i)) return(k)
  cells = strsplit(gsub("^\\||\\|$", "", k[i]), "|", fixed = TRUE)[[1]]
  n = pmax(3, round(60 * widths / sum(widths)))
  new = vapply(seq_along(cells), function(j) {
    left = startsWith(trimws(cells[j]), ":"); right = endsWith(trimws(cells[j]), ":")
    paste0(if (left) ":" else "", strrep("-", n[j]), if (right) ":" else "")
  }, character(1))
  k[i] = paste0("|", paste(new, collapse = "|"), "|")
  k
}

kable_tr = function(x, keys = NULL, caption = NULL, digits = 3, widths = NULL) {

  if (!is.null(keys)) {
    lab = vapply(seq_along(keys), function(i) {
      if (is.na(keys[i])) names(x)[i] else tr(keys[i])
    }, character(1))
    names(x)[seq_along(lab)] = lab
  }

  k = knitr::kable(x, caption = caption, row.names = FALSE, align = "l", digits = digits)

  if (is.null(widths)) widths = content_widths(x, digits)
  set_col_widths(k, widths)

}

#' Relative column widths from the content of a table
#'
#' Word gives every column the same share when a table is wider than the
#' page, so short numeric columns get as much room as long text. This
#' weighs each column by the longest word in its header and by the
#' typical length of its cells, so text columns get the space.
#'
#' @param x the data.frame behind the table
#' @param digits digits used to print numbers
#' @return a numeric vector, one weight per column
#' @examples
#' content_widths(data.frame(item = c("Hybrid 01", "Hybrid 02"), n = c(10, 12),
#'                           note = c("a long sentence here", "short")))
content_widths = function(x, digits = 3) {
  vapply(seq_along(x), function(j) {
    v = x[[j]]
    v = if (is.numeric(v)) format(round(v, digits), trim = TRUE) else as.character(v)
    v = v[!is.na(v) & nzchar(trimws(v))]
    n = nchar(v)
    # the typical cell, but never far below the longest one, so a column
    # with a few long entries still gets room for them
    cells = if (length(n) == 0) 0 else max(stats::quantile(n, 0.9, names = FALSE), 0.6 * max(n))
    header = max(nchar(strsplit(names(x)[j], "[[:space:]]+")[[1]]), 0)
    min(40, max(7, cells, header))
  }, numeric(1))
}

#' Traits shown in the main body of the report
#'
#' The reference trait and any yield trait. Every other trait goes to the
#' appendix, so a trial with sixty traits does not fill the report with
#' one long table.
#'
#' @param traits trait names, possibly with the moment in brackets
#' @param reference_trait the reference trait name, with or without brackets
#' @return the subset of traits to show
#' @examples
#' main_traits(c("Yield [Harvest]", "Taste [Harvest]", "Overall preference [Harvest]"),
#'             "Overall Preference")
main_traits = function(traits, reference_trait) {
  base = tolower(trimws(sub("\\s*\\[.*\\]\\s*$", "", traits)))
  ref = tolower(trimws(sub("\\s*\\[.*\\]\\s*$", "", reference_trait)))
  is_yield = grepl("yield|rendimiento|rendement|rendimento|mavuno|avling|produc", base)
  unique(traits[base == ref | is_yield])
}

#' Shorten lists of traits for table cells
#'
#' Drops the data collection moment in brackets when the trait names are
#' still unique without it, so "Yield [Harvest]; Taste [Harvest]" becomes
#' "Yield; Taste".
#' @param x a character vector of lists separated by "; "
short_trait_list = function(x) {
  items = unique(unlist(strsplit(x[!is.na(x)], ";\\s*")))
  items = items[nzchar(items)]
  short = sub("\\s*\\[.*\\]\\s*$", "", items)
  if (anyDuplicated(short) > 0) return(x)
  for (i in seq_along(items)) x = gsub(items[i], short[i], x, fixed = TRUE)
  x
}

#' Write paragraphs from an asis chunk
para = function(...) {
  txt = c(...)
  txt = txt[!is.na(txt) & nzchar(txt)]
  if (length(txt) > 0) cat(paste0(txt, "\n\n"), sep = "")
}

#' Write a heading from an asis chunk
heading = function(text, level = 1) {
  cat("\n", strrep("#", level), " ", text, "\n\n", sep = "")
}

#' Format a p-value for the text
format_p = function(p) {
  ifelse(is.na(p), "", ifelse(p < 0.001, "< 0.001", formatC(p, digits = 3, format = "fg")))
}
