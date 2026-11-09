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

kable_tr = function(x, keys = NULL, caption = NULL, digits = 3) {

  if (!is.null(keys)) {
    lab = vapply(seq_along(keys), function(i) {
      if (is.na(keys[i])) names(x)[i] else tr(keys[i])
    }, character(1))
    names(x)[seq_along(lab)] = lab
  }

  knitr::kable(x, caption = caption, row.names = FALSE, align = "l", digits = digits)

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
