# ................................................................
# Languages
#
# All the text of the reports lives in two CSV files in report/i18n/
#   report-text.csv     main report, tables and chart labels
#   feedback-text.csv   participant feedback sheet and certificate
# Each file has a column "key" and one column per language code.
# Translators only edit these files, see report/i18n/README.md.
#
# In the code and the templates text is written as tr(<key>, name = value).
# Placeholders in the text are written as {name}. A missing or empty
# translation falls back to English.
# ................................................................

climmob_languages = c("en", "es", "fr", "pt", "sw", "no")

i18n = new.env()

#' Turn the language sent by ClimMob into one of the supported codes
#' @examples normalize_language("pt-BR"); normalize_language("nb")
normalize_language = function(x, available = climmob_languages) {

  x = tolower(trimws(as.character(x %||% "en")))[1]

  if (is.na(x) || !nzchar(x)) {
    return("en")
  }

  x = sub("[-_].*$", "", x)

  aliases = c(nb = "no", nn = "no", nob = "no", nor = "no", spa = "es",
              por = "pt", fra = "fr", fre = "fr", swa = "sw", eng = "en")

  if (x %in% names(aliases)) {
    x = aliases[[x]]
  }

  if (x %in% available) x else "en"

}

#' Read one translation file into a named list of named character vectors
read_translations = function(file) {

  txt = utils::read.csv(file, stringsAsFactors = FALSE, encoding = "UTF-8",
                        fileEncoding = "UTF-8", na.strings = character(0),
                        check.names = FALSE)

  if (!"key" %in% names(txt)) {
    stop("Translation file ", basename(file), " has no 'key' column")
  }

  langs = setdiff(names(txt), "key")

  out = lapply(langs, function(l) stats::setNames(txt[[l]], txt$key))
  names(out) = langs

  out

}

#' Load the translations and set the language for this run
#'
#' @param language language code sent by ClimMob
#' @param code_path folder with the code, translations are in report/i18n
set_language = function(language = "en", code_path = getOption("climmob.code_path", ".")) {

  dir = file.path(code_path, "report", "i18n")

  files = list.files(dir, pattern = "[.]csv$", full.names = TRUE)

  tables = list()

  for (f in files) {
    t = read_translations(f)
    for (l in names(t)) {
      tables[[l]] = c(tables[[l]], t[[l]])
    }
  }

  i18n$tables = tables
  i18n$language = normalize_language(language, available = names(tables))

  invisible(i18n$language)

}

#' Current language code
current_language = function() {
  i18n$language %||% "en"
}

#' Translate a key and fill in its placeholders
#'
#' @param .key the key in the translation files
#' @param ... values for the placeholders, as name = value
#' @param .lang language code, the current language by default
#' @examples
#' tr("sum_items", n_items = 8, options = "varieties", participant = "farmer", n_per = 3)
tr = function(.key, ..., .lang = current_language()) {

  # .key starts with a dot so that placeholders such as k = 1 cannot
  # partially match it (R matches argument names before ... partially)
  key = .key

  tables = i18n$tables

  text = NA_character_

  if (!is.null(tables[[.lang]]) && key %in% names(tables[[.lang]])) {
    text = tables[[.lang]][[key]]
  }

  if (is.na(text) || !nzchar(text)) {
    text = if (!is.null(tables$en) && key %in% names(tables$en)) tables$en[[key]] else NA_character_
  }

  if (is.na(text)) {
    # make missing keys visible in the output instead of failing
    return(paste0("[", key, "]"))
  }

  fill_placeholders(text, list(...))

}

#' Replace {name} in a text with values
#' @examples fill_placeholders("{n} farmers", list(n = 3))
fill_placeholders = function(text, values) {

  for (nm in names(values)) {
    v = values[[nm]]
    v = if (length(v) == 0 || all(is.na(v))) "" else paste(v, collapse = ", ")
    text = gsub(paste0("{", nm, "}"), v, text, fixed = TRUE)
  }

  text

}

#' Join words with commas and the conjunction of the current language
#' @examples join_words(c("A", "B", "C"))
join_words = function(x, .lang = current_language()) {

  x = x[!is.na(x) & nzchar(x)]

  if (length(x) <= 1) {
    return(paste(x, collapse = ""))
  }

  paste0(paste(x[-length(x)], collapse = ", "), " ", tr("and", .lang = .lang), " ", x[length(x)])

}

#' Plural of the labels sent by ClimMob (variety, farmer...)
#'
#' Common labels have a fixed plural, otherwise a simple rule per language.
#' Swahili plurals change the noun prefix, so the label is kept as sent.
plural_label = function(x, .lang = current_language()) {

  x = as.character(x)[1]

  if (is.na(x) || !nzchar(x)) {
    return(x)
  }

  fixed = c(variety = "varieties", technology = "technologies", family = "families",
            variedad = "variedades", opcion = "opciones", "opci\u00f3n" = "opciones",
            "vari\u00e9t\u00e9" = "vari\u00e9t\u00e9s", variedade = "variedades",
            sort = "sorter", "agricultor" = "agricultores", "agriculteur" = "agriculteurs",
            bonde = "b\u00f8nder", mkulima = "wakulima", aina = "aina")

  low = tolower(x)

  if (low %in% names(fixed)) {
    return(fixed[[low]])
  }

  if (.lang == "sw") {
    return(x)
  }

  if (.lang == "no") {
    return(if (grepl("e$", x)) paste0(x, "r") else paste0(x, "er"))
  }

  if (.lang %in% c("es", "pt") && grepl("[^aeiou\u00e1\u00e9\u00ed\u00f3\u00fa]$", low)) {
    return(paste0(x, "es"))
  }

  if (grepl("s$|x$", low)) {
    return(x)
  }

  if (.lang == "en" && grepl("[^aeiou]y$", low)) {
    return(sub("y$", "ies", x))
  }

  paste0(x, "s")

}

#' Date written in the current language, for example 31 October 2026
format_date = function(date = Sys.Date(), .lang = current_language()) {

  date = as.Date(date)
  day = as.integer(format(date, "%d"))
  month = tr(paste0("month_", as.integer(format(date, "%m"))), .lang = .lang)
  year = format(date, "%Y")

  switch(.lang,
         es = paste(day, "de", month, "de", year),
         pt = paste(day, "de", month, "de", year),
         no = paste0(day, ". ", month, " ", year),
         paste(day, month, year))

}
