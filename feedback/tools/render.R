# Render the feedback templates from feedback.json with R
#
#   Rscript tools/render.R <feedback.json> <output folder>
#
# feedback.json is written by tools/build_feedback.py, or by hand following
# the data contract in README.md. This script only fills the Mustache
# templates (package whisker) and prints them to PDF with Chrome
# (package pagedown, which needs Chrome or Chromium installed).
# Install once:  install.packages(c("jsonlite", "whisker", "pagedown"))
#
# Not yet run on a machine with R. Report problems in the repository issues.

args = commandArgs(trailingOnly = TRUE)
if (length(args) < 2) stop("usage: Rscript tools/render.R <feedback.json> <output folder>")
json_path = args[1]
out_dir = args[2]
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

here = normalizePath(dirname(sub("--file=", "", grep("--file=", commandArgs(), value = TRUE)[1])))
tpl_dir = file.path(dirname(here), "templates")

data = jsonlite::fromJSON(json_path, simplifyVector = FALSE)

read = function(f) paste(readLines(file.path(tpl_dir, f), encoding = "UTF-8", warn = FALSE), collapse = "\n")

css = read("theme.css")
head = whisker::whisker.render(read("_head.html"), data)
head = sub('<link rel="stylesheet" href="theme.css">', paste0("<style>", css, "</style>"), head, fixed = TRUE)
foot = read("_foot.html")

tpl = list(sheet = read("participant-sheet.html"), items = read("items-page.html"),
           flyer = read("community-flyer.html"), cert = read("certificate.html"),
           manual = read("workshop-manual.html"))

base = list(trial = data$trial, t = data$t)

per_participant = function(template, with_items = FALSE) {
  paste(vapply(data$participants, function(p) {
    d = c(base, p)
    out = whisker::whisker.render(template, d)
    if (with_items && !is.null(p$items_table)) {
      out = paste0(out, whisker::whisker.render(tpl$items, d))
    }
    out
  }, character(1)), collapse = "\n")
}

docs = list(
  "participant-sheets" = per_participant(tpl$sheet, with_items = TRUE),
  "certificates" = per_participant(tpl$cert),
  "community-flyer" = whisker::whisker.render(tpl$flyer, c(base, list(flyer = data$flyer))),
  "workshop-manual" = whisker::whisker.render(tpl$manual, c(base, list(workshop = data$workshop)))
)

for (name in names(docs)) {
  html = file.path(out_dir, paste0(name, ".html"))
  writeLines(paste0(head, docs[[name]], foot), html, useBytes = TRUE)
  pdf = file.path(out_dir, paste0(name, ".pdf"))
  size = if (name == "certificates") c(210, 148) else c(210, 297)
  ok = tryCatch({
    pagedown::chrome_print(html, output = pdf, verbose = 0,
                           options = list(paperWidth = size[1] / 25.4, paperHeight = size[2] / 25.4,
                                          marginTop = 0, marginBottom = 0, marginLeft = 0, marginRight = 0,
                                          printBackground = TRUE))
    TRUE
  }, error = function(e) { message("PDF failed for ", name, ": ", conditionMessage(e)); FALSE })
  cat(name, if (ok) "-> PDF" else "-> HTML only", "\n")
}

# one PDF per participant, for sending by email
if (requireNamespace("qpdf", quietly = TRUE) && file.exists(file.path(out_dir, "participant-sheets.pdf"))) {
  dir.create(file.path(out_dir, "per-participant"), showWarnings = FALSE)
  n_pages = qpdf::pdf_length(file.path(out_dir, "participant-sheets.pdf"))
  pages_each = n_pages / length(data$participants)
  for (i in seq_along(data$participants)) {
    p = data$participants[[i]]
    from = (i - 1) * pages_each + 1
    qpdf::pdf_subset(file.path(out_dir, "participant-sheets.pdf"), pages = from:(from + pages_each - 1),
                     output = file.path(out_dir, "per-participant", paste0("sheet-", p$package_id, ".pdf")))
  }
}
