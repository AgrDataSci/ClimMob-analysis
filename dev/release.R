# ................................................................
# Prepare a release
#
#   Rscript dev/release.R 4.0.0
#
# 1. checks that NEWS.md starts with "ClimMob-analysis v4.0.0 (<date>)"
# 2. sets the version and date in CITATION.cff
# 3. runs the tests
# 4. prints the git commands to commit, tag and push
# Pushing the tag starts .github/workflows/release.yml, which publishes
# the GitHub release with the notes from NEWS.md. Zenodo archives it.
# ................................................................
args = commandArgs(trailingOnly = TRUE)

if (length(args) != 1 || !grepl("^[0-9]+[.][0-9]+[.][0-9]+$", args[1])) {
  stop("Usage: Rscript dev/release.R <major.minor.patch>, for example 4.0.0")
}

version = args[1]
tag = paste0("v", version)
today = format(Sys.Date(), "%Y-%m-%d")

# 1. NEWS.md
news = readLines("NEWS.md", encoding = "UTF-8")
first = news[nzchar(trimws(news))][1]
expected = paste0("ClimMob-analysis ", tag, " (")

if (!startsWith(first, expected)) {
  stop("NEWS.md must start with a section for this version, for example\n",
       "ClimMob-analysis ", tag, " (", today, ")\n",
       "=========================\n",
       "First line now: ", first)
}

# 2. CITATION.cff
cff = readLines("CITATION.cff", encoding = "UTF-8")
cff = sub("^version: .*$", paste0("version: \"", tag, "\""), cff)
cff = sub("^date-released: .*$", paste0("date-released: ", today), cff)
writeLines(cff, "CITATION.cff", useBytes = TRUE)
message("CITATION.cff set to ", tag, ", ", today)

# 3. tests
status = system2("Rscript", "tests/testthat.R")
if (status != 0) {
  stop("Tests failed, fix them before releasing")
}

# 4. git commands
message("\nNow run\n",
        "  git add NEWS.md CITATION.cff\n",
        "  git commit -m \"Release ", tag, "\"\n",
        "  git tag -a ", tag, " -m \"ClimMob-analysis ", tag, "\"\n",
        "  git push && git push origin ", tag, "\n",
        "Then deploy the tag on the ClimMob server.")
