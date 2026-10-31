# ................................................................
# INNOFORAGE, Norwegian forage mixture trials (SS25 and NS25)
#
# Combines the two 2025 projects, writes the report in Norwegian, the
# feedback sheets for participants, the community results for those
# who did not send their answers, and the AI-ready tables used to draft
# letters and flyers with an AI assistant.
# Run from the root of the repository with CLIMMOB_KEY set.
# ................................................................
source("load.R")
load_climmob()

key = Sys.getenv("CLIMMOB_KEY")
codes = c("SS25", "NS25")
out = file.path("run-local", "output", "INNOFORAGE-2025")
dir.create(out, recursive = TRUE, showWarnings = FALSE)

projects = getProjectsCM(key, server = "1000farms")
projects = projects[projects$project_code %in% codes, ]

cmlists = lapply(seq_len(nrow(projects)), function(i) {
  getDataCM(key, project = projects$project_code[i], userowner = projects$user_owner[i],
            server = projects$server[i], as.data.frame = FALSE)
})
names(cmlists) = projects$project_code
cmdata = lapply(cmlists, as.data.frame, tidynames = FALSE, pivot.wider = TRUE)

# harmonise, review the file the first time and save the reviewed copy
hfile = file.path(out, "harmonization.csv")
if (!file.exists(hfile)) {
  write.csv(suggest_harmonization(lapply(cmlists, project_catalog)), hfile, row.names = FALSE)
  stop("Review ", hfile, " and run the script again")
}
h = read.csv(hfile, stringsAsFactors = FALSE)

combined = combine_projects(cmdata, h)
pars = pars_from_harmonization(h)

settings = climmob_settings(
  project_name = "INNOFORAGE 2025",
  country = "NO",
  output_path = out,
  language = "no",
  ranker = "bonde",
  option = "blanding",
  code_path = getwd(),
  groups = "project_code",
  reference_names = "Blanding 1",
  infosheets = TRUE,
  feedback_contact = "",
  feedback_recommendations = "")

res = run_climmob_analysis(combined, pars, settings)
print_run_summary(res$log)
write_climmob_outputs(res, settings)

# Files to look at
#   climmob_main_report.docx                      report in Norwegian
#   participant-report/feedback-sheets.pdf        one page per farmer
#   participant-report/community-results.pdf/png  flyer for those who did not answer
#   extra-outputs/ai-ready/                        tables and guide for an AI assistant
