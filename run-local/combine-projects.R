# ................................................................
# Combine several ClimMob projects into one analysis
#
# Projects designed with different templates name their questions and
# data collection moments differently. This script
#   1. fetches the projects
#   2. writes a harmonisation table you review in a spreadsheet
#   3. combines the projects with that table and runs the analysis
# Run it line by line from the root of this repository.
# See docs/combine-projects.md for the details of each step.
# ................................................................
source("load.R")
load_climmob()

key = Sys.getenv("CLIMMOB_KEY")
server = "1000FARMS"
codes = c("PROJECT1", "PROJECT2")
out = file.path("run-local", "output", "combined")
dir.create(out, recursive = TRUE, showWarnings = FALSE)

# 1. Fetch ----
projects = getProjectsCM(key, server = server)
projects = projects[projects$project_code %in% codes, ]

cmlists = lapply(seq_len(nrow(projects)), function(i) {
  getDataCM(key, project = projects$project_code[i], userowner = projects$user_owner[i],
            server = projects$server[i], as.data.frame = FALSE)
})
names(cmlists) = projects$project_code

cmdata = lapply(cmlists, as.data.frame, tidynames = FALSE, pivot.wider = TRUE)

# 2. Harmonise ----
# one row per question and project, with a proposed common code (trait),
# common data collection moment (moment) and label
h = suggest_harmonization(lapply(cmlists, project_catalog))
write.csv(h, file.path(out, "harmonization.csv"), row.names = FALSE)

# Open harmonization.csv and check, for every row
#   trait         same code for the same question in every project
#   moment        same code for the same data collection moment
#   moment_label  name of the moment in the report
#   label         name of the trait in the report
#   include       TRUE to use the row, FALSE to leave it out
# Then read it back
h = read.csv(file.path(out, "harmonization.csv"), stringsAsFactors = FALSE)

# optional, give the same name to the same item across projects
item_map = NULL
# item_map = data.frame(project = "PROJECT2", original = "Var 1", item = "Variety 1")

# 3. Combine and analyse ----
combined = combine_projects(cmdata, h, item_map = item_map)
pars = pars_from_harmonization(h)

settings = climmob_settings(
  project_name = paste(codes, collapse = " + "),
  country = cmlists[[1]]$project$project_cnty,
  output_path = out,
  language = "en",
  ranker = "farmer",
  option = "variety",
  code_path = getwd(),
  groups = c("project_code"),
  reference_names = "Local check",
  infosheets = FALSE)

res = run_climmob_analysis(combined, pars, settings)
print_run_summary(res$log)
write_climmob_outputs(res, settings)
