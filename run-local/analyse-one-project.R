# ................................................................
# Analyse one ClimMob project on your own computer
#
# Produces the same report and outputs as ClimMob, without going
# through the ClimMob server. Run it line by line from the root of
# this repository. See docs/combine-projects.md to combine projects.
# ................................................................
source("load.R")
load_climmob()

# 1. Fetch the data ----
# your API key is in your ClimMob profile, keep it out of shared scripts
key = Sys.getenv("CLIMMOB_KEY")
server = "1000FARMS"

projects = getProjectsCM(key, server = server)
projects[, c("project_code", "project_name", "user_owner", "server")]

p = projects[projects$project_code == "MYPROJECT", ]

cmlist = getDataCM(key, project = p$project_code, userowner = p$user_owner,
                   server = p$server, as.data.frame = FALSE)

cmdata = as.data.frame(cmlist, tidynames = FALSE, pivot.wider = TRUE)

# 2. Build the analysis parameters from the data ----
# On the server these come with the request. Here they are read from
# the questions of the project. Check them before running.
pars = build_pars_from_cmlist(cmlist)

pars$traits[, c("name", "assessmentName", "traitOrder")]   # ranking traits, one is the reference
pars$linear[, c("name", "assessmentName")]                 # measured traits
pars$covariates[, c("nameString", "name")]                 # covariates for the trees

# change the reference trait if needed
# pars$traits$traitOrder = "otherTraits"
# pars$traits$traitOrder[pars$traits$codeQst == "yield"] = "referenceTrait"

# keep only some covariates
# pars$covariates = pars$covariates[pars$covariates$codeQst %in% c("gender1", "age"), ]

# 3. Settings ----
settings = climmob_settings(
  project_name = cmlist$project$project_name,
  country = cmlist$project$project_cnty,          # ISO2 code, used for the map
  output_path = file.path("run-local", "output", p$project_code),
  language = "en",                                # en, es, fr, pt, sw or no
  extension = "docx",
  ranker = "farmer",
  option = "variety",
  code_path = getwd(),
  groups = "gender1",                             # column(s) to split by, or character(0)
  reference_names = cmlist$combination$elements[[1]]$alias_name,  # the check item(s)
  infosheets = TRUE,                              # feedback sheets for participants
  feedback_recommendations = "",                  # optional text for the feedback sheets
  feedback_contact = "")                          # optional contact for the feedback sheets

# 4. Run and inspect ----
res = run_climmob_analysis(cmdata, pars, settings, cmdatajson = cmlist)
print_run_summary(res$log)

# res$monitoring$attrition
# res$release$summary
# res$group_analysis$eligibility
# res$metric$traits[[1]]$means

# 5. Write the report, feedback and extra outputs ----
write_climmob_outputs(res, settings)
