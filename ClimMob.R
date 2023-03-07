# # ................................................................
# # ................................................................
# # Main script to call for the analysis and rendering ClimMob reports
# # ................................................................
# # ................................................................
# # ................................................................
# # ................................................................
# get the arguments from server's call
args = commandArgs(trailingOnly = TRUE)

# Arguments ####
cmparameters = args[1] # a json file with parameters for the analysis
cmdatajson = args[2] # a json file with the results

# get parameters from ClimMob request
pars = jsonlite::fromJSON(cmparameters)
report_parameters = pars[["Parameters"]]
outputpath  = report_parameters[["OutputPath"]] # the path where results will be written
infosheets  = as.logical(report_parameters[["InfoSheets"]]) # logical, if infosheets should be written TRUE FALSE
language    = report_parameters[["Languaje"]] # the language to write the report "en" for english and "es" for spanish
extension   = report_parameters[["Format"]] # report file format it can be "docx", "pdf", and "html"
ranker      = report_parameters[["ReferToParticipants"]] # how the report will refer to participants/farmers
option      = report_parameters[["ReferToTechnologies"]] # how the report will refer to tested technologies
fullpath    = report_parameters[["BackwardPath"]] # this is backward path
groups      = report_parameters[["Split"]] # any group to do the analysis 
reference   = report_parameters[["Reference"]] # the reference item for the analysis
minN        = 5 # minimum n of complete data required in a trait evaluation before it is excluded
minitem     = 2 # minimum n of items tested, e.g. that all items are tested at least twice
mincovar    = 0.95 # minimum proportion of covariates compared to total valid n
sig_level   = 0.1 # significance level for the Plackett-Luce model

# break the groups into a vector, if more than one
groups = as.vector(strsplit(groups, ",")[[1]])

# break the reference into a vector, if more than one
reference = as.integer(strsplit(reference, ",")[[1]])

if (length(reference) > 4) {
  reference = reference[1:4]
}

# # ................................................................
# # ................................................................
## Packages ####
library("ClimMobTools")
library("gosset")
library("nasapower")
library("climatrends")
library("PlackettLuce")
library("partykit")
library("qvcalc")
library("psychotools")
library("jsonlite")
library("knitr")
library("rmarkdown")
library("pls")
library("gtools")
library("ggplot2")
library("igraph")
library("ggparty")
library("patchwork")
library("leaflet")
library("mapview")
library("multcompView")
library("png")
library("plotrix")
library("gridExtra")
library("caret")
library("janitor")

# ................................................................
# ................................................................
# Load modules ####
modules = list.files(paste0(fullpath, "/modules"), full.names = TRUE)
modules = modules[-which(grepl("check_packages.R", modules))]
for (i in seq_along(modules)) {
  source(modules[i])
}

# An object to capture error messages when running the analysis
error = NULL

# ................................................................
# ................................................................
# 1. Read data and organize the rankings #### 
try_data = tryCatch({
  
  # report parameters
  pars = jsonlite::fromJSON(cmparameters)
  pars = decode_pars(pars)
  
  # the trial data 
  cmdata = jsonlite::fromJSON(cmdatajson)
  class(cmdata) = union("CM_list", class(cmdata))
  
  # project data as an independent object
  project_data = cmdata$project
  project_name = project_data$project_name
  
  # get the name of the check variety
  reference = cmdata$combination$elements[reference]
  reference = unlist(lapply(reference, function(x) x$alias_name))
  
  # trial data as data.frame
  cmdata = try(as.data.frame(x = cmdata, 
                             tidynames = FALSE, 
                             pivot.wider = TRUE),
                silent = TRUE)
  
  dir.create(outputpath, showWarnings = FALSE, recursive = TRUE)
  
  rank_dat = organize_ranking_data(cmdata, 
                                    pars, 
                                    project_name,
                                    groups, 
                                    option_label = option,
                                    ranker_label = ranker,
                                    reference_tech = reference,
                                    tech_index = c("package_item_A", 
                                                   "package_item_B",
                                                   "package_item_C"))
  
}, error = function(cond) {
    return(cond)
  }
)

if (any_error(try_data)) {
  e = paste("Organize Ranking Data: \n", try_data$message)
  error = c(error, e)
  rank_dat = error_data_rank_dat
}

# ................................................................
# ................................................................
# 2. Organise quantitative data ####
try_quanti_data = tryCatch({
  
  if (isTRUE(length(pars[["linear"]]) > 0)) {
    
    quanti_dat = organize_quantitative_data(cmdata, 
                                             pars, 
                                             groups = groups, 
                                             id = "id",
                                             tech_index = c("package_item_A", 
                                                            "package_item_B", 
                                                            "package_item_C"))
  }else{
    
    quanti_dat = error_data_quanti_dat
    
  }
  
  
}, error = function(cond) {
  return(cond)
}
)

if (any_error(try_quanti_data)) {
  e = paste("Organize Quantitative Data: \n", try_quanti_data$message)
  error = c(error, e)
  quanti_dat = error_data_quanti_dat
}

# .......................................................
# .......................................................
# 3. Prepare summary tables / charts
org_summ = tryCatch({
  
  overview_and_summaries = get_overview_summaries(cmdata, rank_dat)
  
}, error = function(cond) {
  return(cond)
}
)

if (any_error(org_summ)) {
  e = paste("Overview and Summary: \n", org_summ$message)
  error = c(error, e)
  overview_and_summaries = error_data_overview_and_summaries
}

# .......................................................
# .......................................................
# 4. Make map ####
org_lonlat = tryCatch({
  
  trial_map = get_testing_sites_map(cmdata, 
                                    output_path = outputpath,
                                    backward_path = fullpath)
  
}, error = function(cond) {
  return(cond)
}
)

if (any_error(org_lonlat)) {
  e = paste("Trial map: \n", org_lonlat$message)
  error = c(error, e)
  trial_map = error_data_trial_map

}

# .......................................................
# .......................................................
# 9. Agroclimatic information  ####
org_agroclim = tryCatch({
  
  agroclimate = get_agroclimatic_data(cmdata)
  
}, error = function(cond) {
  return(cond)
}
)

if (any_error(org_agroclim)) {
  e = paste("Agroclimatic data: \n", org_agroclim$message)
  error = c(error, e)
  agroclimate = error_data_agroclimate
}

# .......................................................
# .......................................................
# .......................................................
# 7. Fit PlackettLuce model ####
org_pl = tryCatch({
  
  PL_models = get_PlackettLuce_models(cmdata, rank_dat)
  
}, error = function(cond) {
  return(cond)
}
)

if (any_error(org_pl)) {
  e = paste("Plackett-Luce model: \n", org_pl$message)
  error = c(error, e)
  PL_models = error_data_PL_model
}

# .......................................................
# .......................................................
# 7. Fit PLADMM model ####
# TO DO! 


# .......................................................
# .......................................................
# 8. Fit pltree ####
org_pltree = tryCatch({
  
  PL_tree = get_PlackettLuce_tree(cmdata, rank_dat, agroclimate)
  
}, error = function(cond) {
  return(cond)
}
)

if (any_error(org_pltree)) {
  e = paste("Plackett-Luce Tree: \n", org_pltree$message)
  error = c(error, e)
  PL_tree = error_data_PL_tree
}


# .......................................................
# .......................................................
# 10. Summaries from quantitative data  ####
org_quantitative_summ = tryCatch({
  
  quantitative_traits = get_quantitative_summaries(quanti_dat)
  
}, error = function(cond) {
  return(cond)
}
)

if (any_error(org_quantitative_summ)) {
  e = paste("Quantitative Traits: \n", org_quantitative_summ$message)
  error = c(error, e)
  quantitative_traits = error_data_quantitative_traits
}

# ................................................................
# ................................................................
# 9. Write outputs ####
#determine format based on extensions
output_format = ifelse(extension == "docx","word_document", 
                        paste0(extension,"_document"))


# arguments to use in the report text
project_name = rank_dat$projname
noptions = length(rank_dat$technologies_index)
ntechnologies = length(rank_dat$technologies)
option = rank_dat$option
options = pluralize(rank_dat$option)
participant = rank_dat$ranker
participants = pluralize(rank_dat$ranker)
nparticipants = nrow(cmdata)
ntraits = length(rank_dat$trait_list)
reference_trait = title_case(rank_dat$reference_trait)
reference_tech = rank_dat$reference_tech

# resolution of display items
dpi = 400
out_width = "100%"

# define height of plots based on number of technologies
worthmap_h = ntraits
worthmap_w = ntraits + 0.5
favplot_h = ntechnologies * 0.4

if (worthmap_h < 7)  worthmap_h = 7
if (worthmap_h > 8)  worthmap_h = 8
if (worthmap_w < 7)  worthmap_w = worthmap_h + 0.5
if (worthmap_w > 8)  worthmap_w = worthmap_h + 0.5
if (favplot_h < 5) favplot_h = 5
if (favplot_h > 8) favplot_h = 7.5

# the main report
rmarkdown::render(paste0(fullpath, "/report/mainreport.Rmd"),
                  output_dir = outputpath,
                  output_format = output_format,
                  output_file = paste0("climmob_main_report", ".", extension))


if (isTRUE(infosheets)) {
  
  # .......................................................
  # .......................................................
  # 12. Participant report  ####
  org_participant_report = tryCatch({
    
    participant_report = get_participant_report(cmdata, 
                                                rank_dat, 
                                                fullpath, 
                                                language = language)
    
  }, error = function(cond) {
    return(cond)
  }
  )
  
  if (any_error(org_participant_report)) {
    e = paste("Participant report: \n", org_participant_report$message)
    error = c(error, e)
    participant_report = error_participant_report
  }
  
  participant_report_dir = paste0(outputpath, "/participant-report/")
  
  dir.create(participant_report_dir, recursive = TRUE, showWarnings = FALSE)
  
  for (i in seq_along(participant_report$partitable$id)) {
    
    rmarkdown::render(paste0(fullpath, "/report/participant_report_main.Rmd"),
                      output_dir = participant_report_dir,
                      output_format = output_format,
                      output_file = paste0("participant_report_package_", i, 
                                           ".", extension))
    
  }
  
}




if (length(error) > 0) {
  print(error)
}


# Now write the extra charts and tables
chartdir = paste0(outputpath, "/extra-outputs/")
dir.create(chartdir, recursive = TRUE, showWarnings = FALSE)

# log worth plot by trait
for(m in seq_along(PL_models$logworth_plot)){
  try(ggsave(paste0(chartdir, rank_dat$trait_code[m], "_logworth.png"),
         plot = PL_models$logworth_plot[[m]],
         width = 21,
         height = 15,
         units = "cm",
         dpi = 200), silent = TRUE)
}

# plot kendall tau plot
try(ggsave(paste0(chartdir, "kendall_tau.png"),
           plot = PL_models$kendall$kendall_plot,
           width = 15,
           height = 18,
           units = "cm",
           dpi = 200), silent = TRUE)

# plot worth map
try(ggsave(paste0(chartdir, "worth_map.png"),
           plot = PL_models$worthmap,
           width = 25,
           height = 25,
           units = "cm",
           dpi = 200), silent = TRUE)

# plot worth map
try(ggsave(paste0(chartdir, "reliability.png"),
           plot = PL_models$reliability_plot,
           width = 25,
           height = 25,
           units = "cm",
           dpi = 200), silent = TRUE)

try(write.csv(PL_models$reliability_data, paste0(chartdir, "reliability_data.csv"),
           row.names = FALSE), silent = TRUE)

if(length(unique(rank_dat$group)) > 1) {
  g = unique(rank_dat$group)
  # log worth plot by group
  for(m in seq_along(PL_models$logworth_plot_groups)){
    try(ggsave(paste0(chartdir, "Group", m, "_", g[m], "_logworth_grouped_rank.png"),
           plot = PL_models$logworth_plot_groups[[m]],
           width = 21,
           height = 15,
           units = "cm",
           dpi = 200), silent = TRUE)
  }
}

if(PL_tree$isTREE){
  try(ggsave(paste0(chartdir, "PlackettLuce.png"),
             plot = PL_tree$PLtree_plot,
             width = 18,
             height = 25,
             units = "cm",
             dpi = 200), silent = TRUE)
}

# agroclimate data during the "season"
if (isTRUE(agroclimate$agroclimate)) {
   
  write.csv(agroclimate$rainfall_season,
            file = paste0(chartdir, "weekly_precipitation_indices.csv"),
            row.names = FALSE)
  
  write.csv(agroclimate$temperature_season,
            file = paste0(chartdir, "weekly_temperature_indices.csv"),
            row.names = FALSE)
  
  try(ggsave(paste0(chartdir, "weekly_precipitation_indices.png"),
             plot = agroclimate$rain_plot,
             width = 20,
             height = 20,
             units = "cm",
             dpi = 200), silent = TRUE)
  
  try(ggsave(paste0(chartdir, "weekly_temperature_indices.png"),
             plot = agroclimate$temperature_plot,
             width = 20,
             height = 20,
             units = "cm",
             dpi = 200), silent = TRUE)
  
}

# write outputs of quantitative data if any 
if (isTRUE(quanti_dat$quantitative)) {
  
  write.csv(quanti_dat$outliers,
            file = paste0(chartdir, 
                          "possible_outliers_in_quantitative_data.csv"),
            row.names = FALSE)
  
}

# End of analysis


