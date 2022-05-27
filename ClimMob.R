# # ................................................................
# # ................................................................
# # Main script to call for the analysis and rendering ClimMob reports
# # ................................................................
# # ................................................................
# # ................................................................
# # ................................................................
# Arguments ####
# get the arguments from server's call
args <- commandArgs(trailingOnly = TRUE)
infoname    <- args[1] # a json file with parameters for the analysis
outputname  <- args[2] # a json file with the results
outputpath  <- args[3] # the path where results will be written
infosheets  <- as.logical(args[4]) # logical, if infosheets should be written TRUE FALSE
language    <- args[5] # the language to write the report "en" for english and "es" for spanish
extension   <- args[6] # report file format it can be "docx", "pdf", and "html"
ranker      <- args[7] # how the report will refer to participants/farmers
option      <- args[8] # how the report will refer to tested technologies
fullpath    <- args[9] # this is backward path
groups      <- args[10] # any group to do the analysis 
reference   <- as.integer(args[11]) # the reference item for the analysis
minN        <- args[12] # minimum n of complete data required in a trait evaluation before it is excluded
minitem     <- args[13] # minimum n of items tested, e.g. that all items are tested at least twice
mincovar    <- args[14] # minimum proportion of covariates compared to total valid n
sig_level   <- args[15] # significance level for the standard PL model
sig_level_tree  <-  as.numeric(args[16]) # significance level for the tree
minsplit    <- as.integer(args[17]) # minimum n in each tree node
template    <- args[18] 

if (isTRUE(is.na(reference))) {
  reference <- 1
}

if (isTRUE(is.na(minN))) {
  minN <- 5
}

if (isTRUE(is.na(minitem))) {
  minitem <- 2
}

if (isTRUE(is.na(mincovar))) {
  mincovar <- 0.95
}

if (isTRUE(is.na(sig_level))) {
  sig_level <- 0.1
}

if (isTRUE(is.na(sig_level_tree))) {
  sig_level_tree <- sig_level
}

if (isTRUE(is.na(minsplit))) {
  minsplit <- 10
}

if (isTRUE(is.na(language))) {
  language <- "en"
}

# # ................................................................
# # ................................................................
## Packages ####
library("ClimMobTools")
library("gosset")
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
library("ggrepel")
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
library("nasapower")
library("climatrends")

# .........................................
# Load modules
modules <- list.files(paste0(fullpath, "modules"), full.names = TRUE)
modules <- modules[-which(grepl("check_packages.R", modules))]
for (i in seq_along(modules)) {
  source(modules[i])
}

# An object to capture error messages when running the analysis
error <- NULL

# ................................................................
# ................................................................
# 1. Read data and organize the rankings #### 
try_data <- tryCatch({
  
  # call pars sent by ClimMob
  pars <- jsonlite::fromJSON(infoname)
  pars <- decode_pars(pars)
  
  # the trial data 
  cmdata <- jsonlite::fromJSON(outputname)
  class(cmdata) <- union("CM_list", class(cmdata))
  project_name <- cmdata$project$project_name
  reference <- cmdata$combination$elements[[reference]]$alias_name
  cmdata <- try(as.data.frame(x = cmdata, tidynames = FALSE, pivot.wider = TRUE),
                silent = TRUE)
  
  dir.create(outputpath, showWarnings = FALSE, recursive = TRUE)
  
  rank_dat <- organize_ranking_data(cmdata, 
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
  e <- paste("Organize Ranking Data: \n", try_data$message)
  error <- c(error, e)
  rank_dat <- error_data_rank_dat
}

# ................................................................
# ................................................................
# 2. Organise the rankings ####
try_quanti_data <- tryCatch({
  
  if (isTRUE(length(pars[["linear"]]) > 0)) {
    
    quanti_dat <- organize_quantitative_data(cmdata, 
                                             pars, 
                                             groups = groups, 
                                             tech_index = c("package_item_A", 
                                                            "package_item_B", 
                                                            "package_item_C"))
  }else{
    
    quanti_dat <- error_data_quanti_dat
    
  }
  
  
}, error = function(cond) {
  return(cond)
}
)

if (any_error(try_quanti_data)) {
  e <- paste("Organize Quantitative Data: \n", try_quanti_data$message)
  error <- c(error, e)
  quanti_dat <- error_data_quanti_dat
}

# .......................................................
# .......................................................
# 3. Prepare summary tables / charts
org_summ <- tryCatch({
  
  overview_and_summaries <- get_overview_summaries(cmdata, rank_dat)
  
}, error = function(cond) {
  return(cond)
}
)

if (any_error(org_summ)) {
  e <- paste("Overview and Summary: \n", org_summ$message)
  error <- c(error, e)
  overview_and_summaries <- error_data_overview_and_summaries
}

# .......................................................
# .......................................................
# 4. Make map ####
org_lonlat <- tryCatch({
  
  trial_map <- get_testing_sites_map(cmdata, output_path = outputpath)
  
}, error = function(cond) {
  return(cond)
}
)

if (any_error(org_lonlat)) {
  e <- paste("Trial map: \n", org_lonlat$message)
  error <- c(error, e)
  trial_map <- error_data_trial_map

}

# .......................................................
# .......................................................
# 9. Agroclimatic information  ####
org_agroclim <- tryCatch({
  
  agroclimate <- get_agroclimatic_data(cmdata)
  
}, error = function(cond) {
  return(cond)
}
)

if (any_error(org_agroclim)) {
  e <- paste("Agroclimatic data: \n", org_agroclim$message)
  error <- c(error, e)
  agroclimate <- error_data_agroclimate
}

# .......................................................
# .......................................................
# .......................................................
# 7. Fit PlackettLuce model ####
org_pl <- tryCatch({
  
  PL_models <- get_PlackettLuce_models(cmdata, rank_dat)
  
}, error = function(cond) {
  return(cond)
}
)

if (any_error(org_pl)) {
  e <- paste("Plackett-Luce model: \n", org_pl$message)
  error <- c(error, e)
  PL_models <- error_data_PL_model
}

# .......................................................
# .......................................................
# 7. Fit PLADMM model ####
# TO DO! 


# .......................................................
# .......................................................
# 8. Fit pltree ####
org_pltree <- tryCatch({
  
  PL_tree <- get_PlackettLuce_tree(cmdata, rank_dat, agroclimate)
  
}, error = function(cond) {
  return(cond)
}
)

if (any_error(org_pltree)) {
  e <- paste("Plackett-Luce Tree: \n", org_pltree$message)
  error <- c(error, e)
  PL_tree <- error_data_PL_tree
}


# .......................................................
# .......................................................
# 10. Summaries from quantitative data  ####
org_quantitative_summ <- tryCatch({
  
  quantitative_traits <- get_quantitative_summaries(quanti_dat)
  
}, error = function(cond) {
  return(cond)
}
)

if (any_error(org_quantitative_summ)) {
  e <- paste("Quantitative Traits: \n", org_quantitative_summ$message)
  error <- c(error, e)
  quantitative_traits <- error_data_quantitative_traits
}


# ................................................................
# ................................................................
# 9. Write outputs ####
#determine format based on extensions
output_format <- ifelse(extension == "docx","word_document", 
                        paste0(extension,"_document"))


# arguments to use in the report text
project_name <- rank_dat$projname
noptions <- length(rank_dat$technologies_index)
ntechnologies <- length(rank_dat$technologies)
option <- rank_dat$option
options <- pluralize(rank_dat$option)
participant <- rank_dat$ranker
participants <- pluralize(rank_dat$ranker)
nparticipants <- nrow(cmdata)
ntraits <- length(rank_dat$trait_list)
reference_trait <- title_case(rank_dat$reference_trait)
reference_tech <- rank_dat$reference_tech

# resolution of display items
dpi <- 400
out_width <- "100%"

# define height of plots based on number of technologies
worthmap_h <- ntraits
worthmap_w <- ntraits + 0.5
favplot_h <- ntechnologies * 0.4

if (worthmap_h < 7)  worthmap_h <- 7
if (worthmap_h > 8)  worthmap_h <- 8
if (worthmap_w < 7)  worthmap_w <- worthmap_h + 0.5
if (worthmap_w > 8)  worthmap_w <- worthmap_h + 0.5
if (favplot_h < 5) favplot_h <- 5
if (favplot_h > 8) favplot_h <- 7.5

# the main report
rmarkdown::render(paste0(fullpath, "/report/mainreport.Rmd"),
                  output_dir = outputpath,
                  output_format = output_format,
                  output_file = paste0("climmob_main_report", ".", extension))

if (length(error) > 0) {
  print(error)
}

# End of analysis


