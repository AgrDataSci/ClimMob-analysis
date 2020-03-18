# # ................................................................
# # ................................................................
# # Main script to call for the analysis and rendering the reports 
# # in ClimMob v3
# # ................................................................
# # ................................................................

# tag <- "techapp"
# args <- c(paste0("dev/data/",tag,"/data.json"), paste0("dev/data/",tag,"/info.json"),
#          paste0("dev/output/",tag,"/"), "TRUE","en","docx",
#          "participant","item", getwd())

# get the arguments from server's call
args <- commandArgs(trailingOnly = TRUE)
infoname    <- args[1] # a json file with parameters for the analysis
outputname  <- args[2] # a json file with the results
pathname    <- args[3] # the path where results will be written
infosheets  <- as.logical(args[4]) # logical, if infosheets should be written TRUE FALSE
language    <- args[5] # the language to write the report "en" for english and "es" for spanish
extension   <- args[6] # report file format it can be "docx", "pdf", and "html"
ranker      <- args[7] # how the system will refer to participants/farmers
option      <- args[8] # how the system will refer to tested items
fullpath    <- args[9] # this is backward path

# ................................................................
# ................................................................
## Packages ####
source(paste0(fullpath, "/R/functions.R"))

if (!require("devtools")){
  install.packages("devtools")
  library("devtools")
}else{
  library("devtools")
}

if (!require("ClimMobTools")) {
  devtools::install_github("agrobioinfoservices/ClimMobTools",
                           upgrade = "never")
  library("ClimMobTools")
}else{
  # check if library has the latest pkg version
  latest <- .latest_version("ClimMobTools",
                            "https://raw.githubusercontent.com/agrobioinfoservices/ClimMobTools/master/DESCRIPTION")
  
  if (isTRUE(latest)) {
    
    library("ClimMobTools")
  
    }else{
    
    devtools::install_github("agrobioinfoservices/ClimMobTools",
                             upgrade = "never")
    library("ClimMobTools")
  
  }
}

if (!require("gosset")) {
  devtools::install_github("agrobioinfoservices/gosset",
                           upgrade = "never")
  library("gosset")
}else{
  # check if library has the latest pkg version
  latest <- .latest_version("gosset",
                            "https://raw.githubusercontent.com/agrobioinfoservices/gosset/master/DESCRIPTION")
  
  if (isTRUE(latest)) {
    
    library("gosset")
    
  }else{
    
    devtools::install_github("agrobioinfoservices/gosset",
                             upgrade = "never")
    library("gosset")
    
  }
}

if (!require("PlackettLuce")) {
  install.packages("PlackettLuce")
  library("PlackettLuce")
}else{
  library("PlackettLuce")
}

if (!require("partykit")) {
  install.packages("partykit")
  library("partykit")
}else{
  library("partykit")
}

if (!require("qvcalc")) {
  install.packages("qvcalc")
  library("qvcalc")
}else{
  library("qvcalc")
}

if (!require("psychotools")) {
  install.packages("psychotools")
  library("psychotools")
}else{
  library("psychotools")
}

if (!require("jsonlite")) {
  install.packages("jsonlite")
  library("jsonlite")
}else{
  library("jsonlite")
}

if (!require("multcompView")) {
  install.packages("multcompView")
  library("multcompView")
}else{
  library("multcompView")
}

if (!require("knitr")) {
  install.packages("knitr")
  library("knitr")
}else{
  library("knitr")
}

if (!require("rmarkdown")) {
  install.packages("rmarkdown")
  library("rmarkdown")
}else{
  library("rmarkdown")
}

if (!require("pls")) {
  install.packages("pls")
  library("pls")
}else{
  library("pls")
}

if (!require("gtools")) {
  install.packages("gtools")
  library("gtools")
}else{
  library("gtools")
}

if (!require("ggplot2")) {
  install.packages("ggplot2")
  library("ggplot2")
}else{
  library("ggplot2")
}

if (!require("igraph")) {
  install.packages("igraph")
  library("igraph")
}else{
  library("igraph")
}

# ................................................................
# ................................................................
# Read data #### 
# Read data with selected traits and explanatory variables to be analysed
pars <- jsonlite::fromJSON(infoname)
pars <- ClimMobTools:::.decode_pars(pars)

cmdata <- jsonlite::fromJSON(outputname)
class(cmdata) <- union("CM_list", class(cmdata))
cmdata <- as.data.frame(cmdata, tidynames = FALSE, pivot.wider = TRUE)

# ................................................................
# ................................................................
# Dataset parameters ####
Option <- ClimMobTools:::.title_case(option)

# the project name
projname <- which(grepl("project_name", names(cmdata)))
projname <- cmdata[1, projname]

# variables to produce split of results into multiple groups.
expvar <- pars$expl$vars
expvar_full <- pars$expl$name

# number of rankers
nranker <- nrow(cmdata)

itemnames <- cmdata[, grepl("package_item", names(cmdata))]

# Number of items each participant evaluates
ncomp <- ncol(itemnames)

# Name of items tested
items <- unique(sort(unlist(itemnames)))

# Number o items tested
nitems <- length(items)

# Number of characteristics (traits) avaluated
ntrait <- nrow(pars$chars)

# Colnames where items are placed within cmdata
itemnames <- names(itemnames)

# number of questions
nquest <- pars$chars$n_quest[1]

# define which function should be called to build the rankings
if (ncomp == 3) {
  rankwith <- "rank_tricot"
}

if (ncomp > 3) {
  rankwith <- "rank_numeric"
}

# ................................................................
# ................................................................
# Statistic parameters ####
# Set maximum proportion of missing data allowed in a characteristic evaluation
# before it is excluded. 
missper <- 0.5

# Set minimum proportion of valid observations in explanatory variables
missexp <- 0.8

# Set minimum split size for tree models.
minsplit <- ceiling(nrow(cmdata) * 0.1)
if (minsplit < 10) {minsplit <- 10}
# Set alpha
sig_level <- 0.05

# method for adjustments for confidence intervals and setting widths for comparison. 
# Defaults to B-H (Benjamini an Hochberg). Any of the methods from p.adjust will work here 
# though: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"
ci_adjust <- "BH"

# confidence interval level for comparison plots with error bars. 84% to give an
# approximate 5% significance level for comparisons of non-overlapping confidence
# intervals (e.g. https://www.ncbi.nlm.nih.gov/pmc/articles/PMC524673/)
# should probably allow alternatives to be 0.9, 0.95 or 0.99
ci_level <- 0.84

# resolution of display items
dpi <- 200


# participant report params 
info_table_items <- c() #info.table.items <- c("variety 1", "variety 2", "variety 3")
info_table_info <- c() #info.table.info <- c("plant it early", "good in high altitude", "")
info_table_typeinfo <- "" #info.table.typeinfo <- "expert advice"

# ................................................................
# ................................................................
# Run analysis ####
source(paste0(fullpath, "/R/analysis_climmob.R"))

# ................................................................
# ................................................................
# Write outputs ####
rankers <- ClimMobTools:::.pluralize(ranker)
options <- ClimMobTools:::.pluralize(option)
nothertraits <- ntrait - 1

#determine format based on extensions
output_format <- ifelse(extension == "docx","word_document", 
                        paste0(extension,"_document"))

# produce main report if output type is "summary" or "both"
dir.create(pathname, showWarnings = FALSE, recursive = TRUE)
rmarkdown::render(paste0(fullpath, "/report/", language, "/mainreport/mainreport.Rmd"),
                  output_dir = pathname,
                  output_format = output_format,
                  output_file = paste0(projname,"_report",".",extension))

# #produce farmer reports if output type is "farmer" or "both"
# if (infosheets) {
#   source("Farmer Reports/farmerreport.R")
# }