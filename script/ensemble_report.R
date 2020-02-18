# ................................................................
# ................................................................
# Analyse the performance of agricultural technologies from 
# crowdsourcing trials using Plackett-Luce model
# ................................................................
# ................................................................
# Kaue de Sousa 
# Updated 17Feb2020
# ................................................................
# ................................................................
#system("R-script, climmobv3_analysis.R info.json data.json r.json /output/ TRUE TRUE")
args <- c(
  "data/data.json",
  "data/info.json",
  "output",
  TRUE
)

# get the arguments
# args <- commandArgs(trailingOnly = TRUE)
infoname       <- args[1] # a json file with parameters for the analysis
outputname     <- args[2] # a json file with the results
pathname       <- args[3] # the path where results will be written
infosheets     <- as.logical(args[5]) # logical() TRUE FALSE

# ................................................................
# ................................................................
## Packages ####
library("ClimMobTools")
library("gosset")
library("PlackettLuce")
library("partykit")
library("qvcalc")
library("psychotools")
library("jsonlite")
library("formattable")
library("knitr")
library("rmarkdown")
library("pls")
source("script/functions.R")

# ................................................................
# ................................................................
# Read data #### 
# Read data with selected traits and explanatory variables to be analysed
pars <- jsonlite::fromJSON(infoname)
pars <- ClimMobTools:::.decode_pars(pars)

cmdata <- jsonlite::fromJSON(outputname)
class(cmdata) <- union("CM_list", class(cmdata))
cmdata <- as.data.frame(cmdata, tidynames = FALSE, pivot.wider = TRUE)

cmdata[1, c(28:32)] <- NA
cmdata[3,c(24:25)] <- NA
# ................................................................
# ................................................................
# Set up main style parameters to fill up the report ####

# the language to write the report "en" for english and "es" for spanish
language <- "en"

# the project name
projname <- which(grepl("project_name", names(cmdata)))
projname <- cmdata[1, projname]

# variables to produce split of results into multiple groups.
forcesplit <- pars$expl$vars

# Outputs requested. should be "summary" for summary report only; 
# "participant" for participant report only or "both" for both sets of reports
output <- "both"

# File format to use in rendering output. Only available options should be 
# "docx", "pdf", and "html". Output format derived from extension
extension <- "docx"

# Set how the system will refer to each of the different options and 
# to each of the different rankers. Defaults to "participant" and "item". 
ranker <- "participant"
option <- "item"

nranker <- nrow(cmdata)

itemnames <- cmdata[, grepl("package_item", names(cmdata))]

ncomp <- ncol(itemnames)

items <- unique(sort(unlist(itemnames)))

nitems <- length(items)

ntrait <- nrow(pars$chars)

itemnames <- names(itemnames)

# number of questions
nquest <- pars$chars$n_quest[1]

# Statistics parameters
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

# set maximum proportion of missing data allowed in a variable before it is
# excluded. 20% seems to work reasonably - probably should restrict the maximum
# value to something like 30 or 40%.
missper <- 0.2

# Set minimum split size for tree models. Probably should be tied cleverly into
# the number of varieties being compared, but can allow user to push this back or
# forth if they want
minsplit <- ceiling(nrow(cmdata) * 0.1)

# participant report params 
info_table_items <- c() #info.table.items <- c("variety 1", "variety 2", "variety 3")
info_table_info <- c() #info.table.info <- c("plant it early", "good in high altitude", "")
info_table_typeinfo <- "" #info.table.typeinfo <- "expert advice"

# ................................................................
# ................................................................
# Run analysis ####
source("script/analysis_climmob.R")



# ................................................................
# ................................................................
# Write outputs ####

#determine format based on extensions
output_format <- ifelse(extension == "docx","word_document", 
                        paste0(extension,"_document"))

#produce main report if output type is "summary" or "both"
dir.create(pathname, showWarnings = FALSE, recursive = TRUE)
if(output != "participant"){
  rmarkdown::render(paste0("main_report/",language,"/mainreport.Rmd"),
                    output_dir = pathname,
                    output_format = output_format,
                    output_file = paste0(projname,"_report",".",extension))
}

#produce farmer reports if output type is "farmer" or "both"
if(output!="summary"){
  source("Farmer Reports/farmerreport.R")
}



