## Packages ####
library("ClimMobTools")
library("gosset")
library("PlackettLuce")
library("partykit")
library("qvcalc")
library("psychotools")
library("jsonlite")
library("multcompView")
library("knitr")
library("rmarkdown")
library("pls")
library("gtools")
library("ggplot2")
library("igraph")
library("mapview")
library("ggrepel")
library("ggparty")
library("patchwork")

source(paste0("R/functions.R"))


key <- "096a48e3-b2fa-40ce-b3bf-c3f91edc0b55"

cmdata <- getDataCM(key, "ARDBFB2019", pivot.wider = TRUE, tidynames = FALSE)

# replace randomization
pkg <- read.csv("dev/data/ARDBFB2019/pkgs_ILRIDBFB2019.csv")
pkg[,1] <- gsub("Package #","", pkg[,1])
pkg[2:4] <- lapply(pkg[2:4], function(x){
  gsub("\n| ","",x) 
})
names(pkg) <- c("id",paste0("package_item_", LETTERS[1:3]))

cmdata <- cmdata[,-match(paste0("package_item_", LETTERS[1:3]), names(cmdata))]

# combine new packages
cmdata <- merge(cmdata, pkg, by = "id", all.x = TRUE)

cmdata

#write.csv(cmdata, "dev/data/ARDBFB2019/ARDBFB2019_data.csv", row.names = FALSE)

charpattern <- c("_overallperf_","_frf_","_prf_","_drf_","_emf_","_phf_","_tilf_","_splf_","_nsf_","_gyf_")
newname <- c("Overall Characteristic","Frost tolerant","Pest tolerant","Disease resistance","Earlines",
             "Plant height","Tillering","Spike length","Seeds per spike","Grain yield")

chars <- data.frame()

for(i in seq_along(charpattern)){
  index <- charpattern[i]
  index <- which(grepl(index, names(cmdata)))
  
  ch <- data.frame(quest_1 = names(cmdata)[index[1]],
                   quest_2 = names(cmdata)[index[2]],
                   n_quest = 2,
                   char_full = newname[i],
                   char = newname[i])
  
  chars <- rbind(chars, ch)
  
}


perf <- data.frame(quest_1 = "farmersevaluationsecond_ASS8d8942ff1f4c_perf_overallchar_1",
                   quest_2 = "farmersevaluationsecond_ASS8d8942ff1f4c_perf_overallchar_2",
                   quest_3 = "farmersevaluationsecond_ASS8d8942ff1f4c_perf_overallchar_3",
                   n_quest = 3,
                   perf_full = "Overall performance",
                   perf = "overall_performance")

expl <- data.frame(name = c("What is the gender?","Main crop", "Longitude", "Latitude"),
                   id = NA,
                   vars = c("registration_REG_gender","geopoint_ASSec43b9cf61d6_pc","geopoint_ASSec43b9cf61d6_lon","geopoint_ASSec43b9cf61d6_lat"))



pars <- list(chars = chars, expl = expl, perf = perf)
rm(ch, chars, expl, perf, i, charpattern, key, newname, index, pkg)
# ................................................................
# ................................................................
# ................................................................
# ................................................................
# Dataset parameters ####
option <- "variety"
ranker <- "farmer"
extension <- "docx"
language <- "en"
fullpath <- getwd()

Option <- ClimMobTools:::.title_case(option)

# the project name
projname <- which(grepl("project_name", names(cmdata)))
projname <- cmdata[1, projname]

pathname <- paste0("dev/output/",projname,"/")

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
sig_level <- 0.1

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
dpi <- 250
out_width <- "100%"


# participant report params 
info_table_items <- c() #info.table.items <- c("variety 1", "variety 2", "variety 3")
info_table_info <- c() #info.table.info <- c("plant it early", "good in high altitude", "")
info_table_typeinfo <- "" #info.table.typeinfo <- "expert advice"

# ................................................................
# ................................................................
# Run analysis ####
dir.create(pathname, showWarnings = FALSE, recursive = TRUE)
source(paste0("R/analysis_climmob.R"))

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
rmarkdown::render(paste0("report/", language, "/mainreport/mainreport.Rmd"),
                  output_dir = pathname,
                  output_format = output_format,
                  output_file = paste0(projname,"_report",".",extension))
