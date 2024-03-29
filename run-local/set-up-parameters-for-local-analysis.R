# Run ClimMob Analysis locally  
#
# This workflow is used to prepare the tricot data to run the analysis locally.
# You can use this, for example, to add environmental variables and/or 
# combine data from different trials into a single report.
# All the process is manual and you need to 
# adapt some parts of this workflow depending on the type of data that you are dealing with. 
# Mostly depending on how you designed your tricot project(s) and how similar they are to
# each other in terms of data collection moments and questions' ids.
# If you need support and/or advice, please submit an issue using the link below
# https://github.com/AgrDataSci/ClimMob-analysis/issues
# Kauê de Sousa 

# ................................................................
# ................................................................
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
# 1. Load modules ####
modules = list.files(paste0("modules"), full.names = TRUE)
modules = modules[-which(grepl("check_packages.R", modules))]
for (i in seq_along(modules)) {
  source(modules[i])
}

# ...................................
# ...................................
# 2. Fetch ClimMob data ####
# Add API key, if data will be fetched from ClimMob using ClimMobTools
keys = "ff05a174-28d0-4a40-ab5a-35dc486133a6"

server = "1000farms"

projects = getProjectsCM(keys, server = server)

projects

# select the projects to merge 
sel = c("eggaru23", "eggken22")
keep = projects$project_id %in% sel

projects = projects[keep, ]

# some empty objects to fill in
jsonlt = list()
lt = list()
quest = data.frame()

# Run across projects 
for(i in seq_along(projects$project_id)) {
  print(i)
  y = getDataCM(keys, 
                project = projects$project_id[i], 
                server = projects$server[i],
                userowner = projects$user_owner[i],
                as.data.frame = FALSE)
  
  quest = rbind(quest, y$specialfields)
  
  x = as.data.frame(y, pivot.wider = TRUE, tidynames = TRUE)
  
  lt[[i]] = x
  jsonlt[[i]] = y
  
}

# ...................................................
# ...................................................
# 3. Harmonize column names in ClimMob data ####
# It is important to have all the data in one single data frame 
# This means that all columns should match
# You need to check the columns names in each data frame in the list 
# Sometimes they don't have the same name due to non standardization of 
# data collection moment
# So try to fix it, using grepl() gsub() or other replacement functions
# ................................
# Make list of parameters ####
# This is the list of traits (characteristics) assessed in the 
# project
questions = quest

# remove duplicates in quest
questions$name2 = sapply(quest$name, function(x) {
  z = strsplit(x, "_")[[1]]
  z = z[-c(1,2)]
  z = as.vector(paste(z, collapse = "_"))
  z = gsub("_pos", "", z)
  z = gsub("_neg", "", z)
  z
})

questions = questions[!duplicated(questions$name2), ]

questions

# some questions from the different project have different code but are the same 
# we fix this by creating a single code
questions$final_name = sapply(questions$name2, function(x){
  x[x == "cbsdrootnec"] = "cbsdrootnecrosis"
  x[x == "cbsd21b"] = "cbsd"
  x[x == "cuttingsplantingmaterial"] = "plantmaterial"
  x[x == "mosaicdis21b"] = "mosaicdis"
  x[x == "bestwostyield"] = "rootyield"
  x
})
 
questions$name2 = paste0("_", questions$name2, "_")
questions$final_name = paste0("_", questions$final_name, "_")

# now rename the data frames in the list to match
for(i in seq_along(lt)) {
  for(j in seq_along(questions$name2)){
    names(lt[[i]]) = gsub(questions$name2[j], questions$final_name[j], names(lt[[i]]))
  }
}

# now the same for the data collection moments
# sometimes is the same data collection moment but recorded with 
# a different name
unique(unlist(lapply(lt, names)))

assessments = data.frame()

for(i in seq_along(jsonlt)) {
  x = data.frame(name = jsonlt[[i]]$assessments$desc,
                 code = gsub(" ", "", tolower(jsonlt[[i]]$assessments$desc)),
                 project_cod = jsonlt[[i]]$project$project_cod)
  assessments = rbind(assessments, x)
}

assessments

# rename cols of assessments
assessments$final_name = sapply(assessments$code, function(x){
  x[x == "agronomyperformance"] = "agronomicperformance"
  x
})

assessments

# now rename the data frames in the list to match
for(i in seq_along(lt)) {
  for(j in seq_along(assessments$code)){
    names(lt[[i]]) = gsub(assessments$code[j], assessments$final_name[j], names(lt[[i]]))
  }
}

cmdata = rowbind(lt)

sort(names(cmdata))

# ...................................................
# ...................................................
# 4. Prepare parameters of traits to analyse ####
# This is the vector with string patterns to be searched in cmdata
questions = questions[!duplicated(questions$final_name),]
questions[1:ncol(questions)] = lapply(questions[1:ncol(questions)], function(x){
  gsub("overallchar", "overall", x)
})
questions = questions[!grepl("overallper", questions$name2), ]
traitpattern = unique(questions$final_name)
newname = title_case(unique(questions$final_name))

# Check if both vectors have the same length
length(traitpattern) == length(newname)

# Now we must create a data frame with parameters for the traits
# this will be used internally by ClimMob.R to look for the string patterns in 
# the data and sort out for the analysis 
traits = data.frame()

# Run over the vector traipattern
for(i in seq_along(traitpattern)){
  
  index = traitpattern[i]
  
  # Look for the positive and negative tricot questions
  pos = which(grepl(paste0(index, "pos"), names(cmdata)))
  neg = which(grepl(paste0(index, "neg"), names(cmdata)))
  
  for (j in seq_along(pos)) {
    
    string = strsplit(index, "_")[[1]]
    
    tr = data.frame(nameString1 = names(cmdata)[pos[j]],
                     nameString2 = names(cmdata)[neg[j]],
                     nQst = 2,
                     name = newname[i],
                     codeQst = gsub(" ", "", newname[i]),
                     questionAsked1 = questions$desc[which(grepl(index, questions$final_name))],
                     questionAsked2 = "",
                     assessmentId = strsplit(names(cmdata)[pos[j]], "_")[[1]][[1]],
                     assessmentName = strsplit(names(cmdata)[pos[j]], "_")[[1]][[1]],
                     assessmentDay = "0",
                     traitOrder = "otherTraits")
    
    traits = rbind(traits, tr)
    
  }
  
}

rownames(traits) = 1:nrow(traits)

traits

# Define the reference trait
# Select one among the vector in nameString1
traits$nameString1
referencetrait = "agronomicperformance_overall_pos"
traits[which(grepl(referencetrait, traits$nameString1)), "traitOrder"] = "referenceTrait"

# Make nice names for data collection
oldnames = unique(traits$assessmentName)
oldnames
newnames = c("Vegetative", "Agronomic")

assessday = c(1, 2) 

for (i in seq_along(oldnames)) {
  traits$assessmentName = ifelse(traits$assessmentName == oldnames[i], newnames[i], traits$assessmentName)
  traits$assessmentDay = ifelse(traits$assessmentId == oldnames[i], assessday[i], traits$assessmentDay)
}

traits$assessmentDay = as.integer(traits$assessmentDay)

traits = traits[order(traits$assessmentDay), ]

traits

# Now nice names for the traits 
traits$name = gsub("_qst_|_|tricot", "", traits$name)
traits$name = title_case(traits$name)
traits$name = gsub("resistance", "Resistance", traits$name)
traits$name = gsub("tolerance", "Tolerance", traits$name)
traits$name = gsub("colour", "Colour", traits$name)
traits$name = gsub("feeding", "Feeding", traits$name)
traits$name = gsub("colour", "Colour", traits$name)
traits$name = gsub("requirement", "Requirement", traits$name)
traits$name = gsub("population", "Population", traits$name)

traits

# ...................................................
# ...................................................
# 5. Add comparison with local ####
# NOT SUPPORTED YET, create an empty data frame
# This is the vector with string patterns to be searched in cmdata
# Check if there is a comparison with the local
# if not create this object 
tricotVSlocal = data.frame()

# quest
# 
# names(cmdata)[which(grepl("perf_overallvslocal_1", names(cmdata)))]
# 
# tricotVSlocal = data.frame(nameString1 = "final_survey_perf_overallvslocal_1",
#                             nameString2 = "final_survey_perf_overallvslocal_2",
#                             nameString3 = "final_survey_perf_overallvslocal_3",
#                             name = "Tricot vs Local",
#                             codeQst = "tricotVSlocal",
#                             nQst = 3)

# ...................................................
# ...................................................
# 6. Add parameters for continuous data ####
# This is the vector with string patterns to be searched in cmdata
linear = y$assessments$fields[[2]]

linear = linear[grepl("_a$|_b$|_c$", linear$name), ]

linear = linear[,c("name", "desc")]

traitq = unique(gsub("_a|_b|_c", "", linear$name))

traitq = traitq[!grepl("date", traitq)]

quanti = data.frame()

for (i in seq_along(traitq)) {
  
  index = paste0(traitq[i], "_", letters[1:3])
  
  q = data.frame(nameString1 = names(cmdata)[grep(index[1], names(cmdata))],
             nameString2 = names(cmdata)[grep(index[2], names(cmdata))],
             nameString3 = names(cmdata)[grep(index[3], names(cmdata))],
             nQst = 3,
             name = traitq[i],
             codeQst = traitq[i],
             questionAsked1 = "",
             questionAsked2 = "",
             questionAsked3 = "",
             assessmentId = "AgronomicPerformance",
             assessmentName = "AgronomicPerformance",
             assessmentDay = "0")
  
  quanti = rbind(quanti, q)
  
}

quanti

# ............................................
# ............................................
# 7. Some data cleaning ####
# which will be dependent of the type of data and how it was collected
# check the item names
sort(unique(unlist(cmdata[paste0("package_item_", LETTERS[1:3])])))

table(cmdata$registration_gender1)
table(cmdata$registration_district)

# Here you can add code to edit cmdata



# ............................................
# ............................................
# 8. Select the covariates #####
# Here you select covariates from cmdata and/or 
# can add external covariates to cmdata data like environmental indices
# or socio-economic data from a different survey
# You need to build a data.frame with 
# codeQst = a vector with clean names to be displayed in the report, indicating what
#  the covariates mean
# namaString = a vector with names that match the covariates selected in cmdata

names(cmdata)[which(!grepl("_pos$|_neg$", names(cmdata)))]

covariates = data.frame(assessmentId = "",
                         assessmentName = "Assessment",
                         codeQst = c("registration_gender1", "registration_district"),
                         id = "",
                         nameString = c("registration_gender1", "registration_district"),
                         name = c("Gender","District"),
                         questionAsked = "")

# Check if the names match with the ones in cmdata
all(covariates$nameString %in% names(cmdata))

# Put the parameters in the list
pars = list(traits = traits, tricotVSlocal = tricotVSlocal, covariates = covariates, linear = quanti)
rm(tr, traits, tricotVSlocal, covariates, i, traitpattern, keys, newname, index)

# ................................................................
# ................................................................
# ................................................................
# ................................................................
# 9. Run the analysis ####
tag = "Eggplant Kenya and Tanzania" # the project name
unique(cmdata$package_item_A)
reference   = c("Black beauty (check)", "Local Check") # the reference item for the analysis
project_name = paste(tag)
outputpath  = paste0(getwd(), "/run-local/output/", project_name)
infosheets  = F # logical, if infosheets should be written TRUE FALSE
language    = "en" # the language to write the report
extension   = "docx" # report file format it can be "docx", "pdf", and "html"
ranker      = "participant" # how the system will refer to participants/farmers
option      = "variety" # how the system will refer to tested items
fullpath    = getwd() # this is backward path
minN        = 5 # minimum n of complete data required in a trait evaluation before it is excluded
minitem     = 2 # minimum n of items tested, e.g. that all items are tested at least twice
mincovar    = 0.95 # minimum proportion of covariates compared to total valid n
sig_level   = 0.1 # significance level for the Plackett-Luce model
groups       = ""
language = "en"

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

# Run analysis
# read the file with the main script to check where to begin and end when
# applying source2()
checkfile = readLines("ClimMob.R")
b = which(grepl("2. Organise quantitative data ####", checkfile))
e = which(grepl("# End of analysis", checkfile))
error = NULL
# Run the workflow to analyse the data and produce the report 
source2("ClimMob.R", b, e)

