# This script is used to prepared a group of ClimMob data to run the analysis 
# locally. You can use this for example to add environmental variables and/or 
# combine results from different projects. All the process is manual and you need to 
# adapt some parts if this scripts depending on the type of data that you are dealing with. 
# Mostly depending on how you designed your tricot project(s) and how similar they are to
# each other in terms of data structure and behaviour
# Good luck and contact me if you need support and/or advice
# Kauê de Sousa

# Load packages
library("ClimMobTools")
library("gosset")
library("climatrends")
library("chirps")
library("nasapower")
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
source("R/functions.R")

# Add API key, if data will be fetched from ClimMob using ClimMobTools
key <- "920b3608-a88b-452b-bd0f-6e3153e25666"

# Add project id(s), if data will be fetched from ClimMob using ClimMobTools
getProjectsCM(key)


ids <- c("Bean21MM")

lt <- list()
quest <- data.frame()

# Run across projects 
for(i in seq_along(ids)) {
  x <- getDataCM(key, project = ids[[i]], as.data.frame = FALSE)

  quest <- rbind(quest, x$specialfields)

  x <- as.data.frame(x, pivot.wider = TRUE, tidynames = TRUE)
  
  lt[[i]] <- x
}

# It is important to have all the data in one single data frame 
# This means that all columns should match
# You need to check the columns names in each data frame in the list 
# Sometimes they don't have the same name due to non standardization of 
# data collection moment
# So try to fix it, using grepl() gsub() or other replacement functions

# Combine data by rows, this function will look for identical names
# if no match it will create a new column and fill with NAs 
cmdata <- rowbind(lt)

names(cmdata)


# ................................
# Make list of parameters ####
# This is the list of traits (characteristics) assessed in the 
# project
quest

# remove duplicates in quest
questions <- quest[!duplicated(quest$desc), ]
# and keep only the questions pointing to the positive side of 
# tricot questions
questions$name[which(grepl("_pos$", questions$name))]

# look for the the pattern in cmdata as some traits were asked more than once in 
# different data collection moments
traitpattern <- names(cmdata)[which(grepl("_pos$", names(cmdata)))]
traitpattern <- gsub("_pos$", "", traitpattern)
traitpattern <- unique(traitpattern)

# This is the vector with string patterns to be searched in cmdata
traitpattern

# We need to provide the clean names for these traits manually use 
# print(traitpattern) to guide this
newname <- c('Germination', 'Vigor', 'Diseases resistance', 
             'Pest resistance', 'Flood tolerance', 'Drought tolerance',
             'Plant survival', 'Maturity', 'Grain yield', 
             'Seed size', 'Seed colour', 'Marketability', 
             'Taste', 'Cooking time', 'Overall')

# Check if both vectors have the same length
length(traitpattern) == length(newname)

# Now we should create a data frame with parameters for the traits
# this will be used internally by ClimMob.R to look for the string patterns in 
# the data and sort out for the analysis 
traits <- data.frame()
# Run over the vector traipattern
for(i in seq_along(traitpattern)){
  
  index <- traitpattern[i]
  
  # Look for the positive and negative tricot questions
  pos <- which(grepl(paste0(index, "_pos"), names(cmdata)))
  neg <- which(grepl(paste0(index, "_neg"), names(cmdata)))
  
  string <- strsplit(index, "_")[[1]]

  tr <- data.frame(nameString1 = names(cmdata)[pos],
                   nameString2 = names(cmdata)[neg],
                   nQst = 2,
                   name = newname[i],
                   codeQst = gsub(" ", "", ClimMobTools:::.title_case(newname[i])),
                   questionAsked1 = questions$desc[which(grepl(paste0(string[length(string)], "_pos"), questions$name))],
                   questionAsked2 = questions$desc[which(grepl(paste0(string[length(string)], "_neg"), questions$name))],
                   assessmentId = string[1],
                   assessmentName = string[1],
                   assessmentDay = "0",
                   traitOrder = "otherTraits")
  
  traits <- rbind(traits, tr)
  
}

# Define the reference trait
# Select one among the vector in nameString1
traits$nameString1
referencetrait <- "final_survey_overall_pos"
traits[which(grepl(referencetrait, traits$nameString1)), "traitOrder"] <- "referenceTrait"


# Check if there is a comparison with the local
# if not create this object 
# tricotVSlocal <- data.frame()

quest

names(cmdata)[which(grepl("perf_overallvslocal_1", names(cmdata)))]

tricotVSlocal <- data.frame(nameString1 = "final_survey_perf_overallvslocal_1",
                            nameString2 = "final_survey_perf_overallvslocal_2",
                            nameString3 = "final_survey_perf_overallvslocal_3",
                            name = "Tricot vs Local",
                            codeQst = "tricotVSlocal",
                            nQst = 3)


# ............................................
# ............................................
# Some data cleaning
# which will be dependent of the type of data and how it was collected

fixnames <- lapply(strsplit(cmdata$package_participant_name, "_"), function(x) x[[1]])
cmdata$package_participant_name <- as.vector(do.call("rbind", fixnames))

survivingplots <- gsub("D|; D|D; ", "", cmdata$final_survey_survivingplots)

pack <- cmdata[,c("package_item_A", "package_item_B", "package_item_C")]

for (i in seq_along(survivingplots)) {
  if(survivingplots[i] %in% LETTERS[1:3]) {
    pack[i,which(LETTERS[1:3] %in% survivingplots[i])] <- NA
  }
}


cmdata[,c("package_item_A", "package_item_B", "package_item_C")] <- pack

cmdata$registration_farmdistrict <- gsub("Ileje0|IlejeIleje", "Ileje", cmdata$registration_farmdistrict)

# ............................................
# ............................................
# Select the covariates
# Here you select covariates from cmdata and/or 
# can add external covariates to cmdata data like environmental indices
# or socio-economic data from a different survey
# You need to build a data.frame with 
# codeQst = a vector with clean names to be displayed in the report, indicating what
#  the covariates mean
# namaString = a vector with names that match the covariates selected in cmdata 
names(cmdata)[which(!grepl("_pos$|_neg$", names(cmdata)))]

covariates <- data.frame(assessmentId = "DataCollection",
                         assessmentName = "Data collection",
                         codeQst = c("Gender",
                                     "District",
                                     "Longitude",
                                     "Latitude", 
                                     "Altitude"),
                         id = "",
                         nameString = c("registration_genderfarmer", 
                                        "registration_farmdistrict", 
                                        "registration_swagps_longitude", 
                                        "registration_swagps_latitude",       
                                        "registration_swagps_elevation"),
                         name = "",
                         questionAsked = "")

covariates$name <- covariates$codeQst
covariates$questionAsked <- covariates$codeQst 

# Check if the names match with the ones in cmdata
all(covariates$nameString %in% names(cmdata))

# Put the parameters in the list
pars <- list(traits = traits, tricotVSlocal = tricotVSlocal, covariates = covariates)
rm(tr, traits, tricotVSlocal, covariates, i, traitpattern, key, newname, index)

# ................................................................
# ................................................................
# ................................................................
# ................................................................
# Dataset parameters ####
tag <- "BeanTanzaniaOAF" # the project name
projname <- tag
outputpath  <- paste0(getwd(), "/run-local/output/", tag)
infosheets  <- FALSE # logical, if infosheets should be written TRUE FALSE
language    <- "sw" # the language to write the report
extension   <- "docx" # report file format it can be "docx", "pdf", and "html"
ranker      <- "farmer" # how the system will refer to participants/farmers
option      <- "variety" # how the system will refer to tested items
fullpath    <- getwd() # this is backward path
reference   <- "Uyole Njano" # the reference item for the analysis
minN        <- 5 # minimum n of complete data required in a trait evaluation before it is excluded
minitem     <- 2 # minimum n of items tested, e.g. that all items are tested at least twice
mincovar    <- 0.90 # minimum proportion of covariates compared to total valid n
sig_level   <- 0.5 # significance level
sig_level_tree   <- 0.5 # significance tree level
minsplit    <- 50 # minimum n in each tree node

# Two objects to begin with that will be used to verify the process
error <- NULL
done <- TRUE


# ................................................................
# ................................................................
# Run analysis ####
# read the file with the main script to check where to begin and end when
# applying source2()
checkfile <- readLines("ClimMob.R")
b <- which(grepl("# 1. Run analysis", checkfile))
e <- which(grepl("# End of analysis", checkfile))

# Run the workflow to analyse the data and produce the report 
source2("ClimMob.R", b, e)




