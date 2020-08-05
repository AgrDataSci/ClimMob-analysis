# # ................................................................
# # ................................................................
# # Main script to call for the analysis and rendering ClimMob reports
# # ................................................................
# # ................................................................

tag <- "leaders"
args <- c(paste0("dev/data/",tag,"/data.json"), paste0("dev/data/",tag,"/info.json"),
         paste0("dev/output/",tag,"/"), "TRUE","en","docx",
         "participant", "option", getwd())

# get the arguments from server's call
# args <- commandArgs(trailingOnly = TRUE)
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
# Read data #### 
# Read data with selected traits and explanatory variables to be analysed
pars <- tryCatch({
  pars <- jsonlite::fromJSON(infoname)
  pars <- ClimMobTools:::.decode_pars(pars)
},error = function(e) {
    e$message <-
      paste(
        "Unable to read the json file with the",
        "parameters required to perform the analysis."
      )
  }
)

# Read as json and add the class "CM_list" so it can be passed to as.data.frame 
# method from ClimMobTools
cmdata <- tryCatch({
  cmdata <- jsonlite::fromJSON(outputname)
  class(cmdata) <- union("CM_list", class(cmdata))
  cmdata <- as.data.frame(cmdata, tidynames = FALSE, pivot.wider = TRUE)
},error = function(e) {
  e$message <-
    paste(
      "Unable to read the json file with the",
      "ClimMob data."
    )
}
)

# ................................................................
# ................................................................
# Participant report parameters ####
info_table_items <- c() #info.table.items <- c("variety 1", "variety 2", "variety 3")
info_table_info <- c() #info.table.info <- c("plant it early", "good in high altitude", "")
info_table_typeinfo <- "" #info.table.typeinfo <- "expert advice"

# ................................................................
# ................................................................
# Run analysis ####
tryCatch({
  dir.create(pathname, showWarnings = FALSE, recursive = TRUE)
})

source(paste0(fullpath, "/R/analysis_climmob.R"))

# ................................................................
# ................................................................
# Write outputs ####
#determine format based on extensions
output_format <- ifelse(extension == "docx","word_document", 
                        paste0(extension,"_document"))

# produce the reports
if (isTRUE(done)) {
  
  # the main report
  done <- tryCatch({
    rmarkdown::render(paste0(fullpath, "/report/", language, "/mainreport/mainreport.Rmd"),
                      output_dir = pathname,
                      output_format = output_format,
                      output_file = paste0(projname, "_climmob_main_report", ".", extension))
  }, error = function(e) {
    return(FALSE)
  }
  )
  
  # farmer reports if output type is "farmer" or "both"
  # if (isTRUE(infosheets)) {
  #   source("Farmer Reports/farmerreport.R")
  # }
}

# if there was any error in the analysis, produce a error report 
if (isFALSE(done)) {
  rmarkdown::render(paste0(fullpath, "/report/", language, "/mainreport/mainreport_failed.Rmd"),
                    output_dir = pathname,
                    output_format = output_format,
                    output_file = paste0(projname, "_climmob_main_report", ".", extension))
}

