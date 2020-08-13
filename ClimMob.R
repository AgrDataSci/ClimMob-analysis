# # ................................................................
# # ................................................................
# # Main script to call for the analysis and rendering ClimMob reports
# # ................................................................
# # ................................................................

# get the arguments from server's call
args <- commandArgs(trailingOnly = TRUE)
infoname    <- args[1] # a json file with parameters for the analysis
outputname  <- args[2] # a json file with the results
outputpath  <- args[3] # the path where results will be written
infosheets  <- as.logical(args[4]) # logical, if infosheets should be written TRUE FALSE
language    <- args[5] # the language to write the report "en" for english and "es" for spanish
extension   <- args[6] # report file format it can be "docx", "pdf", and "html"
ranker      <- args[7] # how the system will refer to participants/farmers
option      <- args[8] # how the system will refer to tested items
fullpath    <- args[9] # this is backward path
reference   <- args[10] # the reference item for the analysis
if (isTRUE(is.na(reference))) {
  reference <- 1
}

# Two objects to begin with that will be used to verify the process
error <- NULL
done <- TRUE

# Function to validate the class of objects generated in the tryCatch(s)
any_error <- function(x){
  isTRUE("error" %in% class(x))
}

# ................................................................
# ................................................................
# Read data #### 
# Read data with selected traits and explanatory variables to be analysed
try_pars <- tryCatch({
  pars <- jsonlite::fromJSON(infoname)
  pars <- ClimMobTools:::.decode_pars(pars)
}, error = function(cond) {
    return(cond)
  }
)

if (any_error(try_pars)) {
  e <- paste("Error 101.", try_pars$message)
  error <- c(error, e)
  done <- FALSE
}

# Read the trial data 
try_cmdata <- tryCatch({
  # add the class "CM_list" so it can be passed to 
  # the as.data.frame() method from ClimMobTools
  cmdata <- jsonlite::fromJSON(outputname)
  class(cmdata) <- union("CM_list", class(cmdata))
  cmdata <- as.data.frame(cmdata, tidynames = FALSE, pivot.wider = TRUE)
  
  # read the questions the were made
  quest <- jsonlite::fromJSON(outputname)
  quest <- quest[["specialfields"]]
  
}, error = function(cond) {
  return(cond)
}
)

if (any_error(try_cmdata)) {
  e <- paste("Error 102.", try_cmdata$message)
  error <- c(error, e)
  done <- FALSE
}

# ................................................................
# ................................................................
# Run analysis ####
tryCatch({
  dir.create(outputpath, showWarnings = FALSE, recursive = TRUE)
})

if (isTRUE(done)) {
  source(paste0(fullpath, "/R/analysis_climmob.R"))
}


# ................................................................
# ................................................................
# Write outputs ####
#determine format based on extensions
output_format <- ifelse(extension == "docx","word_document", 
                        paste0(extension,"_document"))


if (all(infosheets, done)) {
  
  tryCatch({
    dir.create(paste0(outputpath, "/participant_report/png"),  
               showWarnings = FALSE, recursive = TRUE)
  })
  
  try_infosheet <- tryCatch({
    
    source(paste0(fullpath, "/R/participant_report.R"))
    
  }, error = function(cond) {
    return(cond)
  }
  )
  
  if (any_error(try_infosheet)) {
    e <- paste("Error 115. Participant(s) report. ", try_infosheet$message)
    error <- c(error, e)
  }
  
}

# produce the main report
if (isTRUE(done)) {
  # the main report
  try_rep <- tryCatch({
    rmarkdown::render(paste0(fullpath, "/report/", language, "/mainreport.Rmd"),
                      output_dir = outputpath,
                      output_format = output_format,
                      output_file = paste0("climmob_main_report", ".", extension))
  }, error = function(cond) {
    return(cond)
  }
  )
  
  if (any_error(try_rep)) {
    e <- paste("Error 116.", try_rep$message)
    error <- c(error, e)
    done <- FALSE
  }
  
}

# if there was any error in the analysis, produce a error report 
if (isFALSE(done)) {
  rmarkdown::render(paste0(fullpath, "/report/", language, "/mainreport_failed.Rmd"),
                    output_dir = outputpath,
                    output_format = output_format,
                    output_file = paste0("climmob_main_report", ".", extension))
}

if (length(error) > 0) {
  print(error)
}


