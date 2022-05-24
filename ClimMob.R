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
reference   <- args[11] # the reference item for the analysis
minN        <- args[12] # minimum n of complete data required in a trait evaluation before it is excluded
minitem     <- args[13] # minimum n of items tested, e.g. that all items are tested at least twice
mincovar    <- args[14] # minimum proportion of covariates compared to total valid n
sig_level   <- args[15] # significance level for the standard PL model
sig_level_tree  <-  args[16] # significance level for the tree
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

# method for adjustments for confidence intervals and setting widths for comparison. 
ci_adjust <- "BH"

# confidence interval level for comparison plots with error bars
ci_level <- 0.84

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
library("GGally")
source(paste0(fullpath, "/modules/01_functions.R"))

# Two objects to begin with that will be used to verify the process
error <- NULL
done <- TRUE

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
  cmdata <- as.data.frame(cmdata, tidynames = FALSE, pivot.wider = TRUE)
  
  dir.create(outputpath, showWarnings = FALSE, recursive = TRUE)
  
  source(paste0(fullpath, "/modules/02_organize_ranking_data.R"))
  
  rank_dat <- organize_ranking_data(cmdata, 
                                    pars, 
                                    groups = groups, 
                                    option = "technology",
                                    ranker = "participant",
                                    tech_index = c("package_item_A", "package_item_B", "package_item_C"))
  
  if (length(rank_dat) == 0) {
    rmarkdown::render(paste0(fullpath, "/report/mainreport_no_traits.Rmd"),
                      output_dir = outputpath,
                      output_format = "word_document",
                      output_file = paste0("climmob_main_report.docx"))
    quit()
  }
  
}, error = function(cond) {
    return(cond)
  }
)

if (any_error(try_data)) {
  e <- paste("Organize Ranking Data: \n", try_data$message)
  error <- c(error, e)
  done <- FALSE
}

# ................................................................
# ................................................................
# 2. Organise the rankings ####
try_quanti_data <- tryCatch({
  
  source(paste0(fullpath, "/modules/03_organize_quantitative_data.R"))
  
  if (isTRUE(length(pars$linear) > 0)) {
    quanti_dat <- organize_quantitative_data(cmdata, 
                                             pars, 
                                             groups = groups, 
                                             tech_index = c("package_item_A", "package_item_B", "package_item_C"))
  }else{
    quanti_dat <- NULL
  }
  
  
  
}, error = function(cond) {
  return(cond)
}
)

if (any_error(try_quanti_data)) {
  e <- paste("Organize Quantitative Data: \n", try_quanti_data$message)
  error <- c(error, e)
  done <- FALSE
}

# .......................................................
# .......................................................
# 3. Prepare summary tables / charts
org_summ <- tryCatch({
 
  source(paste0(fullpath, "/modules/04_overview_and_summaries.R"))
  
  overview_and_summaries <- get_overview_summaries(cmdata, rank_dat)
  
  
}, error = function(cond) {
  return(cond)
}
)

if (any_error(org_summ)) {
  error <- c(error, org_summ$message)
  overview_and_summaries <- list(partipation_plot = 0L,
                                 summary_table_trait = data.frame(),
                                 summary_table_tech = idata.frame(),
                                 trial_connectivity = 0L)
}

# .......................................................
# .......................................................
# 4. Make map ####
org_lonlat <- tryCatch({
  
  source(paste0(fullpath, "/modules/05_spatial_overview.R"))
  
  trial_map <- make_trial_map(cmdata, output_path = outputpath)
  
  
}, error = function(cond) {
  return(cond)
}
)

if (any_error(org_lonlat)) {
  e <- org_lonlat$message
  error <- c(error, e)
  trial_map <- list(geoTRUE = FALSE,
                    map_path = "")

}

# .......................................................
# .......................................................
# .......................................................
# 7. Fit PlackettLuce model ####
# This will use the rankings from each trait 
# to fit a PlackettLuce model and do
# some exploratory analysis
# and a table showing statistical differences for each trait 
org_pl <- tryCatch({
  
  source(paste0(fullpath, "/modules/06_PlackettLuce_by_traits.R"))
  
  PL_models <- get_rank_models(cmdata, rank_dat, reference)
  
  
}, error = function(cond) {
  return(cond)
}
)

if (any_error(org_pl)) {
  e <- org_pl$message
  error <- c(error, e)
  
  PL_models <- list(PL_models = list(),
                    PL_models_overview = data.frame(),
                    logworth_aggregated_rank = 0L,
                    worthmap = 0L,
                    logworth_plot = 0L,
                    kendall = list(isKendall = FALSE,
                                   strongest_link = "", 
                                   weakest_link = "",
                                   kendall_plot = 0L))
  
}

# .......................................................
# .......................................................
# 7. Fit PLADMM model ####
# this will try to fit a simple PLDMM for the overall trait
# using the log-worth from the other traits as a reference
org_pladmm <- tryCatch({
  
  sa
  
}, error = function(cond) {
  return(cond)
}
)

if (any_error(org_pladmm)) {
  
  isPLADMM <- FALSE

}


# .......................................................
# .......................................................
# 8. Fit pltree ####
# organize covariates and prepate to fit pltree()
# outputs of pltree(), if any, are also processed for further insights
org_pltree <- tryCatch({
  ss
}, error = function(cond) {
  return(cond)
}
)

if (any_error(org_pltree)) {
  
  e <- org_pltree$message
  
  error <- c(error, e)
  
  isTREE <- FALSE
  
}


# ................................................................
# ................................................................
# 9. Write outputs ####
#determine format based on extensions
output_format <- ifelse(extension == "docx","word_document", 
                        paste0(extension,"_document"))


if (all(infosheets, done)) {
  
  try_infosheet <- tryCatch({
    
    # table with the worth parameters from the reference trait
    # number of times each item was tested
    # and how many times it was ranked first or last
    # check if items have more than 20 characters and make it shorter
    cmdata[, itemnames] <- lapply(cmdata[, itemnames], function(x){
      abbreviate(x, minlength = 20)
    })
    
    # make the rank without the Local item
    overall <- trait_list[[reference_trait]]
    # take the question asked
    question_asked <- overall$question
    # the vector to filter the data
    keep <- overall$keep
    
    # multilanguage text
    reporttext <- read.csv(paste0(fullpath, 
                                  "/report/participant_report_multilanguage_text.csv"))
    
    
    # pick the vector with the language
    reporttext <- reporttext[, c(1, match(language, names(reporttext)))]
    
    # replace fields with info from ClimMob
    notreplied <- reporttext[match("notreplied", reporttext[,1]), 2]
    rranker     <- reporttext[match("partictag", reporttext[,1]), 2]
    rrankers    <- reporttext[match("partictag2", reporttext[,1]), 2]
    roption     <- reporttext[match("techtag", reporttext[,1]), 2]
    roptions    <- reporttext[match("techtag2", reporttext[,1]), 2]
    
    reporttext[,2] <- gsub("r rankers", rrankers, reporttext[,2])
    reporttext[,2] <- gsub("r nranker", nranker, reporttext[,2])
    reporttext[,2] <- gsub("r question_asked", question_asked, reporttext[,2])
    reporttext[,2] <- gsub("r nitems", nitems, reporttext[,2])
    reporttext[,2] <- gsub("r options", roptions, reporttext[,2])
    reporttext[,2] <- gsub("r nothertraits", nothertraits, reporttext[,2])
    reporttext[,2] <- gsub("r ncomp", ncomp, reporttext[,2])
    
    # list of arguments for the function that will be used to 
    # create the rankings
    a <- list(cmdata[keep, ],
              items = itemnames,
              input = overall$strings, 
              full.output = TRUE)
    
    R <- do.call(rankwith, args = a)
    
    order_items <- coef(PlackettLuce(R[["PLranking"]]), ref = reference)
    
    # do this to remove ties
    order_items <- order_items[names(order_items) %in% items]
    
    rank_items <- gosset:::.rank_decimal(order_items)$rank
    
    order_items <- names(order_items)
    
    freq_items <- table(unlist(R[["myrank"]]))
    
    ordering <- R[["myrank"]]
    
    first_items <- table(ordering[,1])
    last_items  <- table(ordering[,3])
    
    infotable <- data.frame(item = order_items,
                            rank = rank_items,
                            freq = as.vector(freq_items[order_items]),
                            first = as.vector(first_items[order_items]),
                            last = as.vector(last_items[order_items]))
    
    infotable[is.na(infotable)] <- 0
    
    infotable <- infotable[order(infotable$rank), ]
    # ................................................................
    # ................................................................
    # Get the info from the participants ####
    sel <- c("id", itemnames)
    partitable <- cmdata[, sel]
    
    partitable$name <- cmdata[,which(grepl("package_participant_name|package_farmername", names(cmdata)))]
    
    #names(partitable) <- gsub("package_|farmer", "", names(partitable))
    
    # empty matrix to expand values from ord so it can fit partitable
    # in case of missing data when participants did not replied the reference trait
    x <- matrix(NA, 
                ncol = ncomp, 
                nrow = length(cmdata$id),
                dimnames = list(seq_along(cmdata$id), paste0("Position", 1:ncomp)))
    
    partitable <- cbind(partitable, x)
    
    partitable[keep, paste0("Position", 1:ncomp)] <- ordering
    
    # fill NAs with "Not replied" in the first case and then with an empty character
    partitable$Position1[is.na(partitable$Position1)] <- notreplied
    partitable[is.na(partitable)] <- ""
    
    # ................................................................
    # ................................................................
    # If any other trait, do the same ####
    if(isTRUE(nothertraits > 0)){
      
      otr_list <- trait_list[-reference_trait]
      
      otr <- list()
      
      otrnames <- lapply(otr_list, function(x){
        x$name
      })
      
      otrnames <- as.vector(unlist(otrnames))
      
      for(i in seq_along(otr_list)){
        
        a <- list(cmdata[otr_list[[i]]$keep, ],
                  items = itemnames,
                  input = otr_list[[i]]$strings,
                  full.output = TRUE)
        
        R <- do.call(rankwith, args = a)[["myrank"]]
        
        # expand the rankings (in rows) so it can fit with the full
        # information to include those participants who did not replied the
        # question 
        Rexp <- matrix(NA, 
                       nrow = nrow(partitable), 
                       ncol = ncomp, 
                       dimnames = list(partitable$id, 
                                       paste0("Position", 1:ncomp)))
        
        Rexp[otr_list[[i]]$keep, ] <- R
        
        R <- Rexp
        
        R[is.na(R[, 1]), 1] <- notreplied
        
        R[is.na(R)] <- ""
        
        otr[[i]] <- R
        
      }
      
      # now put all together by participants ids
      otrp <- list()
      for(i in  seq_along(partitable$id)){
        
        x <- NULL
        
        # combine (by rows) the response for the participant i 
        # across all the j other traits
        for(j in seq_along(otr_list)){
          
          x <- rbind(x, otr[[j]][i, ])
          
        }
        
        
        # add the question that was made
        x <- cbind(Trait = otrnames, 
                   x)
        
        # add the reference trait at the top of the table
        x <- rbind(unlist(c(overall$name, partitable[i, paste0("Position", 1:ncomp)])),
                   x)
        
        x <- as.data.frame(x)
        
        # change names of order based on the number of comparisons 
        # used in the trail
        if (isTRUE(ncomp == 3)){
          nmx <- reporttext[match("tabletitle", reporttext[,1]), 2]
          nmx <- strsplit(nmx, ";")[[1]]
          names(x) <- nmx
        }
        
        if (isTRUE(ncomp > 3)) {
          names(x) <- c("Trait", paste("Position", 1:ncomp))
        }
        
        otrp[[i]] <- x
        
      }
      
    }
    
    # use the coefficients from the reference trait model and plot it as bar plot
    # to show the overall evaluation compared to the farmer evaluation
    pover <- worth_plot[[reference_trait]]
      
    # make a template of ggplot to assemble a podium
    podium <- data.frame(label = factor(c("1st", "2nd", "3rd"), levels = c("2nd", "1st", "3rd")),
                         values = (3:1))
    
    ggpodium <- 
      ggplot(data = podium, 
             aes(y = values, x = label, fill = label)) +
      geom_bar(stat = "identity", position = "dodge", show.legend = FALSE) +
      labs(x = "", 
           y = "") +
      scale_fill_manual(values = c("#C0C0C0", "#FFD700", "#cd7f32")) +
      theme(element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(size = 8),
            panel.background = element_blank(),
            plot.margin=grid::unit(c(0,0,0,0), "mm"))
    
    
    rmarkdown::render(paste0(fullpath, "/report/participant_report_main.Rmd"),
                      output_dir = outputpath,
                      output_format = output_format,
                      output_file = paste0("participants_report", ".", extension))
    
    
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
  
  
  # resolution of display items
  dpi <- 400
  out_width <- "100%"
  
  # define height of plots based on items
  worthmap_h <- ntrait
  worthmap_w <- ntrait + 0.5
  favplot_h <- nitems * 0.4
  
  if (worthmap_h < 7)  worthmap_h <- 7
  if (worthmap_h > 8)  worthmap_h <- 8
  if (worthmap_w < 7)  worthmap_w <- worthmap_h + 0.5
  if (worthmap_w > 8)  worthmap_w <- worthmap_h + 0.5
  if (favplot_h < 5) favplot_h <- 5
  if (favplot_h > 8) favplot_h <- 7.5
  
  # the main report
  try_rep <- tryCatch({
    rmarkdown::render(paste0(fullpath, "/report/mainreport.Rmd"),
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
  rmarkdown::render(paste0(fullpath, "/report/mainreport_failed.Rmd"),
                    output_dir = outputpath,
                    output_format = "word_document",
                    output_file = paste0("climmob_main_report.docx"))
}

if (length(error) > 0) {
  print(error)
}

# End of analysis


