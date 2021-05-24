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
reference   <- args[10] # the reference item for the analysis
if (isTRUE(is.na(reference))) {
  reference <- 1
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
source(paste0(fullpath, "/R/functions.R"))

# Two objects to begin with that will be used to verify the process
error <- NULL
done <- TRUE

# ................................................................
# ................................................................
# Read data #### 
# with selected traits and explanatory variables to be analysed
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
  
  projname <- cmdata[1, "package_project_name"]
  
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
# write output directory
tryCatch({
  dir.create(outputpath, showWarnings = FALSE, recursive = TRUE)
})

# ................................................................
# ................................................................
# Dataset parameters ####
dtpars <- tryCatch({
  # Get some info from the data and imputed parameters 
  Option       <- ClimMobTools:::.title_case(option)
  options      <- ClimMobTools:::.pluralize(option)
  rankers      <- ClimMobTools:::.pluralize(ranker)
  nranker      <- nrow(cmdata)
  items        <- cmdata[, grepl("package_item", names(cmdata))]
  itemnames    <- names(items)
  items        <- unique(sort(unlist(items)))
  nitems       <- length(unique(sort(unlist(items))))
  covar        <- pars$covariates
  covarTRUE    <- isTRUE(length(covar) > 0)
  ntrait       <- dim(pars$traits)[[1]]
  nothertraits <- ntrait - 1
  ncomp        <- length(itemnames)
  nquest       <- pars$traits$nQst[1]
  # select which function will be used to create the Plackett-Luce rankings
  # it will depend on how many items each participant compares
  if (ncomp == 3) {
    rankwith <- "rank_tricot"
  }
  
  if (ncomp > 3) {
    rankwith <- "rank_numeric"
  }
  
  # minimum n of complete data required in a trait evaluation
  # before it is excluded, it can be at least 5 or 
  # that all items are tested at least twice
  missper <- 5
  minitem <- 2
  
  # minimum proportion of valid entries in explanatory variables
  missexp <- 0.95
  
  # minimum proportion of valid entries in tricot vs local
  # this will be computed based on the valid entries of the reference
  # trait after validations
  mintricotVSlocal <- 0.95
  
  # minimum split size for PL Tree models
  minsplit <- ceiling(nranker * 0.1)
  if (isTRUE(minsplit < 10)) {
    minsplit <- 10
  }
  
  if (isFALSE(is.numeric(minsplit))) {
    minsplit <- 10
  }
  
  # Set alpha
  sig_level <- 0.1
  sig_level_tree <- 0.5
  
  # method for adjustments for confidence intervals and setting widths for comparison. 
  ci_adjust <- "none"
  
  # confidence interval level for comparison plots with error bars
  ci_level <- 0.84
  
  # resolution of display items
  dpi <- 250
  out_width <- "100%"
  
}, error = function(cond) {
  return(cond)
}
)

if (any_error(dtpars)) {
  e <- paste("Error 103.", dtpars$message)
  error <- c(error, e)
  done <- FALSE
}

# ................................................................
# ................................................................
# Organise the rankings ####
# This is to check for missing data and create a vector
# for each characteristic that will be used to filter 
# inconsistent data
org_rank <- tryCatch({
  
  trait <- pars$traits
  # rename traits to avoid duplicated strings, in case the same 
  # trait is tested in different data collection moments 
  trait$codeQst <- rename_duplicates(trait$codeQst)
  trait$name    <- rename_duplicates(trait$name, sep = " ")
  
  tricotVSlocal <- length(pars$tricotVSlocal) > 0
  
  # get a vector with trait names matched with the assessment id
  # do this because the same trait can be assessed in different data collection moments
  trait_names <- trait$codeQst
  
  # find the index for the reference trait, the one that is going to be used 
  # as main trait for the analysis
  reference_trait <- which(trait$traitOrder == "referenceTrait")
  
  # this list will keep the logical vectors to filter the data per 
  # remaining trait, the ones that passed the evaluation on minimal 
  # amount of missing data 
  trait_list <- list()
  
  # make a vector to register any possible trait to be dropped due to few 
  # data availability
  trait_dropped <- character()
  
  # run over traits to filter NAs and prepare for PlackettLuce rankings
  for(i in seq_along(trait_names)){
    
    result <- list()
    
    # get the question full name
    trait_i <- as.character(trait[i, paste0("nameString", seq_len(nquest))])
    
    # look for it in cmdata
    for(j in seq_along(trait_i)) {
      
      trait_i[j] <- names(cmdata[which(grepl(trait_i[j], names(cmdata)))])
      
    }
    
    # replace Not observed entries with NA
    rpl <- cmdata[trait_i]
    rpl[rpl == "Not observed"] <- NA
    cmdata[trait_i] <- rpl
    
    # if all the three items are tied than set this entry as NA
    # as it results in issues on PlackettLuce its S3 methods
    tied <- as.vector(apply(cmdata[trait_i], 1, function(x) {
      all(x == "Tie") & all(!is.na(x))
    }))
    
    cmdata[tied, trait_i] <- NA
    
    # check for data completeness in this trait
    # should return a vector with TRUE, FALSE, 
    # where TRUE = complete, FALSE = missing
    keep <- apply(cmdata[trait_i], 1, is.na)
    keep <- as.vector(colSums(keep) == 0)
    
    # check if number of missing data is lower than the threshold
    # at least that the number of valid entries is higher than missper (5)
    cond1 <- sum(keep) > missper
    
    # at least that all the items are tested twice
    cond2 <- all(table(unlist(cmdata[keep,itemnames])) > 1)
    
    keepit <- all(cond1, cond2)
    
    # if lower than missper it will be dropped
    if (isFALSE(keepit)) {
      trait_dropped <- c(trait_dropped, trait$name[i])
      next
    } 
    
    # if required, add comparison with local item
    if (isTRUE(i == reference_trait & tricotVSlocal)) {
      
      # some times the user can add this question twice, I still don't have a solution for 
      # this, so I will use the one that is related to overallperf
      indexO <- which(grepl("_overalperf", pars$tricotVSlocal$nameString1))
      
      if (length(indexO) != 1) {
        indexO <- 1
      }
      
      ovsl <- as.vector(pars$tricotVSlocal[indexO, paste0("nameString", seq_len(pars$tricotVSlocal$nQst[indexO]))])
      
      # search for the columns in the data
      for (k in seq_along(ovsl)) {
        
        ovsl[k] <- names(cmdata[which(grepl(ovsl[k], names(cmdata)))])
        
      }
      
      ovsl <- as.character(ovsl)
      
      # check for data completeness
      keep2 <- apply(cmdata[ovsl], 1, is.na)
      keep2 <- as.vector(colSums(keep2) == 0)
      
      # combine with the vector from the first iteration in the i loop
      # which is the overall performance
      keep2 <- keep & keep2
      
      # check if it has the minimal number of observations
      # this should be combined with the available data in the reference
      # trait as they will be filtered together
      dropit2 <- sum(keep2 & keep) / sum(keep) < mintricotVSlocal
      
      # if is below the minimal number of observations then the comparison
      # with the local item is ignored
      if (isTRUE(dropit2)) {
        trait_dropped <- c(trait_dropped, "Comparison with local")
        tricotVSlocal <- FALSE
      }
      
      result[["keep2"]] <- keep2
      
      result[["tricotVSlocal"]] <- ovsl
      
    }
    
    # logical vector to subset the data 
    result[["keep"]]       <- keep  
    # string names to find the data in cmdata
    result[["strings"]]    <- trait_i
    # the trait name
    result[["name"]]       <- trait$name[i]
    # the trait code
    result[["code"]]       <- trait$codeQst[i]
    # the questions that were asked 
    result[["question"]]   <- paste(trait[i, paste0("questionAsked", seq_len(nquest))], collapse = ", ")
    # the data collection moment
    result[["assessment"]] <- trait$assessmentName[i]
    # days after the beginning of the trail for the data collection moment
    result[["day"]]        <- trait$assessmentDay[i]
    # the assessment id
    result[["assessmentid"]] <- trait$assessmentId[i]
    
    trait_list[[trait_names[i]]] <- result
    
  }
  
  # if all the traits were removed then make a report and stop the process here 
  if (all(trait$name %in% trait_dropped)) {
    rmarkdown::render(paste0(fullpath, "/report/mainreport_no_traits.Rmd"),
                      output_dir = outputpath,
                      output_format = "word_document",
                      output_file = paste0("climmob_main_report.docx"))
    quit()
  }
  
  # refresh the number of traits
  ntrait <- length(trait_list)
  
  # refresh the index for the reference trait in case it changed due to 
  # dropped trait
  reference_trait <- paste(trait[reference_trait, c("codeQst","assessmentId")], collapse = "")
  reference_trait <- which(names(trait_list) %in% reference_trait)
  
  # if for some reason the reference trait was dropped, than take the last one
  if (length(reference_trait) == 0) {
    reference_trait <- length(trait_list) 
    # and coerce tricotVSlocal to FALSE to prevent errors in matching strings
    tricotVSlocal <- FALSE
  }
  
  # get the reference trait as a separated list
  # overall <- trait_list[[reference_trait]]
  # get the name of the reference trait both in lower and title case
  ovname <- tolower(trait_list[[reference_trait]]$name)
  Ovname <- ClimMobTools:::.title_case(trait_list[[reference_trait]]$name)
  
  # the name of the other traits
  # other_traits <- names(trait_list)[-reference_trait]
  
  # and the other traits as a separated list
  # other_traits_list <- trait_list[-reference_trait]
  
  # the name of other traits combined with the name of assessments
  traits_names <- as.vector(unlist(lapply(trait_list, function(x) {
    paste0(x$name, " [", x$assessment, "]")
  })))
  
  code_trait <- as.vector(unlist(lapply(trait_list, function(x) {
    x$code
  })))
  
  code_trait <- gsub("[[:punct:]]", "", code_trait)
  
  # refresh number of other traits
  nothertraits <- length(trait_list) - 1
  
}, error = function(cond) {
  return(cond)
}
)

if (any_error(org_rank)) {
  e <- paste("Error 104.", org_rank$message)
  error <- c(error, e)
  done <- FALSE
}

# .......................................................
# .......................................................
# Organise the covariates ####
# if no variable is provided than add a pseudo variable that will be used 
# in pltree(), this is to fit the model with the intercept only
org_covar <- tryCatch({
  
  if (isFALSE(covarTRUE)) {
    
    cmdata$Intercept <- rep(0, nranker)
    
    overall$keep_covariate <- rep(TRUE, nranker)
    
    covar <- data.frame(codeQst = "xinterceptx", 
                        nameString = "Intercept",
                        name = "Intercept-only model",
                        questionAsked = "",
                        assessmentName = "",
                        assessmentId = "")
  }
  
  if (isTRUE(covarTRUE)) {
    
    # rename covariates to avoid duplicated strings, in case the same 
    # question was made in different data collection moments 
    covar$codeQst <- rename_duplicates(covar$codeQst)
    covar$name    <- rename_duplicates(covar$name, sep = " ")
    
    
    # add the string $ to indicate the end of pattern
    strings <- paste0(covar$nameString, "$")
    
    # check for the full names
    for(i in seq_along(covar$nameString)){
      covar$nameString[i] <- names(cmdata[which(grepl(strings[i], names(cmdata)))])
    }
    
    strings <- covar$nameString
    
    # check for missing data
    keep <- NULL
    for(i in seq_along(strings)){
      k <- !is.na(cmdata[, strings[i]])
      keep <- cbind(keep, k)
    }
    
    # find those that are above the threshold of missexp
    dropit <- missexp > (colSums(keep) / nranker)
    
    # drop those bellow threshold
    keep <- as.data.frame(keep[, !dropit])
    
    # create a single vector that will be used to filter cmdata
    keep <- rowSums(keep)
    keep <- keep == max(keep)
    
    covar_dropped <- covar$name[dropit]
    covar <- covar[!dropit, ]
   
    # if no covariate left out put a pseudo variable
    if(isTRUE(dim(covar)[[1]] == 0)) {
      
      cmdata$Intercept <- rep(0, nranker)
      
      trait_list[[reference_trait]]$keep_covariate <- rep(TRUE, nranker)
      
      covar <- data.frame(codeQst = "xinterceptx", 
                          nameString = "Intercept",
                          name = "Intercept-only model",
                          questionAsked = "",
                          assessmentName = "",
                          assessmentId = "")
      
      }else{
        trait_list[[reference_trait]]$keep_covariate <- keep
    }
    
  }
}, error = function(cond) {
  return(cond)
}
)

if (any_error(org_covar)) {
  
  e <- paste("Error 105.", org_covar$message)
  
  error <- c(error, e)
  
  cmdata$Intercept <- rep(0, nranker)
  
  trait_list[[reference_trait]]$keep_covariate <- rep(TRUE, nranker)
  
  covar <- data.frame(codeQst = "xinterceptx", 
                      nameString = "Intercept",
                      name = "Intercept-only model",
                      questionAsked = "",
                      assessmentName = "",
                      assessmentId = "")
  
}

# .......................................................
# .......................................................
# Make map ####
org_lonlat <- tryCatch({
  
  # Check if lonlat is provided
  lon <- grepl("_longitude", names(cmdata))
  lat <- grepl("_latitude", names(cmdata))
  
  geoTRUE <- all(any(lon), any(lat))
  
  if (isTRUE(geoTRUE)) {
    
    # find the vector with most completeness 
    less_nas <- lapply(cmdata[lon], function(x){
      sum(is.na(x))
    })
    
    lon <- names(which.min(unlist(less_nas)))
    lat <- gsub("_longitude", "_latitude", lon)
    
    lonlat <- cmdata[,c(lon,lat)]
    
    lonlat <- na.omit(lonlat)
    
    nlonlat <- dim(lonlat)[[1]]
    
    if (nlonlat > 0){
      
      trial_map <- plot_map(lonlat, xy = c(1, 2), minimap = TRUE, 
                            map_provider = "OpenStreetMap.Mapnik")
      
      mapshot(trial_map, 
              url = paste0(outputpath, "/", projname, "_trial_map.html"),
              file = paste0(outputpath, "/", projname, "_trial_map.png"))

    }
    
    if (nlonlat == 0) {
      geoTRUE <- FALSE
    }
    
  }
  
  if (isFALSE(geoTRUE)) {
    
    trial_map_statement <- ""
    
  }  
}, error = function(cond) {
  return(cond)
}
)

if (any_error(org_lonlat)) {
  e <- paste("Error 106.", org_lonlat$message)
  error <- c(error, e)
  geoTRUE <- FALSE
  trial_map_statement <- ""
}

# .......................................................
# .......................................................
# Make frequencies of item evaluation #### 
# This is a table, disaggregated by gender showing how many times
# each item was tested 
try_freq_tbl <- tryCatch({
  
  itemdata <- cmdata[, grepl("package_item", names(cmdata))]
  
  itemtable <- data.frame(table(unlist(itemdata)))
  
  itemtable$x <- with(itemtable,
                      round((Freq / nranker * 100), 1))
  
  itemtable$x <- with(itemtable,
                      paste0(x, "%"))
  
  names(itemtable) <- c(Option, "Freq", "Relative freq")
  
  gender_string <- "registration_gender|REG_gender"
  
  # check if gender is provided so it can be added to the itemtable
  gender <- any(grepl(gender_string, names(cmdata)))
  
  if (isTRUE(gender)) {
    dt <- unlist(itemdata)
    
    gender_i <- which(grepl(gender_string, names(cmdata)))[[1]]
    gender_i <- cmdata[, gender_i]
    gender_i <- ifelse(is.na(gender_i), "Unstated", gender_i)
    
    gendrs <- unique(gender_i)
    
    ngendrs <- rep(NA, length(gendrs))
    
    for (i in seq_along(gendrs)) {
      ngendrs[i] <- sum(gender_i == gendrs[i], na.rm = TRUE)
    }
    
    gendertbl <- c()
    
    for (i in seq_along(gendrs)) {
      gendertbl <- cbind(gendertbl, 
                         tapply(rep(gender_i, ncomp), dt, function(x) sum(x == gendrs[i], na.rm = TRUE)))
    }
    
    gendertbl <- as.data.frame(gendertbl)
    
    names(gendertbl) <- paste0(gendrs, " (n=", ngendrs, ")")
    
    itemtable <- cbind(itemtable, gendertbl)
    
    rm(dt)
    
  }
  
  itemtable$Abbreviation <- gosset:::.reduce(as.character(itemtable[, Option]))
  
  itemtable <- itemtable[union(c(Option, "Abbreviation"), names(itemtable))]
  
}, error = function(cond) {
  return(cond)
}
)

if (any_error(try_freq_tbl)) {
  e <- paste("Error 107.", try_freq_tbl$message)
  error <- c(error, e)
  itemtable <- data.frame()
}

# .......................................................
# .......................................................
# Favourability Analysis ####
# first for overall performance
try_fav <- tryCatch({
  
  fav_traits <- list()
  
  vic_traits <- list()
  
  dom_traits <- list()
  
  for (i in seq_along(trait_list)) {
    
    keep <- trait_list[[i]]$keep
    
    # list of arguments for the function that will be used to 
    # create the rankings
    a <- list(cmdata[keep, c(itemnames, trait_list[[i]]$strings)],
              items = itemnames,
              input = trait_list[[i]]$strings)
    
    R <- do.call(rankwith, args = a)
    
    # get the network of items evaluated in the project
    if (i == reference_trait) {
      
      net <- network(R)
      
      if (isTRUE(tricotVSlocal)) {
        
        keep <- trait_list[[i]]$keep2 & trait_list[[i]]$keep
        
        a <- list(cmdata[keep, c(itemnames, 
                                 trait_list[[i]]$strings, 
                                 trait_list[[i]]$tricotVSlocal)],
                  items = itemnames,
                  input = trait_list[[i]]$strings,
                  additional.rank = cmdata[keep, trait_list[[i]]$tricotVSlocal])
        
        R <- do.call(rankwith, args = a)
        
      }
      
    }
    
    fav1_i <- summarise_favorite(R)
    fav2_i <- fav1_i
    
    fav2_i$best <- paste0(round(fav2_i$best, 1), "%")
    fav2_i$worst <- paste0(round(fav2_i$worst, 1), "%")
    fav2_i$fav_score <- round(fav2_i$fav_score, 1)
    
    fav2_i <- fav2_i[,-which(grepl("wins", names(fav2_i)))]
    
    names(fav2_i) <- c(Option,"N","Top ranked","Bottom ranked",
                       "Net favorability score")
    
    fav_traits[[i]] <- list(fav1_i, fav2_i)
    
    vic_traits[[i]] <- summarise_victories(R)
    
    dom_traits[[i]] <- summarise_dominance(R)
    
  }
  
}, error = function(cond) {
  return(cond)
}
)

if (any_error(try_fav)) {
  e <- paste("Error 108.", try_fav_ot$message)
  error <- c(error, e)
  done <- FALSE
}

# .......................................................
# .......................................................
# Agreement analysis ####
# this assess how the other traits agreed with the overall preference
# build rankings for the other characteristics
try_agree <- tryCatch({
  
  if (isTRUE(nothertraits > 0)) {
    
    # filter cmdata so it matches the dims in all traits
    keep <- NULL
    
    for(i in seq_along(trait_list)) {
      keep <- cbind(keep, trait_list[[i]]$keep)
    }
    
    keep <- rowSums(keep)
    keep <- keep == length(trait_list)
    
    # list of rankings
    compare_with <- list()
    
    for (i in seq_along(trait_list)) {
      
      a <- list(cmdata[keep, ],
                items = itemnames,
                input = trait_list[[i]]$strings)
      
      otr <- do.call(rankwith, args = a) 
      
      compare_with[[i]] <- otr
      
    }
    
    agreement <- summarise_agreement(compare_with[[reference_trait]], 
                                     compare_with[-reference_trait], 
                                     labels = traits_names[-reference_trait])
    
    strongest_link <- c(agreement[[which.max(agreement$kendall), "labels"]],
                        round(max(agreement$kendall), 0))
    
    
    weakest_link   <- c(agreement[[which.min(agreement$kendall), "labels"]],
                        round(min(agreement$kendall), 0))
    
    
    agreement_table <- agreement
    
    agreement_table[,c(2:4)] <- lapply(agreement_table[,c(2:4)], function(x){
      x <- round(x, 1)
      x <- paste0(x,"%")
    })
    
    names(agreement_table) <- c("Trait", 
                                "Complete ranking agreement (Kendall tau)",
                                "Agreement with best", 
                                "Agreement with worst")
    
    
  } 
  
  if (isTRUE(nothertraits == 0)) {
    strongest_link <- character()
    weakest_link <- character()
    agreement_table <- data.frame(Option = paste("Only", ovname, "was used"),
                                  X = "",
                                  Y = "",
                                  Z = "")
    names(agreement_table) <- c(" ","","  ","   ")
    agreement <- data.frame(labels = "",
                            kendall = 0,
                            first = 0,
                            last = 0)
    
    class(agreement) <- union("gosset_agree", class(agreement))
  }
  
}, error = function(cond) {
  return(cond)
}
)

if (any_error(try_agree)) {
  e <- paste("Error 109.", try_agree$message)
  error <- c(error, e)
  
  strongest_link <- character()
  weakest_link <- character()
  agreement_table <- data.frame(Option = "",
                                X = "",
                                Y = "",
                                Z = "")
  names(agreement_table) <- c(" ","","  ","   ")
  agreement <- data.frame(labels = "",
                          kendall = 0,
                          first = 0,
                          last = 0)
  
  class(agreement) <- union("gosset_agree", class(agreement))
}

# .......................................................
# .......................................................
# PlackettLuce Model ####
try_pl <- tryCatch({
  
  # Run over the other traits
  mods <- list()
  summaries <- list()
  plot_summaries <- list()
  worths <- list()
  anovas <- list()
  aov_tables <- list()
  
  for (i in seq_along(trait_list)) {
    
    keep <- trait_list[[i]]$keep
    
    # list of arguments for the function that will be used to 
    # create the rankings
    a <- list(cmdata[keep, c(itemnames, trait_list[[i]]$strings)],
              items = itemnames,
              input = trait_list[[i]]$strings)
    
    R <- do.call(rankwith, args = a)
    
    mod_i <- PlackettLuce(R)
    
    if (anova.PL(mod_i)[2,5] <= sig_level) {
      ci_adjust <- "none"
    }else{
      ci_adjust <- "BH"
    }
    
    mods[[i]] <- mod_i
    
    aov_i <- anova.PL(mod_i)
    
    # aov tables 
    anovas[[i]] <- aov_i
    
    # organise pvalues
    aov_i[,5] <- paste(formatC(aov_i[,5], format = "e", digits = 2),
                       stars.pval(aov_i[,5]))
    
    aov_i[2, "model"] <- trait_list[[i]]$name
    
    aov_tables[[i]] <- aov_i
    
    # table with multicomparison analysis 
    summ_i <- multcompPL(mod_i, 
                         ref = reference,
                         threshold = sig_level, 
                         adjust = ci_adjust)
    
    # and this is the chart with multicomparion analysis
    summ_i_plot <- 
      plot(summ_i, level = ci_level) + 
      theme_classic() +
      theme(axis.text.x = element_text(size = 10, color = "#000000"),
            axis.text.y = element_text(size = 10, color = "#000000"))
    
    summ_i <- summ_i[, c("term", "estimate","quasiSE","group")]
    
    names(summ_i) <- c(Option, "Estimate","quasiSE","Group")
    
    summaries[[i]] <- summ_i
    
    plot_summaries[[i]] <- summ_i_plot
    
    # This is Table 4.*.3
    worths_i <- rev(sort(coef(mod_i, log = FALSE)))
    worths_i <- data.frame(label = factor(names(worths_i),
                                          (names(worths_i))),
                           worth = worths_i,
                           prob = paste0(round(worths_i * 100, 1), "%"), 
                           check.names = FALSE)
    
    names(worths_i) <- c(Option, c("Worth","Win probability"))
    worths[[i]] <- worths_i
  }
  
  # .......................................................
  # .......................................................
  # Plackett-Luce combining traits together ####
  
  coefs <- numeric()
  for(i in seq_along(mods)){
    coefs <- cbind(coefs, 
                   scale(qvcalc(mods[[i]], ref = reference)[[2]]$estimate))
  }
  
  coefs <- as.data.frame(coefs)
  
  names(coefs) <- code_trait
  
  rownames(coefs) <- names(coef(mods[[1]]))

}, error = function(cond) {
  return(cond)
}
)

if (any_error(try_pl)) {
  e <- paste("Error 111.", try_pl$message)
  error <- c(error, e)
  coefs <- NULL
}

# .......................................................
# .......................................................
# Compute partial least squares ####
try_pls <- tryCatch({
  
  if (isTRUE(nothertraits > 0)) {
    
    # remove special characters to produce the formula
    fml <- paste(code_trait[reference_trait], " ~ ", 
                 paste(code_trait[-reference_trait], collapse = " + "))
    fml <- as.formula(fml)
    
    names(coefs) <- gsub("[[:punct:]]", "", names(coefs))
    
    m2 <- plsr(fml,
               data = coefs,
               validation = "LOO", 
               jackknife = TRUE)
    
    if (ncol(m2$projection) > 1) {
      
      arrowlabels <- unlist(lapply(other_traits_list, function(x){x$code}))
      
      arrows <- data.frame((m2$projection)[,1:2],
                           trait = arrowlabels,
                           x0 = 0, 
                           y0 = 0, 
                           stringsAsFactors = FALSE)
      scores <- data.frame((m2$scores)[,1:2],
                           var = rownames(m2$scores))
    }
    
    
    yve <- drop(R2(m2, 
                   estimate = "train",
                   intercept = FALSE)$val)
    
    adjCV <- m2$validation$adj
    
    nc <- which(adjCV == min(adjCV))
  }
  
  if (isTRUE(dim(arrows)[[1]] > 0)) {
    
    scores$group <- "item"
    scores$label <- rownames(scores)
    
    arrows$group <- "char"
    arrows$label <- arrows$trait
    
    sco <- rbind(scores[,c("Comp.1","Comp.2","group","label")], 
                 arrows[,c("Comp.1","Comp.2","group","label")])
    
    plsplot <-
      ggplot(sco, 
           aes(x = Comp.1, y = Comp.2, label = label, group = group, color = group)) +
      geom_point() +
      geom_text_repel() +
      theme_bw() +
      scale_color_manual(values=c("#f03b20", "black")) +
      scale_x_continuous(expand = expansion(mult = 0.3)) +
      scale_y_continuous(expand = expansion(mult = 0.3)) +
      geom_segment(data = arrows, 
                   aes(x = Comp.1,
                       y = Comp.2,
                       label = other_traits_names,
                       xend = x0, 
                       yend = y0),
                   col = "#f03b20", arrow = arrow(length = unit(0.2, "cm"), ends = "first")) +
      geom_abline(linetype = 2, col = "gray50", slope = (yve[2] - yve[1]) / yve[1], intercept = 0) +
      geom_hline(yintercept = 0) + 
      geom_vline(xintercept = 0) +
      theme(panel.grid.minor = element_blank(),
            legend.position = "none") +
      labs(x = paste0("PC1 ", round(yve[1] * 100, 2), "%"),
           y = paste0("PC2 ", round((yve[2]-yve[1]) * 100, 2),"%"))
      
      
    
    
  }
  
}, error = function(cond) {
  return(cond)
}
)

if (any_error(try_pls)) {
  e <- paste("Error 112.", try_pls$message)
  error <- c(error, e)
  arrows <- data.frame()
  scores <- data.frame()
}

# .......................................................
# .......................................................
# Plackett-Luce trees with explanatory variables ####
try_plt <- tryCatch({
  
  if (isTRUE(tricotVSlocal)) {
    
    keep <- overall$keep2 & overall$keep_covariate
    
    a <- list(cmdata[keep, c(itemnames, overall$strings, overall$tricotVSlocal)],
              items = itemnames,
              input = overall$strings,
              additional.rank = cmdata[keep, overall$tricotVSlocal], 
              group = TRUE)
    
    G <- do.call(rankwith, args = a)
    
    
  } 
  
  if (isFALSE(tricotVSlocal)) {
    
    keep <- overall$keep & overall$keep_covariate
    
    a <- list(cmdata[keep, c(itemnames, overall$strings)],
              items = itemnames,
              input = overall$strings, 
              group = TRUE)
    
    G <- do.call(rankwith, args = a)
    
  }
  
  # data frame of explanatory variables
  Gdata <- as.data.frame(cmdata[keep, covar$nameString], stringsAsFactors = TRUE)
  nvar <- length(covar$nameString)
  
  # setup the variables, round numeric for 3 digits
  # and coerce the characters as factor
  Gdata[1:nvar] <- lapply(Gdata[1:nvar], function(x){
    if(is.numeric(x)) {
      x <- round(x, 3)
    }
    if(is.character(x)) {
      x <- as.factor(x)
    }
    return(x)
  })
  
  # rename covariates with the name taken from ClimMob
  names(Gdata) <- covar$codeQst
  Gdata <- cbind(G, Gdata)
  
  # perform a forward selection as pltree() sometimes don't split 
  # the tree when G ~ . is used
  var_keep <- character(0L)
  best <- TRUE
  counter <- 1
  exp_var <- covar$codeQst
  
  while (best) {
    
    fs <- length(exp_var)
    models <- data.frame()
    for(i in seq_len(fs)){
      t_i <- pltree(as.formula(paste0("G ~ ", paste(c(var_keep, exp_var[i]), collapse = " + "))),
                    data = Gdata,
                    minsize = minsplit,
                    alpha = sig_level_tree,
                    ref = reference,
                    gamma = TRUE)
      
      validations <- data.frame(nnodes = length(nodeids(t_i, terminal = TRUE)),
                                AIC = AIC(t_i),
                                noerror = !"try-error" %in% class(try(plot(t_i), silent = TRUE)))
      
      models <- rbind(models, validations)
      
    }
    
    counter <- counter + 1
    
    if (length(exp_var) == 0) {
      best <- FALSE
    }
    
    if (best) {
      # update vector with covariates to keep only those with no error
      # and those with no split
      exp_var <- exp_var[models$noerror & models$nnodes > 1]
      # also take out from the models data frame
      models <- models[models$noerror == TRUE & models$nnodes > 1, ]
      # find the index for the best model, the one with lowest AIC
      index_bext <- which.min(models$AIC)
      # and the best model
      best_model <- exp_var[index_bext]
      exp_var <- exp_var[-index_bext]
      var_keep <- c(var_keep, best_model)
    }
    
  }
  
  if (length(var_keep) == 0) {
    var_keep <- names(Gdata)[2]
    minsplit <- nrow(Gdata)
  }
  
  treeformula <- as.formula(paste0("G ~ ", paste(c(var_keep), collapse = " + ")))
  
  # now fit the tree with the selected covariates
  tree_f <- pltree(treeformula,
                   data = Gdata,
                   minsize = minsplit,
                   alpha = sig_level_tree,
                   ref = reference)
  
  # plot the tree (if any)
  plottree <- gosset:::plot_tree(tree_f)
  
  # if the tree has splits, extract coeffs from nodes
  if (isTRUE(length(tree_f) > 1)) { 
    
    node_ids <- nodeids(tree_f, terminal = TRUE)
    
    coefs_t <- NULL
    for(i in seq_along(node_ids)) {
      
      coef_i <- data.frame(node = node_ids[i],
                           rule = partykit:::.list.rules.party(tree_f, node_ids[i]),
                           multcompPL(tree_f[[ node_ids[i] ]]$node$info$object, 
                                      ref = reference,
                                      threshold = 0.05, adjust = ci_adjust),
                           n = tree_f[[ node_ids[i] ]]$node$info$nobs,
                           stringsAsFactors = FALSE)
      
      coefs_t <- rbind(coefs_t, coef_i)
      
    }
    
    # remove the strings %in% " and c() from rules 
    coefs_t$rule <- gsub("%in%","@", coefs_t$rule)
    coefs_t$rule <- gsub("[(]|[)]| c","", coefs_t$rule)
    
    coefs_t$Label <- paste("Node", coefs_t$node, ":", coefs_t$rule,"\n","n=",coefs_t$n)
    
    coefs_t <- split(coefs_t, coefs_t$node)
    
    coefs_t <- lapply(coefs_t, function(x){
      x$m <- mean(x$estimate)
      x$ctd <- x$estimate - x$m
      x
    })
    
    coefs_t <- do.call(rbind, coefs_t)
    
    rules <- unique(coefs_t$rule)
    best_tree <- NULL
    for(i in seq_along(rules)){
      
      tmp <- subset(coefs_t, rule == rules[i])
      
      best_tree <- rbind(best_tree,
                         c(tmp$n[1], 
                           paste(tmp$term[tmp$group %in% "a"], collapse=", "),
                           paste(rev(tmp$term[grepl(tmp$group[nrow(tmp)], tmp$group)]), collapse=", ")))
    }
    
    rules <- cbind("Covariate", rules)
    rules[,2] <- gsub("@", " Attribute ", rules[,2])
    rules[,2] <- gsub("&", " & Covariate ", rules[,2])
    rules <- paste(rules[,1], rules[,2])
    
    node_summary <- data.frame(rules, 
                               best_tree, 
                               stringsAsFactors = FALSE)
    
    names(node_summary) <- c("Split rule", "N", "Best ranked","Worst ranked")
    
  }
  
  # fitted 
  outtabs <- NULL
  for(j in seq_along(tree_f)){
    
    zzz <- nodeapply(tree_f, j, function(n){
      info_node(n)$test
    })[[1]]
    
    if (isTRUE(length(zzz) > 0)) {
      x <- data.frame(Node = j,
                      t(nodeapply(tree_f, j, function(n){
                        info_node(n)$test
                      })[[1]]))
      
      x$p.value <- round(x$p.value, 3)
      
      outtabs[[j]] <- x
      
      outtabs[[j]]$p <- format.pval(outtabs[[j]]$p.value)
    }else{
      outtabs[[j]] <- data.frame(Node = rep(j, length(covar$codeQst)), 
                                 statistic = rep(NA, length(covar$codeQst)),
                                 p.value = rep(NA, length(covar$codeQst)),
                                 stringsAsFactors = FALSE)
      
      rownames(outtabs[[j]]) <- covar$codeQst
      
    }
  }
  
  # Built table with p-values per node
  pval_nodes <- data.frame()
  
  for (i in seq_along(outtabs)) {
    if (ncol(outtabs[[i]]) > 3) {
      ot <- data.frame(Covariate = rownames(outtabs[[i]]),
                       outtabs[[i]])
      pval_nodes <- rbind(pval_nodes, ot)
    }
  }
  
  if (dim(pval_nodes)[[2]] > 0) {
    
    pval_nodes <- pval_nodes[,c("Covariate", "Node", "p.value")]
    
    row.names(pval_nodes) <- NULL
    
    # put bold in significant values 
    putbold <- which(pval_nodes$p <= sig_level)
    
    sig <- stars.pval(pval_nodes$p.value)
    
    pval_nodes$p.value <- paste(format.pval(pval_nodes$p.value), 
                                stars.pval(pval_nodes$p.value))
    
    for(i in seq_along(putbold)) {
      index_i <- putbold[i]
      
      pval_nodes[index_i, ] <- paste0("**", pval_nodes[index_i, ], "**")
      
    }
    
  }
  
}, error = function(cond) {
  return(cond)
}
)

if (any_error(try_plt)) {
  e <- paste("Error 113.", try_plt$message)
  error <- c(error, e)
  done <- FALSE
}

# ....................................................................
# ....................................................................
# Build headline summaries ####
try_head_summ <- tryCatch({
  
  ps <- fullanova[2, 5]
  
  bests <- as.character(model_summaries$term[grep("a", model_summaries$group)])
  
  if (isTRUE(length(bests) > 4)) {
    bests <- bests[1:4]
  }
  
  worsts <- as.character(rev(model_summaries$term[grepl(model_summaries$group[nrow(model_summaries)],
                                                        model_summaries$group)]))
  
  if(isTRUE(length(worsts) > 4)) {
    worsts <- worsts[1:4]
  }
  
  # avoid best and worst to be displayed in twice, this when the 
  # project tests a very few items < 6
  b <- bests[!bests %in% worsts]
  w <- worsts[!worsts %in% bests]
  
  bests <- paste(b, collapse =", ")
  worsts <- paste(w, collapse = ", ")
  
  if (isTRUE(nothertraits > 0)) {
    
    ps_ot <- numeric()
    bests_ot <- character()
    worsts_ot <- character()
    
    for(i in seq_along(anovas)){
      
      ps_i <- anovas[[i]][2,5]
      
      summ_i <- summaries[[i]]
      
      # take the best three items from this comparison
      bests_i <- as.character(summ_i[, Option][grepl("a", summ_i[,"Group"])])
      # if more than three, subset to get only three
      if (isTRUE(length(bests_i) > 3)) {
        bests_i <- bests_i[1:3]
      }
      
      # get the three worst items
      worsts_i <- as.character(rev(summ_i[grepl(summ_i[nrow(summ_i),"Group"], summ_i[, "Group"]), Option]))
      
      # if more than three, subset to get only three
      if (isTRUE(length(worsts_i) > 3)) {
        worsts_i <- worsts_i[1:3]
      }
      
      b <- bests_i[!bests_i %in% worsts_i]
      w <- worsts_i[!worsts_i %in% bests_i]
      
      bests_i <- paste(b, collapse =", ")
      worsts_i <- paste(w, collapse = ", ")
      
      bests_i <- paste(bests_i, collapse = ", ")
      
      worsts_i <- paste(worsts_i, collapse=", ")
      
      # put it together with the bests for overall performance
      bests_ot <- c(bests_ot, bests_i)
      
      worsts_ot <- c(worsts_ot, worsts_i)
      
      ps_ot <- c(ps_ot, ps_i)
      
    }
    
    bests <- c(bests_ot, bests)
    worsts <- c(worsts_ot, worsts)
    ps <- c(ps_ot, ps)
    
    ptabnames <- lapply(trait_list, function(x){c(x$name, x$assessment)})
    ptabnames <- do.call("rbind", ptabnames)
    
    # reorder table to put the reference trait in the right place
    
    
    ptab <- data.frame(Trait = ptabnames[,1],
                       "Data collection moment" = ptabnames[,2],
                       "Best ranked" = bests,
                       "Worst ranked" = worsts,
                       p.value = ps,
                       check.names = FALSE,
                       stringsAsFactors = FALSE)
    
  } 
  
  if (isTRUE(nothertraits == 0)) {
    ptab <- data.frame(Trait = overall$name,
                       "Data collection moment" = overall$assessment,
                       "Best ranked" = bests,
                       "Worst ranked" = worsts,
                       p.value = ps,
                       check.names = FALSE,
                       stringsAsFactors = FALSE)
  }
  
  ptab[,6] <- stars.pval(ptab$p.value)
  names(ptab)[6] <- ""
  
  ptab$p.value <- formatC(ptab$p.value, format = "e", digits = 2)
  
  # This is Table 1.2.1
  uni_sum <- outtabs[[1]]
  uni_sum$p.value <- as.numeric(uni_sum$p.value)
  uni_sum$codeQst <- rownames(uni_sum)
  uni_sum$p.value <- paste(formatC(uni_sum$p.value, format = "e", digits = 2),
                           stars.pval(uni_sum$p.value))
  uni_sum <- merge(uni_sum, covar[,c("codeQst", "questionAsked","assessmentName")], by = "codeQst")
  uni_sum <- uni_sum[,c("codeQst","assessmentName","questionAsked","p.value")]
  names(uni_sum) <- c("Covariate", "Data collection moment", "Question","p.value")
  rownames(uni_sum) <- NULL
  
  # And this is Figure 3.1
  mod_sum_PL <-
    plot(model_summaries) + 
    theme_classic() +
    theme(axis.text.x = element_text(size = 10, color = "#000000"),
          axis.text.y = element_text(size = 10, color = "#000000")) +
    labs(y = Option, x = "Estimate")
  
  
  # This is Table 3.3
  model_summaries <- model_summaries[, c("term", "estimate","quasiSE","group")]
  names(model_summaries) <- c(Option, "Estimate","quasiSE","Group")
  
  # This is the fist table in Section 1
  tbl_section1 <- data.frame()
  
  for(i in seq_along(trait_list)) {
    
    nd <- try(sum(trait_list[[i]][["keep"]]), silent = TRUE)
    if (isFALSE(is.numeric(nd))){
      nd <- 0L
    }
    
    d <- data.frame(name = trait_list[[i]]$name,
                    collect = trait_list[[i]]$assessment,
                    quest = trait_list[[i]]$question,
                    n = nd,
                    code = trait_list[[i]]$code)
    
    tbl_section1 <- rbind(tbl_section1, d)
    
  }
  
  # copy this table to make a plot
  plottbl1 <- tbl_section1
  
  tbl_section1 <- tbl_section1[,-5]
  # rename colunms in the original table
  names(tbl_section1) <- c("Trait", "Data collection moment", 
                           "Question asked", "Number of valid answers")
  
  # fill up the information if any trait was removed
  trait$key <- paste0(trait$code, trait$assessmentName)
  plottbl1$key <- paste0(plottbl1$code, plottbl1$collect)
  
  # merge datasets
  plottbl1 <- merge(trait[,c("key","codeQst","assessmentName")], 
                    plottbl1, 
                    by = "key", 
                    all.x = TRUE)
  
  plottbl1 <- plottbl1[,c("key","codeQst","assessmentName","n")]
  
  # sort values to match with the trait data
  plottbl1 <- plottbl1[match(trait$key, plottbl1$key), ]
  
  plottbl1$n <- ifelse(is.na(plottbl1$n), 1, plottbl1$n)
  
  names(plottbl1) <- c("key","code","collect","n")
  
  # add the registration info
  regsinfo <- data.frame(key = "Registration",
                         collect = "Registration",
                         n = nranker,
                         code = "Registration")
  
  plottbl1 <- rbind(regsinfo, plottbl1)
  
  # and force the elements to be factors in the right order
  plottbl1$code <- factor(plottbl1$code, levels = plottbl1$code)
  plottbl1$collect <- factor(plottbl1$collect, levels = unique(plottbl1$collect))

  nrects <- length(plottbl1$code)
  
  # define rects of data collection moments
  rects <- data.frame(xstart = seq(0.5, nrects - 0.5, 1),
                      xend =  seq(1.5, nrects + 0.5, 1), 
                      col = plottbl1$collect)
  
  plot_tbl1 <-
  ggplot() +
    geom_line(data = plottbl1, aes(x = code, y = n, group = 1), size = 1) +
    geom_point(data = plottbl1, aes(x = code, y = n, group = 1), size = 1) +
    #geom_smooth(data = plottbl1, aes(x = code, y = n, group = 1), method = lm, se = FALSE) +
    geom_rect(data = rects, aes(xmin = xstart, xmax = xend, 
                                ymin = 0, ymax = round(max(plottbl1$n) + 50, -1),
                                fill = col), alpha = 0.3) +
    scale_fill_grey(name = "Data collection moment") +
    theme_bw() + 
    ylim(0, round(max(plottbl1$n) + 50, -1)) +
    labs(x = "Trait", y = "Number of answers") +
    theme(axis.text.x = element_text(size = 12, 
                                     angle = 60, 
                                     hjust = 1, 
                                     color = "#000000"),
          axis.text.y = element_text(size = 12, color = "#000000"),
          text = element_text(size = 12, color = "#000000"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "bottom")

  # define height of plots based on items
  favplot_h <- nitems * 0.4
  contest_h <- nitems * 0.4 * 2
  agreem_h <- ntrait * 0.9
  multcomp_h <- nitems * 0.6 
  
  if (is.numeric(reference)) {
    reference <- items[reference]
  }
  
}, error = function(cond) {
  return(cond)
}
)

if (any_error(try_head_summ)) {
  e <- paste("Error 114.", try_head_summ$message)
  error <- c(error, e)
  done <- FALSE
}

# ................................................................
# ................................................................
# Write outputs ####
#determine format based on extensions
output_format <- ifelse(extension == "docx","word_document", 
                        paste0(extension,"_document"))


if (all(infosheets, done)) {
  
  try_infosheet <- tryCatch({
    
    # table with the worth parameters from overall performance
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
    
    freq_items <- table(unlist(itemdata))
    
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
    sel <- c("id", "package_farmername", paste0("package_item_", LETTERS[1:ncomp]))
    partitable <- cmdata[, sel]
    
    names(partitable) <- gsub("package_|farmer", "", names(partitable))
    
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
      
      otr <- list()
      
      otrnames <- lapply(other_traits_list, function(x){
        x$name
      })
      
      otrnames <- as.vector(unlist(otrnames))
      
      for(i in seq_along(other_traits_list)){
        
        ot <- other_traits_list[[i]]
        
        a <- list(cmdata[ot$keep, ],
                  items = itemnames,
                  input = ot$strings,
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
        
        Rexp[ot$keep, ] <- R
        
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
        for(j in seq_along(other_traits_list)){
          
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
    
    # use the coefficients from the overall model and plot it as bar plot
    # to show the overall evaluation compared to the farmer evaluation
    pover <- coef(mod_overall, log = FALSE)
    pover <- sort(pover)
    pover <- data.frame(items = factor(names(pover), levels = names(pover)),
                        pw = as.vector(pover))
    
    poverp <- ggplot(pover, aes(x = pw,
                                y = items,
                                fill = pw)) +
      geom_bar(stat = "identity",
               position = "dodge",
               show.legend = FALSE) +
      labs(x = "", y = "") +
      theme(element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size = 5),
            panel.background = element_blank(),
            plot.margin=grid::unit(c(0,0,0,0), "mm"))
    
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

