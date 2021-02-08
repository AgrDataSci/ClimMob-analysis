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
ranker      <- args[7] # how the system will refer to participants/farmers
option      <- args[8] # how the system will refer to tested items
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
  
  projname <- cmdata[1,"package_project_name"]
  
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

# write output directory
tryCatch({
  dir.create(outputpath, showWarnings = FALSE, recursive = TRUE)
})

# ................................................................
# ................................................................
# Dataset parameters ####
dtpars <- tryCatch({
  
  items <- cmdata[, grepl("package_item", names(cmdata))]
  ncomp <- dim(items)[[2]]
  
  # select which function will be used to create the Plackett-Luce rankings
  # it will depends on how many items each participant compares
  if (ncomp == 3) {
    rankwith <- "rank_tricot"
  }
  
  if (ncomp > 3) {
    rankwith <- "rank_numeric"
  }
  
  # Get some info from the data and imputed parameters 
  Option <-  ClimMobTools:::.title_case(option)
  options <- ClimMobTools:::.pluralize(option)
  rankers <- ClimMobTools:::.pluralize(ranker)
  nranker <- nrow(cmdata)
  itemnames <- names(items)
  items <- unique(sort(unlist(items)))
  nitems <- length(unique(sort(unlist(items))))
  expvar <- pars$expl$vars
  expvar_full <- pars$expl$name
  expvar_code <- pars$expl$codeQst
  ntrait <- dim(pars$chars)[[1]]
  nothertraits <- dim(pars$chars)[[1]] - 1
  nquest <- pars$chars$n_quest[1]
  rankwith <- rankwith
  
  # minimum n of complete data required in a characteristic evaluation
  # before it is excluded. 
  missper <- 4
  
  # minimum proportion of valid observations in explanatory variables
  missexp <- 0.4
  
  # minimum split size for tree models
  minsplit <- ceiling(nranker * 0.1)
  if (isTRUE(minsplit < 10)) {
    minsplit <- 10
  }
  
  if (isFALSE(is.numeric(minsplit))) {
    minsplit <- 10
  }
  
  # Set alpha
  sig_level <- 0.1
  
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
  
  trait <- pars$chars$char
  
  trait_full <- pars$chars$char_full
  
  trait_code <- pars$chars$char
  
  overallVSlocal <- length(pars$perf) > 0
  
  # this list will keep the logical vectors to filter the data per 
  # remaining trait, the ones that passed the evaluation on minimal 
  # amount of missing data 
  trait_list <- list()
  
  # run over traits to filter NAs and prepare for PlackettLuce rankings
  for(i in seq_along(trait)){
    
    result <- list()
    
    # get the question full name
    trait_i <- as.character(pars$chars[i, paste0("quest_", seq_len(nquest))])
    
    # look for it in cmdata
    for(j in seq_along(trait_i)) {
      
      trait_i[j] <- names(cmdata[which(grepl(trait_i[j], names(cmdata)))])
      
    }
    
    # check for data completeness in this trait
    # should return a vector with TRUE, FALSE, 
    # where TRUE = complete, FALSE = missing
    keep <- apply(cmdata[trait_i], 1, is.na)
    keep <- as.vector(colSums(keep) == 0)
    
    # check if number of missing data is lower than the threshold
    dropit <- sum(keep) < missper
    
    # if lower than missper it will be dropped
    if (isTRUE(dropit)) next
    
    # if required, add comparison with local item
    # again, it assumes that overall performance is the first characteristic
    if (isTRUE(i == 1 & overallVSlocal)) {
      
      ovsl <- as.vector(pars$perf[, paste0("quest_", seq_len(pars$perf$n_quest))])
      
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
      dropit2 <- (sum(keep2) / nranker) < missper
      
      # if if below the minimal number of observations then the comparison
      # with the local item is ignored
      if (isTRUE(dropit2)) {
        overallVSlocal <- FALSE
      }
      
      result[["keep2"]] <- keep2
      
      result[["ovsl"]] <- ovsl
      
    }
    
    # get the question registered in the survey
    qi <- grepl(paste(strsplit(trait_i[[1]], "_")[[1]][-c(1:2)], collapse = "_"),
                quest$name)
    
    result[["keep"]] <- keep
    result[["input"]] <- trait_i
    result[["fullname"]] <- trait_full[i]
    result[["code"]] <- trait_code[i]
    result[["question"]] <- quest$desc[qi]
    
    trait_list[[trait[i]]] <- result
    
  }
  
  trait <- names(trait_list)
  
  # refresh the number of traits
  ntrait <- length(trait_list)
  
  # find the index for overall performance
  overall <- trait_list[[1]]
  
  # and the other traits
  other_traits <- trait[-1]
  
  other_traits_full <- trait_full[-1]
  
  other_traits_code <- trait_code[-1]
  
  other_traits_list <- trait_list[-1]
  
  # refresh number of other traits
  nothertraits <- length(other_traits)
  
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
  
  expvar_list <- list()
  
  if (any(expvar == "Intercept")) {
    cmdata$Intercept <- rep(0, nranker)
    expvar_list[["expvar"]] <- "Intercept"
    expvar_list[["expvar_full"]] <- "Intercept"
    expvar_list[["keep"]] <- rep(TRUE, nranker)
  }
  
  if (all(expvar != "Intercept")) {
    
    # add the string $ to indicate the end of pattern
    expvar <- paste0(expvar, "$")
    
    # check for the full names
    for(i in seq_along(expvar)){
      expvar[i] <- names(cmdata[which(grepl(expvar[i], names(cmdata)))])
    }
    
    # check for missing data
    keep <- NULL
    for(i in seq_along(expvar)){
      k <- !is.na(cmdata[, expvar[i]])
      keep <- cbind(keep, k)
    }
    
    # find those that are above the threshold of missexp
    dropit <- missexp > (colSums(keep) / nranker)
    
    # drop those bellow threshold
    keep <- as.data.frame(keep[, !dropit])
    
    # create a single vector that will be used to filter cmdata
    keep <- rowSums(keep)
    keep <- keep == max(keep)
    
    expvar <- expvar[!dropit]
    expvar_full <- expvar_full[!dropit]
    expvar_code <- expvar_code[!dropit]
    expvar_dropped <- expvar_code[dropit]
    
    # if no explanatory variable left out put a pseudo variable
    if(isTRUE(length(expvar) == 0)) {
      cmdata$Intercept <- rep(0, nranker)
      expvar_list[["expvar"]] <- "Intercept"
      expvar_list[["expvar_full"]] <- "Intercept"
      expvar_list[["expvar_code"]] <- "Intercept"
      expvar_list[["keep"]] <- rep(TRUE, nranker)
    }else{
      expvar_list[["expvar"]] <- expvar
      expvar_list[["expvar_full"]] <- expvar_full
      expvar_list[["expvar_code"]] <- expvar_code
      expvar_list[["keep"]] <- keep
    }
    
  }
}, error = function(cond) {
  return(cond)
}
)

if (any_error(org_covar)) {
  e <- paste("Error 105.", org_covar$message)
  error <- c(error, e)
  
  expvar_list <- list()
  
  expvar <- "Intercept"
  cmdata$Intercept <- rep(0, nranker)
  expvar_list[["expvar"]] <- "Intercept"
  expvar_list[["expvar_full"]] <- "Intercept"
  expvar_list[["expvar_code"]] <- "Intercept"
  expvar_list[["keep"]] <- rep(TRUE, nranker)
  
}

# .......................................................
# .......................................................
# Make map ####
org_lonlat <- tryCatch({
  # Check if lonlat is provided
  lon <- grepl("_lon", names(cmdata))
  lat <- grepl("_lat", names(cmdata))
  
  geoTRUE <- all(any(lon), any(lat))
  
  if (isTRUE(geoTRUE)) {
    
    #find the vector with most completeness 
    less_nas <- lapply(cmdata[lon], function(x){
      sum(is.na(x))
    })
    
    lon <- names(which.min(unlist(less_nas)))
    lat <- gsub("_lon", "_lat", lon)
    
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
  
  itemtable$Abbreviation <- gosset:::.reduce(as.character(itemtable[,Option]))
  
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
try_fav_oa <- tryCatch({
  
  if (isTRUE(overallVSlocal)) {
    
    keep <- overall$keep2 & overall$keep
    
    a <- list(cmdata[keep, ],
              items = itemnames,
              input = overall$input,
              additional.rank = cmdata[keep, overall$ovsl])
    
    R <- do.call(rankwith, args = a)
    
  }
  
  if (isFALSE(overallVSlocal)) {
    
    keep <- overall$keep
    
    # list of arguments for the function that will be used to 
    # create the rankings
    a <- list(cmdata[keep, ],
              items = itemnames,
              input = overall$input)
    
    R <- do.call(rankwith, args = a)
    
  }
  
  # get the network of items evaluated in the project
  net <- network(R)
  
  fav1 <- summarise_favourite(R) 
  
  fav2 <- fav1
  
  fav2$best <- paste0(round(fav2$best, 1), "%")
  fav2$worst <- paste0(round(fav2$worst, 1), "%")
  fav2$fav_score <- round(fav2$fav_score, 1)
  
  fav2 <- fav2[,-which(grepl("wins", names(fav2)))]
  
  names(fav2) <- c(Option,"N","Top Ranked",
                   "Bottom Ranked", "Net Favourability Score")
  
  fav1 <- 
    plot(fav1) + 
    xlab("") +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, color = "#000000"),
          axis.text.y = element_text(size = 10, color = "#000000"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none")
  
  # Contest Plots
  cont1 <- summarise_dominance(R)
  
  cont2 <- summarise_victories(R)
  
  cont2 <-
    plot(cont2) + 
    labs(y = "", x = "") + 
    theme_minimal() +  
    theme(axis.text.x = element_text(size = 10, color = "#000000"),
          axis.text.y = element_text(size = 10, color = "#000000"),
          strip.text.x = element_text(size = 11, color = "#000000", face = "bold"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "none")
  
}, error = function(cond) {
  return(cond)
}
)

if (any_error(try_fav_oa)) {
  e <- paste("Error 108.", try_fav_oa$message)
  error <- c(error, e)
  done <- FALSE
}

# .......................................................
# .......................................................
# run the same for the other characteristics
try_fav_ot <- tryCatch({
  
  if (isTRUE(nothertraits > 0)) {
    
    fav_other_traits <- list()
    
    vic_other_traits <- list()
    
    dom_other_traits <- list()
    
    for(i in seq_along(other_traits_list)){
      
      a <- list(data = cmdata[other_traits_list[[i]]$keep, ],
                items = itemnames,
                input = other_traits_list[[i]]$input)
      
      R_i <- do.call(rankwith, args = a)
      
      fav1_i <- summarise_favorite(R_i)
      fav2_i <- fav1_i
      
      fav2_i$best <- paste0(round(fav2_i$best, 1), "%")
      fav2_i$worst <- paste0(round(fav2_i$worst, 1), "%")
      fav2_i$fav_score <- round(fav2_i$fav_score, 1)
      
      fav2_i <- fav2_i[,-which(grepl("wins", names(fav2_i)))]
      
      names(fav2_i) <- c(Option,"N","Top Ranked","Bottom Ranked",
                         "Net Favourability Score")
      
      fav_other_traits[[i]] <- list(fav1_i, fav2_i)
      
      vic_other_traits[[i]] <- summarise_victories(R_i)
      
      dom_other_traits[[i]] <- summarise_dominance(R_i)
      
    }
  }
  
}, error = function(cond) {
  return(cond)
}
)

if (any_error(try_fav_ot)) {
  e <- paste("Error 109.", try_fav_ot$message)
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
    
    # list of arguments for the function that will be used to 
    # create the rankings
    a <- list(cmdata[keep, ], 
              items = itemnames,
              input = overall$input)
    
    compare_to <- do.call(rankwith, args = a)
    
    compare_with <- list()
    
    for (i in seq_along(other_traits_list)) {
      
      ot <- other_traits_list[[i]]
      
      a <- list(cmdata[keep, ],
                items = itemnames,
                input = ot$input)
      
      otr <- do.call(rankwith, args = a) 
      
      compare_with[[i]] <- otr
    }
    
    # # get the names of all items to be sure that they 
    # # co-occur in the same network
    # compare_base <- dimnames(compare_to)[[2]]
    # compare_other <- lapply(compare_with, function(x){
    #   dimnames(x)[[2]]
    # })
    
    
    
    agreement <- summarise_agreement(compare_to, 
                                     compare_with, 
                                     labels = other_traits_code)
    
    strongest_link <- c(agreement[[which.max(agreement$kendall), "labels"]],
                        round(max(agreement$kendall), 0))
    
    
    weakest_link   <- c(agreement[[which.min(agreement$kendall), "labels"]],
                        round(min(agreement$kendall), 0))
    
    
    agreement_table <- agreement
    
    agreement_table[,c(2:4)] <- lapply(agreement_table[,c(2:4)], function(x){
      x <- round(x, 1)
      x <- paste0(x,"%")
    })
    
    names(agreement_table) <- c("Characteristic", 
                                "Complete Ranking Agreement",
                                "Agreement with Overall Best", 
                                "Agreement with Overall Worst")
    
    
  } 
  
  if (isTRUE(nothertraits == 0)) {
    strongest_link <- character()
    weakest_link <- character()
    agreement_table <- data.frame(Option = "Only Overall Performance was used",
                                  X = "",
                                  Y = "",
                                  Z = "")
    names(agreement_table)<-c(" ","","  ","   ")
    agreement <- data.frame(labels = "",
                            kendall = 0,
                            first = 0,
                            last = 0)
  }
  
}, error = function(cond) {
  return(cond)
}
)

if (any_error(try_agree)) {
  e <- paste("Error 110.", try_agree$message)
  error <- c(error, e)
  
  strongest_link <- character()
  weakest_link <- character()
  agreement_table <- data.frame(Option = "Only Overall Performance was used",
                                X = "",
                                Y = "",
                                Z = "")
  names(agreement_table) <- c(" ","","  ","   ")
  agreement <- data.frame(labels = "",
                          kendall = 0,
                          first = 0,
                          last = 0)
}

# .......................................................
# .......................................................
# PlackettLuce Model ####
try_pl <- tryCatch({
  mod_overall <- PlackettLuce(R)
  
  model_summaries <- multcompPL(mod_overall, ref = reference)
  
  fullanova <- anova.PL(mod_overall)
  
  worthscaled <- rev(sort(exp(coef(mod_overall)) / sum(exp(coef(mod_overall)))))
  
  worthscaled <- data.frame(label = factor(names(worthscaled),
                                           (names(worthscaled))),
                            worth = worthscaled,
                            prob = paste0(round(worthscaled * 100, 1), "%"), 
                            check.names = FALSE)
  
  # Get Table 3.4
  worthscaled <- worthscaled[, c("label", "worth", "prob")]
  row.names(worthscaled) <- NULL
  names(worthscaled) <- c(Option, c("Worth", "Win probability"))
  
  
  # Get the aov Table 1.1
  aov_mod_overall <- anova.PL(mod_overall)
  aov_mod_overall[, 5] <- paste(format.pval(aov_mod_overall[,5]), 
                                stars.pval(aov_mod_overall[,5]))
  aov_mod_overall[2, "model"] <- overall$code
  
  
  # Run over the other traits
  mods <- list()
  summaries <- list()
  plot_summaries <- list()
  worths <- list()
  anovas <- list()
  aov_tables <- list()
  
  for (i in seq_along(other_traits_list)){
    ot <- other_traits_list[[i]]
    
    if (isTRUE(overallVSlocal)) {
      keep <- overall$keep2 & ot$keep
      
      a <- list(cmdata[keep, ],
                items = itemnames,
                input = ot$input,
                additional.rank = cmdata[keep, overall$ovsl])
      
      Rot <- do.call(rankwith, args = a)
    }
    
    if (isFALSE(overallVSlocal)) {
      keep <- ot$keep
      
      a <- list(data  = cmdata[keep, ],
                items = itemnames,
                input = ot$input)
      
      Rot <- do.call(rankwith, args = a)
      
    }
    
    mod_t <- PlackettLuce(Rot)
    
    mods[[i]] <- mod_t
    
    # aov tables for other characteristics in Section 4. Table 4.*.1
    aov_i <- anova.PL(mod_t)
    
    # this is to be used later for Table 1.1
    anovas[[i]] <- aov_i
    
    # organise pvalues
    aov_i[,5] <- paste(format.pval(aov_i[,5]), 
                       stars.pval(aov_i[,5]))
    aov_i[2, "model"] <- other_traits_code[[i]]
    
    aov_tables[[i]] <- aov_i
    
    # This is Table 4.*.2
    summ_i <- multcompPL(mod_t, ref = reference)
    # And this is Figure 3.*.2
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
    worths_i <- rev(sort(coef(mod_t, log = FALSE)))
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
  # PlackettLuce combining traits together ####
  coefs <- qvcalc(mod_overall, ref = reference)[[2]]$estimate
  
  for(i in seq_along(other_traits)){
    coefs <- cbind(coefs, 
                   scale(qvcalc(mods[[i]], ref = reference)[[2]]$estimate))
  }
  
  coefs <- as.data.frame(coefs)
  
  # add item names as rows
  rownames(coefs) <- rownames(qvcalc(mod_overall)[[2]])
  
  # set col names with title case traits
  otf <- gsub(" ","_", other_traits_code)
  otf <- ClimMobTools:::.title_case(otf)
  
  ov <- gsub(" ","_", overall$code)
  
  names(coefs)[1] <- ov
  names(coefs)[-1] <- otf

}, error = function(cond) {
  return(cond)
}
)

if (any_error(try_pl)) {
  e <- paste("Error 111.", try_pl$message)
  error <- c(error, e)
  done <- FALSE
}

# .......................................................
# .......................................................
# Compute partial least squares ####
try_pls <- tryCatch({
  
  if (isTRUE(nothertraits > 0)) {
    fml <- paste(ov, " ~ ", paste(otf, collapse = " + "))
    fml <- as.formula(fml)
    
    m2 <- plsr(fml,
               data = coefs,
               validation = "LOO", 
               jackknife = TRUE)
    
    if (ncol(m2$projection) > 1 ) {
      arrows <- data.frame((m2$projection)[,1:2],
                           trait = otf,
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
    arrows$group <- "char"
    
    sco <- rbind(scores[,c("Comp.1","Comp.2","group")], 
                 arrows[,c("Comp.1","Comp.2","group")])
    
    sco$label <- rownames(sco)
    
    pls_plot <-
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
                       label = trait,
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
  
  if (isTRUE(overallVSlocal)) {
    
    keep <- overall$keep2 & expvar_list$keep
    
    a <- list(cmdata[keep, ],
              items = itemnames,
              input = overall$input,
              additional.rank = cmdata[keep, overall$ovsl], 
              group = TRUE)
    
    G <- do.call(rankwith, args = a)
    
    
  } 
  
  if (isFALSE(overallVSlocal)) {
    keep <- overall$keep & expvar_list$keep
    
    a <- list(cmdata[keep, ],
              items = itemnames,
              input = overall$input, 
              group = TRUE)
    
    G <- do.call(rankwith, args = a)
    
  }
  
  # data frame of explanatory variables
  Gdata <- as.data.frame(cmdata[keep, expvar_list$expvar], stringsAsFactors = TRUE)
  nvar <- length(expvar_list$expvar)
  
  # setup the variables
  Gdata[1:nvar] <- lapply(Gdata[1:nvar], function(x){
    if(is.numeric(x)) {
      x <- round(x, 3)
    }
    
    if(is.character(x)) {
      x <- as.factor(x)
    }
    return(x)
  })
  
  names(Gdata) <- expvar_list$expvar_code
  Gdata <- cbind(G, Gdata)
  
  tree_f <- pltree(G ~ .,
                   data = Gdata, 
                   minsize = minsplit,
                   alpha = sig_level,
                   ref = reference)
  
  
  # if the tree has splits, extract coeffs from nodes
  if (isTRUE(length(tree_f) > 1)) { 
    
    node_ids <- nodeids(tree_f, terminal = TRUE)
    
    coefs_t <- NULL
    for(i in seq_along(node_ids)) {
      
      coef_i <- data.frame(node = node_ids[i],
                           rule = partykit:::.list.rules.party(tree_f, node_ids[i]),
                           multcompPL(tree_f[[ node_ids[i] ]]$node$info$object, ref = reference),
                           n = tree_f[[ node_ids[i] ]]$node$info$nobs,
                           stringsAsFactors = FALSE)
      
      coefs_t <- rbind(coefs_t, coef_i)
      
    }
    
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
      
      tmp <- subset(coefs_t, rule==rules[i])
      
      best_tree <- rbind(best_tree,
                         c(tmp$n[1], 
                           paste(tmp$term[grepl("a", tmp$group)], collapse=", "),
                           paste(rev(tmp$term[grepl(tmp$group[nrow(tmp)], tmp$group)]), collapse=", ")))
    }
    
    node_summary <- data.frame(rules, 
                               best_tree, 
                               stringsAsFactors = FALSE)
    
    names(node_summary) <- c("Split","Number of Respondents","Best Ranked","Worst Ranked")
    
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
      outtabs[[j]] <- data.frame(Node = j, 
                                 Message = "No further splits possible", 
                                 p.value = NA,
                                 stringsAsFactors = FALSE)
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
  siglist <- NULL
  for(i in seq_along(outtabs)){
    if (dim(outtabs[[i]])[[2]] > 3) {
      siglist <- c(siglist,
                   rownames(outtabs[[i]])[outtabs[[i]]$p.value < sig_level])
    }
  }
  
  siglist <- unique(siglist)
  
  ps <- fullanova[2, 5]
  
  if (isTRUE(ps < sig_level)) {
    bests <- as.character(model_summaries$term[grep("a", model_summaries$group)])
    
    if (isTRUE(length(bests) > 3)) {
      bests <- bests[1:3]
    }
    
    bests <- paste(bests, collapse =", ")
    
    worsts <- as.character(rev(model_summaries$term[grepl(model_summaries$group[nrow(model_summaries)],
                                                          model_summaries$group)]))
    
    if(isTRUE(length(worsts) > 3)) {
      worsts <- worsts[1:3]
    }
    
    worsts <- paste(worsts, collapse = ", ")
    
  } 
  
  if (isTRUE(ps > sig_level)) {
    
    bests <- worsts <- "No significant difference"
    
  }
  
  if (isTRUE(nothertraits > 0)) {
    
    for(i in seq_along(anovas)){
      
      ps_i <- anovas[[i]][2,5]
      
      if (isTRUE(ps_i < sig_level)) {
        
        summ_i <- summaries[[i]]
        
        # take the best three items from this comparison
        bests_i <- as.character(summ_i[, Option][grepl("a", summ_i[,"Group"])])
        # if more than three, subset to get only three
        if (isTRUE(length(bests_i) > 3)) {
          bests_i <- bests_i[1:3]
        }
        
        bests_i <- paste(bests_i, collapse = ", ")
        
        # put it together with the bests for overall performance
        bests <- c(bests, bests_i)
        
        # get the three worst items
        worsts_i <- as.character(rev(summ_i[grepl(summ_i[nrow(summ_i),"Group"], summ_i[, "Group"]), Option]))
        
        # if more than three, subset to get only three
        if (isTRUE(length(worsts_i) > 3)) {
          worsts_i <- worsts_i[1:3]
        }
        
        worsts_i <- paste(worsts_i, collapse=", ")
        
        worsts <- c(worsts, worsts_i)
        
        
      }
      
      if (isTRUE(ps_i > sig_level)) {
        
        bests <- c(bests, "No significant difference")  
        
        worsts <- c(worsts,"No significant difference")  
      }
      
      ps <- c(ps, ps_i)
      
    }
    
    ptab <- data.frame(Ranking = c(overall$code, other_traits_code),
                       "Best Ranked" = bests,
                       "Worst Ranked" = worsts,
                       p.value = ps,
                       check.names = FALSE,
                       stringsAsFactors = FALSE)
    
  } 
  
  if (isTRUE(nothertraits == 0)) {
    ptab <- data.frame(Ranking = overall$code,
                       "Best Ranked" = bests,
                       "Worst Ranked" = worsts,
                       p.value = ps,
                       check.names = FALSE,
                       stringsAsFactors = FALSE)
  }
  
  pval_legend <- attr(stars.pval(ptab$p.value), "legend")
  
  ptab[,5] <- stars.pval(ptab$p.value)
  names(ptab)[5] <- ""
  
  ptab$p.value <- format.pval(ptab$p.value, digits = 3)
  
  
  # This is Table 1.2.1
  uni_sum <- outtabs[[1]]
  uni_sum$p.value <- as.numeric(uni_sum$p.value)
  uni_sum$Covariate <- rownames(uni_sum)
  uni_sum$p.value <- paste(format.pval(uni_sum$p.value), stars.pval(uni_sum$p.value))
  uni_sum$Question <- expvar_full
  uni_sum <- uni_sum[,c("Covariate","Question","p.value")]
  rownames(uni_sum) <- NULL
  
  # And this is Figure 3.1
  mod_sum_PL <-
    plot(model_summaries) + 
    theme_classic() +
    theme(axis.text.x = element_text(size = 10, color = "#000000"),
          axis.text.y = element_text(size = 10, color = "#000000")) +
    labs(y = Option, x = "Estimate")
  
  
  # This is Table 3.3
  model_summaries$items <- row.names(model_summaries)  
  rownames(model_summaries) <- NULL
  model_summaries <- model_summaries[, c("items", "estimate","quasiSE","group")]
  names(model_summaries) <- c(Option, "Estimate","quasiSE","Group")
  
  
  # This is the fist table in Section 1
  tbl_section1 <- data.frame()
  
  for(i in seq_along(pars$chars[,1])) {
    
    nd <- try(sum(trait_list[[i]][["keep"]]), silent = TRUE)
    if (isFALSE(is.numeric(nd))){
      nd <- 0L
    }
    
    d <- data.frame(name = pars$chars[i,"char"],
                    char = pars$chars[i,"char_full"],
                    n = nd)
    
    tbl_section1 <- rbind(tbl_section1, d)
    
  }
  
  names(tbl_section1) <- c("Characteristic", "Question asked", "Number of valid answers")
  
  # define height of plots based on items
  favplot_h <- nitems * 0.4
  contest_h <- nitems * 0.4 * 2
  agreem_h <- ntrait * 0.9
  multcomp_h <- nitems * 0.6 
  
  # overall name
  ovname <- tolower(overall$code)
  Ovname <- ClimMobTools:::.title_case(overall$code)
  
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





