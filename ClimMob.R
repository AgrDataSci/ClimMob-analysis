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
minsplit    <- args[17] # minimum n in each tree node

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
source(paste0(fullpath, "/R/functions.R"))

# Two objects to begin with that will be used to verify the process
error <- NULL
done <- TRUE

# ................................................................
# ................................................................
# 1. Read data #### 
try_data <- tryCatch({
  # call pars sent by ClimMob
  pars <- jsonlite::fromJSON(infoname)
  pars <- ClimMobTools:::.decode_pars(pars)
  
  # the trial data 
  # add the class "CM_list" so it can be passed to 
  cmdata <- jsonlite::fromJSON(outputname)
  class(cmdata) <- union("CM_list", class(cmdata))
  # coerce to data.frame() 
  cmdata <- as.data.frame(cmdata, tidynames = FALSE, pivot.wider = TRUE)
  
  # Get some info from the data and ClimMob parameters 
  projname <- cmdata[1, "package_project_name"]
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
    rankwith <- "rankTricot"
  }
  
  if (ncomp > 3) {
    rankwith <- "rank_numeric"
  }
  
  # minimum proportion of valid entries in tricot vs local
  # this will be computed based on the valid entries of the reference
  # trait after validations
  mintricotVSlocal <- 0.90
  
  # method for adjustments for confidence intervals and setting widths for comparison. 
  ci_adjust <- "none"
  
  # confidence interval level for comparison plots with error bars
  ci_level <- 0.84
  
  # resolution of display items
  dpi <- 400
  out_width <- "100%"
  
  # define height of plots based on items
  favplot_h <- nitems * 0.4
  agreem_h <- ntrait * 0.6
  multcomp_h <- nitems * 0.6 
  
  if (favplot_h < 5) favplot_h <- 5
  if (agreem_h < 6)  agreem_h <- 6
  if (multcomp_h < 6) multcomp_h <- 6
  
  dir.create(outputpath, showWarnings = FALSE, recursive = TRUE)
  
}, error = function(cond) {
    return(cond)
  }
)

if (any_error(try_data)) {
  e <- try_data$message
  error <- c(error, e)
  done <- FALSE
}

# ................................................................
# ................................................................
# 2. Organise the rankings ####
# This is to check for missing data and create a vector
# for each characteristic that will be used to filter 
# inconsistent data
org_rank <- tryCatch({
  
  # check whether a request to split the data by groups (gender, location, etc.) is provided
  if (length(groups) > 0) {
    
    group_index <- integer()
    for(i in seq_along(groups)) {
      group_index <- c(group_index, which(grepl(groups[i], names(cmdata)))[1])
    }
    
    group_index <- group_index[!is.na(group_index)]
    
    groups <- names(cmdata)[group_index]
    
    if (length(groups) > 1) {
      
      keep <- rowSums(apply(cmdata[,groups], 2, function(x) {is.na(x)})) == 0
      
      cmdata <- cmdata[keep, ]
      
      cmdata$group <- apply(cmdata[,groups], 1, function(x) {paste(x, collapse = " - ")})
      
    }
    
    if (length(groups) == 1) {
      
      cmdata <- cmdata[!is.na(cmdata[,groups]), ]
      
      cmdata$group <- cmdata[,groups]
      
    }
    
    # if any of the groups has less than 15% of the total data
    # then the groups will not be considered
    if (any(table(cmdata$group) / nrow(cmdata) < 0.15)) {
      cmdata$group <- NA
    }
    
    # if more than 9 groups
    # then the groups will not be considered
    if (length(unique(cmdata$group)) > 9) {
      cmdata$group <- NA
    }
    
    if (length(groups) == 0) {
      cmdata$group <- NA
    }
    
  }else{
    cmdata$group <- NA
  }
  
  groups <- sort(unique(cmdata$group))
  
  if (any(is.na(groups))) {
    groups <- character()
  }
  
  if (length(groups) == 0) {
    groups <- character()
  }
  
  # rename traits to avoid duplicated strings, in case the same 
  # trait is tested in different data collection moments 
  trait <- pars$traits
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
    # as it results in issues on PlackettLuce S3 methods
    tied <- as.vector(apply(cmdata[trait_i], 1, function(x) {
      all(x == "Tie") & all(!is.na(x))
    }))
    
    cmdata[tied, trait_i] <- NA
    
    # if best and worst are recorded with the same letter
    equal <- as.vector(apply(cmdata[trait_i], 1, function(x) {
      x[1] == x[2] & all(!is.na(x))
    }))
    
    cmdata[equal, trait_i] <- NA
    
    # check for data completeness in this trait
    # should return a vector with TRUE, FALSE, 
    # where TRUE = complete, FALSE = missing
    keep <- apply(cmdata[trait_i], 1, is.na)
    keep <- as.vector(colSums(keep) == 0)
    
    # check if number of missing data is lower than the threshold
    # at least that the number of valid entries is higher than missper (5)
    cond1 <- sum(keep) > minN
    
    # at least that all the items are tested twice
    cond2 <- all(table(unlist(cmdata[keep, itemnames])) >= minitem)
    
    keepit <- all(cond1, cond2)
    
    # if lower than missper it will be dropped
    if (isFALSE(keepit)) {
      trait_dropped <- c(trait_dropped, trait$name[i])
      next
    } 
    
    # if required, add comparison with local item
    if (isTRUE(i == reference_trait & tricotVSlocal)) {
      
      # some times the user can add this question twice, I still don't have a solution for 
      # this, so I will use the one that is related to the reference trait
      ovsl <- which(names(cmdata) %in% pars$tricotVSlocal[1:3])
      
      if (length(ovsl) == 3) {
        ovsl <- names(cmdata)[ovsl]
        # check for data completeness
        keep2 <- apply(cmdata[c(itemnames, ovsl)], 1, is.na)
        keep2 <- as.vector(colSums(keep2) == 0)
        
        # combine with the vector of reference trait
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
        
      }else{
      tricotVSlocal <- FALSE
      result[["keep2"]] <- rep(FALSE, nranker)
      result[["tricotVSlocal"]] <- character()
    }
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
  reference_trait <- trait[reference_trait, "codeQst"]
  reference_trait <- which(names(trait_list) %in% reference_trait)
  
  # if for some reason the reference trait was dropped, than take the last one
  if (length(reference_trait) == 0) {
    reference_trait <- length(trait_list) 
    # and coerce tricotVSlocal to FALSE to prevent errors in matching strings
    tricotVSlocal <- FALSE
  }
  
  # get the name of the reference trait both in lower and title case
  ovname <- tolower(trait_list[[reference_trait]]$name)
  Ovname <- ClimMobTools:::.title_case(trait_list[[reference_trait]]$name)
  
  # the name of other traits combined with the name of assessments
  traits_names <- as.vector(unlist(lapply(trait_list, function(x) {
    paste0(x$name, " [", ClimMobTools:::.title_case(x$assessment), "]")
  })))
  
  traits_code <- as.vector(unlist(lapply(trait_list, function(x) {
    x$code
  })))
  
  # remove any potential special character
  traits_code <- gsub("[[:punct:]]", "", traits_code)
  
  # refresh number of other traits
  nothertraits <- length(trait_list) - 1
  
}, error = function(cond) {
  return(cond)
}
)

if (any_error(org_rank)) {
  e <- org_rank$message
  error <- c(error, e)
  done <- FALSE
}

# .......................................................
# .......................................................
# 3. Prepare summary tables / charts
org_summ <- tryCatch({
  
  #...........................................................
  # This is the fist table in Section 1
  # it shows the traits that were assessed in the project 
  # and the number of answers used in the analysis
  tbl_section1 <- lapply(trait_list, function(x){
    data.frame(name = x$name,
               collect = ClimMobTools:::.title_case(x$assessment),
               quest = x$question,
               n = sum(x$keep))
  })

  tbl_section1 <- do.call(rbind, tbl_section1)  
  
  # rename columns in the original table
  names(tbl_section1) <- c("Trait", "Data collection moment", 
                           "Question asked", "Number of valid answers")
 
  #...........................................................
  # Number of items tested
  # This table show the frequencies where items were tested
  # and how these frequencies were tested among groups, if any
  itemtable <- cmdata[, grepl("package_item", names(cmdata))]
  
  itemtable <- data.frame(table(unlist(itemtable)))
  
  itemtable$x <- with(itemtable,
                      round((Freq / nranker * 100), 1))
  
  itemtable$x <- with(itemtable,
                      paste0(x, "%"))
  
  names(itemtable) <- c(Option, "Freq", "Relative freq")
  
  # check if any group is provided so it can be added to the itemtable
  if (length(groups) > 0) {
    
    x <- unlist(cmdata[, grepl("package_item", names(cmdata))])
    
    ngroups <- table(cmdata$group)
    
    grouptbl <- c()
    
    for (i in seq_along(groups)) {
      grouptbl <- cbind(grouptbl, 
                        tapply(rep(cmdata$group, ncomp), x, function(x) {
                          sum(x == groups[i], na.rm = TRUE)
                          }))
    }
    
    grouptbl <- as.data.frame(grouptbl)
    
    names(grouptbl) <- paste0(groups, " (n=", ngroups, ")")
    
    itemtable <- cbind(itemtable, grouptbl)
    
    rm(x)
    
  }
  
  itemtable$Abbreviation <- gosset:::.reduce(as.character(itemtable[, Option]))
  
  itemtable <- itemtable[union(c(Option, "Abbreviation"), names(itemtable))]
  
  rownames(itemtable) <- 1:nrow(itemtable)
  
  #...........................................................
  # Participation during trial (response rate)
  # it is a plot showing the rate of response in each 
  # data collection moment, it takes the larger N response
  # for each data collection moment
  participation <- data.frame(n = nrow(cmdata),
                              n_tot = nrow(cmdata),
                              group = "Whole group",
                              dc = "Registration")
  
  for(i in seq_along(trait_list)) {
    
    part <- data.frame(n = sum(trait_list[[i]]$keep),
                       n_tot = nrow(cmdata),
                       group = "Whole group",
                       dc = trait_list[[i]]$assessment)

    participation <- rbind(participation, part)
    
  }
  
  if (length(groups) > 0) {
    
    participation2 <- data.frame(n = as.vector(table(cmdata$group)),
                                 n_tot = as.vector(table(cmdata$group)),
                                 group = names(table(cmdata$group)),
                                 dc = "Registration")
    
    for(i in seq_along(trait_list)) {
      
      n <- table(cmdata[trait_list[[i]]$keep, "group"])
      
      p <- data.frame(group = groups,
                      dc = trait_list[[i]]$assessment)
      
      part <- data.frame(n = as.vector(n),
                         n_tot = as.vector(table(cmdata$group)),
                         group = names(n))
      
      part <- merge(p, part, by = "group", all.x = TRUE)
      
      participation2 <- rbind(participation2, part)
      
    }
    
    participation <- rbind(participation, participation2)
  
  }
  
  # get the highest value in each data collection moment
  participation <- split(participation, paste0(participation$dc, participation$group))
  
  participation <- lapply(participation, function(x){
    i <- which.max(x$n)
    x[i, ]
  })
  
  participation <- do.call(rbind, participation)
  
  # transform into proportion to make it easier to visualize
  participation$value_perc <- participation$n / participation$n_tot
  
  participation$dc <- ClimMobTools:::.title_case(participation$dc)
  
  participation$dc <- factor(participation$dc, levels = c("Registration", 
                                                          ClimMobTools:::.title_case(unique(trait$assessmentName))))
  
  participation$group <- factor(participation$group, levels = c("Whole group",
                                                                unique(groups)))
  
  partiplot <- 
    ggplot(participation, aes(x = dc, y = value_perc, 
                              group = group, color = group)) +
    geom_line(size = 1) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_colour_manual(values = col_pallet(length(unique(participation$group))), 
                        name = "") +
    theme_bw() +
    theme(panel.background = element_blank(),
          panel.grid = element_blank(), 
          axis.text.x = element_text(angle = 35, hjust = 1),
          legend.position = "top",
          legend.text = element_text(size = 10, color = "grey20"),
          axis.text = element_text(size = 10, color = "grey20"),
          axis.title = element_text(size = 10, color = "grey20")) +
    labs(x = "Trial stage", y = "Rate of response")
  
}, error = function(cond) {
  return(cond)
}
)

if (any_error(org_summ)) {
  
  partiplot <- 0L
  itemtable <- data.frame()
  tbl_section1 <- data.frame()
  
}

# .......................................................
# .......................................................
# 4. Make map ####
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
  e <- org_lonlat$message
  error <- c(error, e)
  geoTRUE <- FALSE
  trial_map_statement <- ""
}

# .......................................................
# .......................................................
# 5. Correlation between traits and the reference #####
org_kendall <- tryCatch({
  
  if (isTRUE(nothertraits > 0)) {
    
    # filter cmdata so it matches the dims in all traits
    keep <- do.call(cbind, lapply(trait_list, function(x) x$keep))
    
    keep <- rowSums(keep)
    keep <- keep == length(trait_list)
    
    # list of rankings
    compare <- list()
    
    for (k in seq_along(c("all", groups))) {
      
      cp <- list()
      
      if (k == 1) keep_k <- keep
      if (k != 1) keep_k <- keep & cmdata$group == groups[k - 1]
      
      for (j in seq_along(trait_list)) {
        a <- list(cmdata[keep_k, ],
                  items = itemnames,
                  input = c(trait_list[[j]]$strings))
        
        cp[[j]] <- do.call(rankwith, args = a)
        
      }
      
      compare[[k]] <- cp
      
    }
    
    agreement <- lapply(compare, function(x) {
      summarise_agreement(x[[reference_trait]], 
                          x[-reference_trait], 
                          labels = traits_names[-reference_trait])
    })
    
    agreement <- do.call(rbind, agreement)
    
    agreement$group <- rep(c("Whole group", groups), each = nothertraits)
    
    agreement$group <- factor(agreement$group, levels = c("Whole group", groups))
    
    agreement$labels <- factor(agreement$labels, levels = traits_names[-reference_trait])
    
    # Plot kendall tau
    agreement$kendall[agreement$kendall < 0] <- 0
    agreement$kendall <- agreement$kendall / 100
    
    pagreement <- 
      ggplot(agreement,
             aes(x = kendall, y = labels, fill = group)) +
      geom_bar(stat = "identity", position = "dodge", show.legend = FALSE) +
      facet_wrap(. ~ group) +
      scale_x_continuous(labels = c(0, 0.25, 0.50, 0.75, 1), 
                         breaks = c(0, 0.25, 0.50, 0.75, 1), 
                         limits = c(0, 1)) +
      labs(x = "", y = "") +
      theme_minimal() +  
      theme(legend.text = element_text(color = "grey20"),
            axis.text = element_text(color = "grey20"),
            strip.text.x = element_text(color = "grey20"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
      scale_fill_manual(values = col_pallet(length(groups) + 1))
    
    
    agreement <- agreement[agreement$group == "Whole group", ]
    
    strongest_link <- c(agreement[[which.max(agreement$kendall), "labels"]],
                        round(max(agreement$kendall), 3))
    
    
    weakest_link   <- c(agreement[[which.min(agreement$kendall), "labels"]],
                        round(min(agreement$kendall), 3))
    
    agreement <- agreement[,-5]
    
    agreement[c(3:4)] <- lapply(agreement[c(3:4)], function(x) as.character(round(x, 1)))
    
    agreement[,2] <- as.character(round(agreement[,2], 3))
    
    names(agreement) <- c("Trait", "Kendall tau", "Agreement with best (%)", "Agreement with worst (%)")
    
  } 
  
  if (isTRUE(nothertraits == 0)) {
    strongest_link <- character()
    weakest_link <- character()
    pagreement <- 0L
    agreement <- data.frame()
 }
  
}, error = function(cond) {
  return(cond)
}
)

if (any_error(org_kendall)) {
  e <- org_kendall$message
  error <- c(error, e)
  
  strongest_link <- character()
  weakest_link <- character()
  pagreement <- 0L
  agreement <- data.frame()
}

# .......................................................
# .......................................................
# 6. Fit PlackettLuce model ####
# This will use the rankings from each trait 
# to fit a PlackettLuce model and do
# some exploratory analysis like favorability
# and a table showing statistical differences for each trait 
org_pl <- tryCatch({
  
  #...........................................................
  # first a list with rankings
  R <- list()
  
  for (i in seq_along(trait_list)) {
    
    keep <- trait_list[[i]]$keep
    
    # list of arguments for the function that will be used to 
    # create the rankings
    a <- list(cmdata[keep, c(itemnames, trait_list[[i]]$strings)],
              items = itemnames,
              input = trait_list[[i]]$strings)
    
    R[[i]] <- do.call(rankwith, args = a)
    
    # get the network of items evaluated in the project
    #if (i == reference_trait) {
      
      if (isTRUE(tricotVSlocal)) {
        
        keep <- trait_list[[reference_trait]]$keep2 & trait_list[[i]]$keep
        
        a <- list(cmdata[keep, c(itemnames, 
                                 trait_list[[i]]$strings, 
                                 trait_list[[reference_trait]]$tricotVSlocal)],
                  items = itemnames,
                  input = trait_list[[i]]$strings,
                  additional.rank = cmdata[keep, trait_list[[reference_trait]]$tricotVSlocal])
        
        R[[i]] <- do.call(rankwith, args = a)
        
      }
    #}
  }
  
  #...........................................................
  # Favorability scores showing a summary of the most 
  # voted items
  fav_tbl <- lapply(R, function(x){
    summarise_favorite(x)
  })
  
  fav_plot <- lapply(fav_tbl, function(x){
    plot(x, abbreviate = FALSE) +
      xlab("") +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 10, color = "grey20"),
            axis.text.y = element_text(size = 10, color = "grey20"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "none")
  })
  
  # Change the tables to be report friendly 
  fav_tbl <- lapply(fav_tbl, function(x){
    
    x$best <- paste0(round(x$best, 1), "%")
    x$worst <- paste0(round(x$worst, 1), "%")
    x$fav_score <- round(x$fav_score, 1)
    
    x <- x[,-which(grepl("wins", names(x)))]
    
    names(x) <- c(Option,"N","Top ranked","Bottom ranked",
                       "Net favorability score")
    
    x
    
  })
  
  fav_traits <- list(fav_tbl, fav_plot)
  
  #...........................................................
  # Fit Plackett-Luce model 
  mod <- lapply(R, function(x){
    PlackettLuce(x)
  })
  
  #...........................................................
  # Analysis of variance
  anovas <- lapply(mod, function(x){
    a <- anova.PL(x)
    a
  })
  
  # and the tables with rounded p-values and sig stars
  aov_tbl <- list()
  for (i in seq_along(trait_list)) {
    a <- anovas[[i]]
    a[2, "model"] <- trait_list[[i]]$name
    a[,5] <- paste(formatC(a[,5], format = "e", digits = 2),
                   stars.pval(a[,5]))
    aov_tbl[[i]] <- a
  }
  
  # Plot with log-worth and multi comparison analysis 
  logworth_plot <- lapply(mod, function(x){
    
    # table with multi comparison analysis 
    summ <- multcompPL(x,
                       ref = reference,
                       threshold = sig_level, 
                       adjust = ci_adjust)
    
    # and this is the chart with multi comparison analysis
    plot_logworth(summ, level = ci_level) + 
      theme_classic() +
      theme(text = element_text(color = "grey20"),
            axis.text.x = element_text(size = 10, color = "grey20"),
            axis.text.y = element_text(size = 10, color = "grey20")) +
      labs(y = Option, x = "Log-worth")
    
  })
  
  #...........................................................
  # Bar plot with worth parameters for each trait
  worth_plot <- lapply(mod, function(x){
    plot_worth(x)
  })
  
  #...........................................................
  # Table summarizing the best and worst items per trait
  # and their respective level of statistical significance
  overview_mod <- lapply(mod, function(x) {
    ps <- anova.PL(x)[2,5]
    
    summ <- multcompPL(x,
                       ref = reference,
                       threshold = sig_level, 
                       adjust = ci_adjust)
    
    # take the best three items from this comparison
    bests <- as.character(summ[, "term"][grepl("a", summ[,"group"])])
    # if more than three, subset to get only three
    if (isTRUE(length(bests) > 3)) {
      bests <- bests[1:3]
    }
    
    # get the three worst items
    worsts <- as.character(rev(summ[grepl(summ[nrow(summ),"group"], 
                                            summ[, "group"]), "term"]))
    
    # if more than three, subset to get only three
    if (isTRUE(length(worsts) > 3)) {
      worsts <- worsts[1:3]
    }
    
    # avoid bests and worst in the same column, this when very few items are tested
    b <- bests[!bests %in% worsts]
    w <- worsts[!worsts %in% bests]
    
    bests <- paste(b, collapse =", ")
    worsts <- paste(w, collapse = ", ")
    
    tbl <- data.frame(trait = "",
                      dc = "",
                      b = bests,
                      w = worsts,
                      p = ps)
    
  })
  
  for (i in seq_along(trait_list)) {
    overview_mod[[i]][,1] <- ClimMobTools:::.title_case(trait_list[[i]]$name)
    overview_mod[[i]][,2] <- ClimMobTools:::.title_case(trait_list[[i]]$assessment)
  }
  
  overview_mod <- do.call("rbind", overview_mod)
  
  overview_mod[,6] <- stars.pval(overview_mod[,5])
  
  overview_mod[,5] <- formatC(overview_mod[,5], format = "e", digits = 2)
  
  names(overview_mod) <- c("Trait", "Data collection moment", 
                           "Best performance", "Worst performance", 
                           "Pr(>Chisq)", "")
  
  #...........................................................
  # Head to head visualization of each item by trait
  worthmap <- winprob_map(mod[-reference_trait],
                          traits = traits_names[-reference_trait])
  
  
  
}, error = function(cond) {
  return(cond)
}
)

if (any_error(org_pl)) {
  e <- org_pl$message
  error <- c(error, e)
  
  logworth_plot <- list()
  mod <- list()
  worth_plot <- list()
  anovas <- list()
  aov_tbl <- list()
  worthmap <- 0L
  
}

# .......................................................
# .......................................................
# 7. Fit PLADMM model ####
# this will try to fit a simple PLDMM for the overall trait
# using the log-worth from the other traits as a reference
org_pladmm <- tryCatch({
  
  if (nothertraits > 0) {
    
    features <- lapply(mod, function(x){
      x <- itempar(x, log = TRUE, vcov = FALSE)
    })
    
    features <- do.call(cbind, features)
    
    # remove features corresponding to the reference trait
    features <- as.data.frame(features)
    
    features <- features[,-reference_trait]
    
    otn <- rename_duplicates(trait_names[-reference_trait])
    
    # add name of features
    names(features) <- otn
    
    # add column with item names
    features <- cbind(items = rownames(features), features)
    
    rownames(features) <- 1:nrow(features)
    
    # remove traits with high correlation
    cormat <- cor(features[-1])
    
    rmcor <- findCorrelation(cormat, cutoff = 0.7)
    
    # if too many traits, take the last 10
    # this to prevent issues with time out
    if (length(otn) > 10) {
      otn <- otn[rev(length(otn):(length(otn)-10))]
    }
    
    f <- as.formula(paste("~ ", paste(otn[-rmcor], collapse =" + ")))
    
    plad1 <- pladmm(R[[reference_trait]], f, data = features)
    
    plad1 <- pladmm_coeffs(plad1)
    
    # get the names of traits with significant influence to the main trait
    trait_to_overall <- suppressWarnings(as.numeric(plad1[,5]) <= sig_level)
    
    trait_to_overall[is.na(trait_to_overall)] <- FALSE
    
    trait_to_overall <- plad1[trait_to_overall, 1]
    
    trait_to_overall <- paste3(trait_to_overall)
    
    isPLADMM <- TRUE
    
  }else{
    isPLADMM <- FALSE
  }
  
}, error = function(cond) {
  return(cond)
}
)

if (any_error(org_pladmm)) {
  
  e <- org_pladmm$message
  
  error <- c(error, e)
  
  isPLADMM <- FALSE

}


# .......................................................
# .......................................................
# 8. Fit pltree ####
# organize covariates and prepate to fit pltree()
# outputs of pltree(), if any, are also processed for further insights
org_pltree <- tryCatch({
  
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
    # if values are character or factor, add "Unkown" for any missing
    # if numeric or integer, add the median
    cmdata[, strings] <- lapply(cmdata[, strings], function(x){
      
      if (class(x) == "integer" | class(x) == "numeric") {
        x[is.na(x)] <- median(x, na.rm = TRUE)
      }
      
      if (class(x) == "numeric") {
        x <- round(x, 2)
      }
      
      if (class(x) == "character") {
        x <- ifelse(is.na(x), "Unknown", x)
        x <- as.factor(x)
      }
      
      # if a factor with more than 10 classes then omit it
      if (class(x) == "factor") {
        lv <- levels(x)
        if (length(lv) > 10) {
          x <- NA
        }
      }
      
      x
      
    })
    
    # Do another check for NAs, but this time covariates with NAs 
    # will be removed
    keep <- NULL
    for(i in seq_along(strings)){
      k <- !is.na(cmdata[, strings[i]])
      keep <- cbind(keep, k)
    }
    
    # find those that are above the threshold of missexp
    dropit <- mincovar > (colSums(keep) / nranker)
    
    # drop those bellow threshold
    keep <- as.data.frame(keep[, !dropit])
    
    # create a single vector that will be used to filter cmdata
    keep <- rowSums(keep)
    keep <- keep == max(keep)
    
    covar_dropped <- covar$name[dropit]
    covar <- covar[!dropit, ]
   
    trait_list[[reference_trait]]$keep_covariate <- keep
    
    # if no covariate left out put a pseudo variable
    if(isTRUE(dim(covar)[[1]] == 0)) {
      
      covarTRUE <- FALSE
      
    }
    
  }
  
  if (isFALSE(covarTRUE)) {
    
    cmdata$Intercept <- rep(0, nranker)
    
    trait_list[[reference_trait]]$keep_covariate <- rep(TRUE, nranker)
    
    covar <- data.frame(codeQst = "xinterceptx", 
                        nameString = "Intercept",
                        name = "Intercept-only model",
                        questionAsked = "",
                        assessmentName = "",
                        assessmentId = "")
  }
  
  if (isTRUE(tricotVSlocal)) {
    
    keep <- trait_list[[reference_trait]]$keep2 & trait_list[[reference_trait]]$keep_covariate
    
    a <- list(cmdata[keep, c(itemnames, trait_list[[reference_trait]]$strings, 
                             trait_list[[reference_trait]]$tricotVSlocal)],
              items = itemnames,
              input = trait_list[[reference_trait]]$strings,
              additional.rank = cmdata[keep, trait_list[[reference_trait]]$tricotVSlocal], 
              group = TRUE)
    
    G <- do.call(rankwith, args = a)
    
    
  } 
  
  if (isFALSE(tricotVSlocal)) {
    
    keep <- trait_list[[reference_trait]]$keep & trait_list[[reference_trait]]$keep_covariate
    
    a <- list(cmdata[keep, c(itemnames, trait_list[[reference_trait]]$strings)],
              items = itemnames,
              input = trait_list[[reference_trait]]$strings, 
              group = TRUE)
    
    G <- do.call(rankwith, args = a)
    
  }
  
  
  # make the plot with the network
  net <- network(G)
  
  # data frame of explanatory variables
  Gdata <- as.data.frame(cmdata[keep, c(covar$nameString)], stringsAsFactors = TRUE)
  nvar <- length(covar$nameString)
  
  # rename covariates with the name taken from ClimMob
  names(Gdata) <- c(covar$codeQst)
  Gdata <- cbind(G, Gdata)
  nG <- nrow(Gdata)
  rownames(Gdata) <- 1:nG
  
  # perform a forward selection as pltree() sometimes don't split 
  # when G ~ . is used
  var_keep <- character(0L)
  best <- TRUE
  counter <- 1
  exp_var <- covar$codeQst
  
  cat("Selecting the best covariate for Plackett-Luce trees \n")
  
  while (best) {
    
    fs <- length(exp_var)
    models <- data.frame()
    
    for(i in seq_len(fs)){
      
      t_i <- try(pltree(as.formula(paste0("G ~ ", paste(c(var_keep, exp_var[i]), collapse = " + "))),
                        data = Gdata,
                        minsize = minsplit,
                        alpha = sig_level_tree,
                        ref = reference,
                        gamma = TRUE), silent = TRUE)
      
      if (isFALSE("try-error" %in% class(t_i))) {
        validations <- data.frame(nnodes = length(nodeids(t_i, terminal = TRUE)),
                                  AIC = AIC(t_i),
                                  noerror = !"try-error" %in% class(try(itempar(t_i, vcov = TRUE), silent = TRUE)))
      }else{
        validations <- data.frame(nnodes = NA,
                                  AIC = NA,
                                  noerror = FALSE)
      }
      
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
    
    if (length(exp_var) == 0) {
      best <- FALSE
    }
    
  }
  
  if (length(var_keep) == 0) {
    var_keep <- names(Gdata)[2]
    minsplit <- nG
  }
  
  treeformula <- as.formula(paste0("G ~ ", paste(c(var_keep), collapse = " + ")))
  
  # now fit the tree with the selected covariates
  tree_f <- pltree(treeformula,
                   data = Gdata,
                   minsize = minsplit,
                   gamma = TRUE,
                   alpha = sig_level_tree,
                   ref = reference)
  
  isTREE <- isTRUE(length(tree_f) > 1)
  
  # if the tree has splits, extract coeffs from nodes
  if (isTREE) { 
    
    # plot the tree (if any)
    plottree <- gosset:::plot_tree(tree_f, ci.level = ci_level)
    
    node_ids <- nodeids(tree_f, terminal = TRUE)
    
    coefs_tree <- data.frame()
    
    for(i in seq_along(node_ids)){
      
      coef_i <- data.frame(node = node_ids[i],
                           multcompPL(tree_f[[ node_ids[i] ]]$node$info$object, 
                                      ref = reference,
                                      threshold = sig_level_tree, adjust = ci_adjust),
                           n = tree_f[[ node_ids[i] ]]$node$info$nobs,
                           stringsAsFactors = FALSE)
      
      coefs_tree <- rbind(coefs_tree, coef_i)
      
    }
    
    # remove the strings %in% " and c() from rules 
    coefs_tree <- split(coefs_tree, coefs_tree$node)
    
    coefs_tree <- lapply(coefs_tree, function(x){
      x$m <- mean(x$estimate)
      x$ctd <- x$estimate - x$m
      x
    })
    
    coefs_tree <- do.call(rbind, coefs_tree)
    
    best_tree <- data.frame()
    
    for(i in seq_along(node_ids)){
      
      tmp <- coefs_tree[coefs_tree$node == node_ids[i], ]
      
      best_tree <- rbind(best_tree,
                         data.frame(node = tmp$node[1],
                                    n = tmp$n[1], 
                                    best = paste(tmp$term[tmp$group %in% "a"], collapse=", "),
                                    worst = paste(rev(tmp$term[grepl(tmp$group[nrow(tmp)], tmp$group)]), collapse=", ")))
    }
    
    
    rules <- node_rules(tree_f)
    
    node_summary <- merge(best_tree, rules, by = "node")
    
    names(node_summary) <- c("Node", "N", "Best ranked","Worst ranked", "Split rule")
    
  }
  
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
    sel <- c("id", paste0("package_item_", LETTERS[1:ncomp]))
    partitable <- cmdata[, sel]
    
    partitable$name <- cmdata[,which(grepl("package_participant_name|package_farmername", names(cmdata)))]
    
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
    pover <- coef(mods[[reference_trait]], log = FALSE)
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

# End of analysis


