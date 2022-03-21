#' Organize the ranking (tricot) data from a ClimMob project
#' 
#' This function organize the data using internal parameters and
#' the parameters sent by ClimMob to get the data read for analysis 
#' @param pars a list with parameters sent by ClimMob
#' @param cmdata a data frame with the ClimMob data

#organize_ranking_data <- function(cmdata, pars, ...){
  
  # Get some info from the data and ClimMob parameters 
  projname     <- cmdata[1, "package_project_name"]
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
  
  # minimum proportion of valid entries in tricot vs local
  # this will be computed based on the valid entries of the reference
  # trait after validations
  mintricotVSlocal <- 0.90
  
  # method for adjustments for confidence intervals and setting widths for comparison. 
  ci_adjust <- "BH"
  
  # confidence interval level for comparison plots with error bars
  ci_level <- 0.84
  
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
  
  dir.create(outputpath, showWarnings = FALSE, recursive = TRUE)
  
  # check if a request to split the data by groups (segments)
  # (gender, location, etc.) is provided
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
    if (any(table(cmdata$group) / nrow(cmdata) < 0.05)) {
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
  
  # check if comparison with local is required
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
    
    # only letters ABC
    ABC <- !as.vector(apply(cmdata[trait_i], 1, function(x) {
      all(x %in% LETTERS[1:3])
    }))
    
    cmdata[ABC, trait_i] <- NA
    
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
  if (length(unique(trait$assessmentName)) > 1) {
    
    traits_names <- as.vector(unlist(lapply(trait_list, function(x) {
      paste0(x$name, " [", ClimMobTools:::.title_case(x$assessment), "]")
    })))
    
  }else{
    
    traits_names <- as.vector(unlist(lapply(trait_list, function(x) {
      x$name
    })))
    
  }
  
  
  traits_code <- as.vector(unlist(lapply(trait_list, function(x) {
    x$code
  })))
  
  # remove any potential special character
  traits_code <- gsub("[[:punct:]]", "", traits_code)
  
  # refresh number of other traits
  nothertraits <- length(trait_list) - 1
  
#result <- list()
#return(result)
#}





