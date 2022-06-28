#' This Module organizes the quantitative data from a ClimMob project
#' 
#' Organize the quantitative data using internal parameters and
#' the parameters sent by ClimMob to get the data read for analysis 
#' 
#' @param pars a list with parameters sent by ClimMob
#' @param cmdata a data frame with the ClimMob data
#' @param tech_index a vector of characters for the indices in cmdata
#'  with the technologies
#' @param groups a vector with characters for the index in cmdata indicating 
#'  columns to aggregate and make segments of participants 
organize_quantitative_data <- function(cmdata, 
                                       pars, 
                                       groups = NULL,
                                       id = "id",
                                       tech_index = c("package_item_A", "package_item_B", "package_item_C")) {
  
  ntech <- length(tech_index)
  
  quanti_traits <- pars[["linear"]]
  
  # check if a request to split the data by groups (segments)
  # (gender, location, etc.) is provided
  if (isTRUE(length(groups) > 0)) {
    
    group_index <- integer()
    
    for(i in seq_along(groups)) {
      
      group_index <- c(group_index, which(grepl(groups[i], names(cmdata)))[1])
      
    }
    
    group_index <- group_index[!is.na(group_index)]
    
    groups <- names(cmdata)[group_index]
    
    if (isTRUE(length(groups) == 0)) {
      
      group <- NULL
      
    }
    
    if (isTRUE(length(groups) == 1)) {
      
      group <- cmdata[, groups]
      
      if (sum(is.na(group)) > (length(group) * 0.5)) {
        
        group <- NULL
        
      } else {
        
        group <- ifelse(is.na(group), "Others", group)
        
      }
      
    }
    
    if (isTRUE(length(groups) > 1)) {
      
      group <- cmdata[,groups]
      
      group[is.na(group)] <- "Others" 
      
      group <- apply(group, 1, function(x) {paste(x, collapse = " - ")})
      
    }
    
    # We can only handle 9 groups. The ones with fewer data will 
    # be aggregated and assigned to "Others"
    ngroups <- unique(group)
    
    if (isTRUE(ngroups > 8)) {
      
      keep_group <- names(rev(sort(table(group)))[1:8])
      
      group[!group %in% keep_group] <- "Others"
      
    }
    
  }
  
  if (length(groups) == 0) {
    group <- NA
  }
  
  # rename traits to avoid duplicated strings, in case the same 
  # trait is tested in different data collection moments 
  quanti_traits$codeQst <- rename_duplicates(quanti_traits$codeQst)
  quanti_traits$name    <- title_case(rename_duplicates(quanti_traits$name, sep = " "))
  
  quanti_dat <- list()
  
  # run over traits and get it in long format (unlist)
  for(i in seq_along(quanti_traits$codeQst)){
    
    quanti <- data.frame(id = as.vector(unlist(cmdata[, id ])),
                         technology = as.vector(unlist(cmdata[, tech_index])), 
                         group = rep(group, ntech))
    
    strings_i <- as.vector(unlist(quanti_traits[i, paste0("nameString", 1:ntech)]))
    
    index_i <- as.vector(sapply(strings_i,  function(x){
      which(grepl(tolower(x), tolower(names(cmdata))))
    }))
    
    # find the enumerator that submitted the data
    enumerator <- grepl(paste0("ASS", quanti_traits$assessmentId[i]), names(cmdata))
    enumerator <- names(cmdata)[enumerator]
    enumerator <- enumerator[grepl("submitted_by", enumerator)]
    
    quanti <- cbind(quanti, 
                    value = as.numeric(unlist(cmdata[,index_i])), 
                    trait = quanti_traits$name[i],
                    trait_code = quanti_traits$codeQst[i],
                    data_collection = quanti_traits$assessmentName[i],
                    enumerator = rep(cmdata[, enumerator], ntech))
    
    quanti_dat[[i]] <- quanti
    
  }
  
  # identify outliers
  # TO DO: add technology label (A, B, C)
  outliers <- lapply(quanti_dat, function(x){
    
    out_values_i <- unique(boxplot.stats(x[,"value"])$out)
    
    out_i <- x[,"value"] %in% out_values_i

    outliers_i <- x[out_i, ]
    
  })
  
  outliers <- do.call("rbind", outliers)
  
  # sort outliers by id
  if (nrow(outliers) > 1) {
    outliers <- outliers[order(as.integer(outliers$id)), ]
    rownames(outliers) <- NULL
  }
  
  
  if (isTRUE(length(group) == 1)) {
    group <- NULL
  }
  
  result <- list(quantitative = TRUE,
                 quanti_dat = quanti_dat,
                 group = group,
                 outliers = outliers)
  
  return(result)
  
}

# .......................................
# Error in data 
# this is a file that is generated to be used in case of errors
error_data_quanti_dat <- list(quantitative =  FALSE, 
                              quanti_dat = list(),
                              group = NULL,
                              outliers = data.frame())
