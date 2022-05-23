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
#'        
organize_ranking_data <- function(cmdata, 
                                  pars, 
                                  groups = NULL,
                                  tech_index = c("package_item_A", "package_item_B", "package_item_C")) {
  
  ntech <- length(tech_index)
  
  quanti_traits <- pars[["linear"]]
  
  # check if a request to split the data by groups (segments)
  # (gender, location, etc.) is provided
  if (length(groups) > 0) {
    
    group_index <- integer()
    
    for(i in seq_along(groups)) {
      
      group_index <- c(group_index, which(grepl(groups[i], names(cmdata)))[1])
      
    }
    
    group_index <- group_index[!is.na(group_index)]
    
    groups <- names(cmdata)[group_index]
    
    if (length(groups) == 1) {
      
      group <- cmdata[, groups]
      
      if (sum(is.na(group)) > (length(group) * 0.5)) {
        
        group <- NULL
        
      } else {
        
        group <- ifelse(is.na(group), "Others", group)
        
      }
      
    }
    
    if (length(groups) > 1) {
      
      group <- cmdata[,groups]
      
      group[is.na(group)] <- "Others" 
      
      group <- apply(group, 1, function(x) {paste(x, collapse = " - ")})
      
    }
    
    # We can only handle 9 groups. The ones with fewer data will 
    # be aggregated and assigned to "Others"
    ngroups <- unique(group)
    
    if (ngroups > 8) {
      
      keep_group <- names(rev(sort(table(group)))[1:8])
      
      group[!group %in% keep_group] <- "Others"
      
    }
    
  }
  
  if (length(groups) == 0) {
    group <- NULL
  }
  
  # rename traits to avoid duplicated strings, in case the same 
  # trait is tested in different data collection moments 
  quanti_traits$codeQst <- rename_duplicates(quanti_traits$codeQst)
  quanti_traits$name    <- rename_duplicates(quanti_traits$name, sep = " ")
  
  quanti_dat <- list()
  
  # run over traits and get it in long format (unlist)
  for(i in seq_along(quanti_traits$codeQst)){
    
    quanti <- data.frame(id = as.vector(unlist(cmdata[, "id" ])),
                         technology = as.vector(unlist(cmdata[, tech_index])), 
                         group = rep(group, ntech))
    
    strings_i <- as.vector(unlist(quanti_traits[i, paste0("nameString", 1:ntech)]))
    
    index_i <- as.vector(sapply(strings_i,  function(x){
      which(grepl(x, names(cmdata)))
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
  
  # put colnames
  names(quanti)[-c(1:2)] <- quanti_traits$codeQst
  
  # identify outliers
  outliers <- data.frame()
  
  for(i in seq_along(quanti_traits$codeQst)){
    
    index_i <- quanti_traits$codeQst[i]
    
    out_values_i <- unique(boxplot.stats(quanti[,index_i])$out)
    
    out_i <- quanti[,index_i] %in% out_values_i
    
    quanti[out_i, index_i] <- NA
    
    outliers_i <- quanti[out_i, c("id", "items")]
    
    if (nrow(outliers_i) == 0) next
    
    outliers_i$values <- quanti[out_i, index_i]
    
    outliers_i$trait <- quanti_traits$name[i]
    
    outliers_i$assessment <- quanti_traits$assessmentName[i]
    
    outliers <- rbind(outliers, outliers_i)
    
  }
  
  rownames(outliers) <- NULL
  
}
