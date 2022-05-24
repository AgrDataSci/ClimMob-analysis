if (nothertraits > 0) {
  
  features <- gosset:::.combine_coeffs(mod, log = TRUE, vcov = FALSE)
  
  features <- features[,-reference_trait]
  
  otn <- names(trait_list)[-reference_trait]
  
  otn2 <- rename_duplicates(traits_names[-reference_trait])
  
  # add name of features
  names(features) <- otn
  
  # add column with item names
  features <- cbind(items = rownames(features), features)
  
  rownames(features) <- 1:nrow(features)
  
  if (length(otn) > 3) {
    
    # remove traits with high correlation
    cormat <- cor(features[-1])
    
    rmcor <- findCorrelation(cormat, cutoff = 0.7)
    
    if (length(rmcor) > 0 & length(rmcor) < (length(otn) - 2)) {
      otn  <- otn[-rmcor]
      
      otn2 <- otn2[-rmcor]
      
    }
    
  }
  
  # if too many traits, take the last 10
  # this to prevent issues with time out
  if (length(otn) > 10) {
    
    otn <- otn[rev(length(otn):(length(otn)-10))]
    
    otn2 <- otn2[rev(length(otn2):(length(otn2)-10))]
    
  }
  
  # names of traits that went out, if any 
  otn_rmv <- rename_duplicates(trait_names[-reference_trait])
  
  otn_rmv <- otn_rmv[!otn_rmv %in% otn]
  
  # formula to fit PLADMM
  f <- as.formula(paste(" ~ ", paste(otn, collapse = " + ")))
  
  cat("Fitting PLADMM \n")
  
  plad1 <- pladmm(R[[reference_trait]], f, data = features)
  
  plad1 <- pladmm_coeffs(plad1)
  
  # replace the trait code by its name to be easier to read
  plad1[-1, 1] <- otn2
  
  # get the names of traits with significant influence to the main trait
  trait_to_overall <- suppressWarnings(as.numeric(plad1[,5]) <= sig_level)
  
  trait_to_overall[is.na(trait_to_overall)] <- FALSE
  
  trait_to_overall <- plad1[trait_to_overall, 1]
  
  trait_to_overall <- paste3(trait_to_overall)
  
  isPLADMM <- TRUE
  
}else{
  isPLADMM <- FALSE
}
