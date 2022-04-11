#' PlackettLuce with condensed rankings
#' 
#' This function will group the rankings by each trait 
#' using the index argument in PlackettLuce::group() 
#' to assess genotype performance conmbining all 
#' the traits together
#' 
#' @param dat the dataframe with trial data
#' @param basetrait the index in \var{dat} for the base trait
#' @param othertraits a vector with the indices for the other traits in \var{dat}
#' @param traitsL a list with trait parameters from organizeRank() 
#' @param itemnames a vector with the indices of item columns in \var{dat}
condensedRanking <- function(dat, traitsL, basetrait, othertraits, itemnames){
  
  othertraits <- union(basetrait, othertraits)
  indicesbase <- as.vector(which(traitsL[[basetrait]]$keep))
  resetindices <- 1:length(indicesbase)
  
  R <- list()
  index <- c()
  
  for(i in seq_along(othertraits)) {
    
    trait_i <- which(names(traitsL) %in% othertraits[i])
    
    # this should be combined with the baseline trait
    index_i <- as.vector(which(traitsL[[trait_i]]$keep))
    
    keep_i <- index_i %in% indicesbase
    
    index_i <- index_i[keep_i]
    
    r_i <- rankTricot(dat[index_i, ],
                      itemnames,
                      c(traitsL[[trait_i]]$strings),
                      group = FALSE)
    
    # reset indices to match with grouped_rankings later
    index_i <- resetindices[indicesbase %in% index_i]
    
    index <- c(index, index_i)
    
    R[[i]] <- r_i
    
  }
  
  R <- do.call("rbind", R)
  
  GC <- group(R, index = index)
  
  return(GC)
  
}



