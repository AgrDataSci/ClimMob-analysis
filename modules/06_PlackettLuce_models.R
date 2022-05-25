#' This module analyses the ranking data
#' 
#' The analysis is performed iteratively for each trait 
#' retained in the rank_dat object
#' 
#' @param cmdata a data frame with the ClimMob data
#' @param rank_dat a list with parameters
#' @param reference_tech a integer or character indicating the reference technology
get_PlackettLuce_models <- function(cmdata, rank_dat, reference_tech) {
  
  trait_list <- rank_dat[["trait_list"]]
  nranker <- rank_dat[["nranker"]]
  option <- rank_dat[["option"]]
  group <- rank_dat[["group"]]
  technologies_index <- rank_dat[["technologies_index"]]
  technologies <- rank_dat[["technologies"]]
  reference_trait_index <- rank_dat[["reference_trait_index"]]
  comparison_with_local <- rank_dat[["comparison_with_local"]]
  trait_names <- rank_dat[["trait_names"]]
  
  # first a list with rankings
  R <- list()
  
  for (i in seq_along(trait_list)) {
    
    keep <- trait_list[[i]]$keep
    
    # list of arguments for the function that will be used to 
    # create the rankings
    a <- list(cmdata[keep, c(technologies_index, trait_list[[i]]$strings)],
              items = technologies_index,
              input = trait_list[[i]]$strings)
    
    R[[i]] <- do.call("rankTricot", args = a)
    
    if (isTRUE(comparison_with_local)) {
      
      keep <- trait_list[[reference_trait_index]]$keep2 & trait_list[[i]]$keep
      
      a <- list(cmdata[keep, c(technologies_index, 
                               trait_list[[i]]$strings, 
                               trait_list[[reference_trait_index]]$tricotVSlocal)],
                items = technologies_index,
                input = trait_list[[i]]$strings,
                additional.rank = cmdata[keep, trait_list[[reference_trait_index]]$tricotVSlocal])
      
      R[[i]] <- do.call("rankTricot", args = a)
      
    }
  }
  
  #...........................................................
  # Fit Plackett-Luce model 
  mod <- lapply(R, function(x){
    PlackettLuce(x)
  })
  
  if (isTRUE(length(trait_list) > 1)) {
    
    # get the Kendall tau from model coefficients 
    # this is not the best approach but it the one that 
    # works better right now considering that the data is not
    # often completed in all entries
    compare_to <- mod[-reference_trait_index]
    baseline_trait <- coef(mod[[reference_trait_index]], log = FALSE)
    baseline_trait <- baseline_trait[technologies]
    kendall <- lapply(compare_to, function(x){
      x <- coef(x, log = FALSE)
      x <- x[technologies]
      kendallTau(x, baseline_trait)
    })
    
    kendall <- do.call("rbind", kendall)
    
    kendall$trait <- trait_names[-reference_trait_index]
    
    kendall <- kendall[rev(order(kendall$kendallTau)), ]
    
    kendall <- kendall[,-2]
    
    kendall$kendallTau[kendall$kendallTau < 0] <- 0
    
    isKendall <- nrow(kendall) > 1
    
    if (isKendall) {
      
      strongest_link <- c(kendall$trait[1],
                          round(kendall$kendallTau[1], 2))
      
      weakest_link <- c(kendall$trait[nrow(kendall)],
                        round(kendall$kendallTau[nrow(kendall)], 2))
      
      kendall <- kendall[,c(2,1)]
      
      kendall[,2] <- round(kendall[,2], 3)
      
      kendall <- kendall[rev(order(kendall$kendallTau)), ]
      
      kendall$trait <- factor(kendall$trait, levels = kendall$trait)
      
      # make a bar plot plot 
      kendall_plot <- ggplot2::ggplot(data = kendall, 
                                      ggplot2::aes(x = trait,
                                                   y = kendallTau, 
                                                   fill = trait)) +
        ggplot2::geom_bar(stat = "identity", 
                          position = "dodge",
                          show.legend = FALSE,
                          width = 1, 
                          color = "#ffffff") + 
        ggplot2::scale_fill_manual(values = col_pallet(nrow(kendall))) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position="bottom",
                       legend.text = ggplot2::element_text(size = 9),
                       panel.grid.major = ggplot2::element_blank(),
                       axis.text.y = ggplot2::element_text(color = "grey20"),
                       axis.text.x = ggplot2::element_text(angle = 65, vjust = 1,
                                                           hjust=1, color = "grey20")) +
        ggplot2::labs(y = "Kendall tau",
                      x = "Trait") 
      
      
      
    }
    
  }
  
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
    a[2, "Model"] <- trait_list[[i]]$name
    a[,5] <- paste(formatC(a[,5], format = "e", digits = 2),
                   stars.pval(a[,5]))
    aov_tbl[[i]] <- a
  }
  
  #...........................................................
  # Bar plot with worth parameters for each trait
  worth_plot <- lapply(mod, function(x){
    worth_bar(x, ref = reference) +
      labs(y = title_case(option), x = "Worth")
  })
  
  # Plot log worth
  logworth_plot <- lapply(mod, function(x){
    plot_logworth(x, ref = reference_tech, ci.level = 0.5) + 
      labs(y = title_case(option))
  })
  
  #...........................................................
  # Table summarizing the best and worst items per trait
  # and their respective level of statistical significance
  overview_mod <- lapply(mod, function(x) {
    
    ps <- anova.PL(x)[2,5]
    
    summ <- itempar(x, log = TRUE, ref = reference_tech)
    
    # take the best three items from this comparison
    # bests <- as.character(summ[, "term"][grepl("a", summ[,"group"])])
    bests <- names(rev(sort(summ[summ >= 0])))
    # if more than three, subset to get only three
    if (isTRUE(length(bests) > 3)) {
      bests <- bests[1:3]
    }
    
    worsts <- names(sort(summ[summ < 0]))
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
    overview_mod[[i]][,1] <- title_case(trait_list[[i]]$name)
    overview_mod[[i]][,2] <- title_case(trait_list[[i]]$assessment)
  }
  
  overview_mod <- do.call("rbind", overview_mod)
  
  overview_mod[,6] <- stars.pval(overview_mod[,5])
  
  overview_mod[,5] <- formatC(overview_mod[,5], format = "e", digits = 2)
  
  names(overview_mod) <- c("Trait", "Data collection moment", 
                           "Higher performance", "Weaker performance", 
                           "Pr(>Chisq)", "")
  
  #...........................................................
  # Head to head visualization of each technology performance by trait
  worthmap <- worth_map(mod[-reference_trait_index],
                        labels = trait_names[-reference_trait_index],
                        ref = reference_tech) +
    labs(x = title_case(option), y = "Traits")
  
  
  #..........................................................
  # PlackettLuce with aggregated rankings
  # this put the rankings from all traits into a single 
  # grouped rankings to assess "overall technology performance"
  # reference trait must be the first in this vector
  othertraits <- union(names(trait_list[reference_trait_index]),
                       names(trait_list[-reference_trait_index]))
  indicesbase <- as.vector(which(trait_list[[reference_trait_index]]$keep))
  resetindices <- 1:length(indicesbase)
  
  RG <- list()
  
  index <- c()
  
  for(i in seq_along(othertraits)) {
    
    trait_i <- which(names(trait_list) %in% othertraits[i])
    
    # this should be combined with the baseline trait
    index_i <- as.vector(which(trait_list[[trait_i]]$keep))
    
    keep_i <- index_i %in% indicesbase
    
    index_i <- index_i[keep_i]
    
    r_i <- rankTricot(cmdata[index_i, ],
                      technologies_index,
                      c(trait_list[[trait_i]]$strings),
                      group = FALSE)
    
    # reset indices to match with grouped_rankings later
    index_i <- resetindices[indicesbase %in% index_i]
    
    index <- c(index, index_i)
    
    RG[[i]] <- r_i
    
  }
  
  RG <- do.call("rbind", RG)
  
  RG <- group(RG, index = index)
  
  # PlackettLuce of aggregated rankings
  # TO DO: add weights to rankings  
  modRG <- PlackettLuce(RG)
  
  logworth_grouped_rank <- 
    plot_logworth(modRG, ref = reference_tech, ci.level = 0.5) +
    labs(y = title_case(option))
  
  result <- list(PL_models = mod,
                 PL_models_overview = overview_mod,
                 logworth_grouped_rank = logworth_grouped_rank,
                 worthmap = worthmap,
                 logworth_plot = logworth_plot,
                 kendall = list(isKendall = isKendall,
                                strongest_link = strongest_link, 
                                weakest_link = weakest_link,
                                kendall_plot = kendall_plot))
  
}


# .......................................
# Error in data 
# this is a file that is generated to be used in case of errors
error_data_PL_models <- list(PL_models = list(),
                             PL_models_overview = data.frame(),
                             logworth_grouped_rank = 0L,
                             worthmap = 0L,
                             logworth_plot = 0L,
                             kendall = list(isKendall = FALSE,
                                            strongest_link = c("", ""), 
                                            weakest_link = c("", ""),
                                            kendall_plot = 0L))




