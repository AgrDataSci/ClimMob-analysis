#' This module produces the summary tables and trial
#' overview, with information of traits assessed 
#' frequency of participation and technologies 
#' evaluated by participants
#' 
#' @param rank_dat list with parameters to deal with cmdata
#' @param cmdata a data frame with the ClimMob data
get_overview_summaries <- function(cmdata, rank_dat) {
  
  trait_list <- rank_dat[["trait_list"]]
  nranker <- rank_dat[["nranker"]]
  option <- rank_dat[["option"]]
  group <- rank_dat[["group"]]
  technologies_index <- rank_dat[["technologies_index"]]
  reference_trait_index <- rank_dat[["reference_trait_index"]]
  
  
  # this table presents the summaries with number of traits assessed
  tbl_section1 <- lapply(trait_list, function(x){
    data.frame(name = x$name,
               collect = title_case(x$assessment),
               quest = x$question,
               n = sum(x$keep))
  })
  
  tbl_section1 <- do.call("rbind", tbl_section1) 
  
  # rename rows and columns in the original table
  row.names(tbl_section1) <- 1:nrow(tbl_section1)
  
  names(tbl_section1) <- c("Trait", "Data collection moment", 
                           "Question asked", "Number of valid answers")
  
  #...........................................................
  # Number of items tested
  # This table show the frequencies where items were tested
  # and how these frequencies were tested among groups, if any
  itemtable <- cmdata[, technologies_index]
  
  itemtable <- data.frame(table(unlist(itemtable)))
  
  itemtable$x <- with(itemtable,
                      round((Freq / nranker * 100), 1))
  
  itemtable$x <- with(itemtable,
                      paste0(x, "%"))
  
  names(itemtable) <- c(title_case(option), "Freq", "Relative freq")
  
  # check if any group is provided so it can be added to the itemtable
  if (isTRUE(length(group) > 0)) {
    
    x <- unlist(cmdata[, technologies_index])
    
    ngroups <- table(group)
    
    grouptbl <- c()
    
    for (i in seq_along(ngroups)) {
      grouptbl <- cbind(grouptbl, 
                        tapply(rep(group, length(technologies_index)), x, function(x) {
                          sum(x == group[i], na.rm = TRUE)
                        }))
    }
    
    grouptbl <- as.data.frame(grouptbl)
    
    names(grouptbl) <- paste0(names(ngroups), " (n=", ngroups, ")")
    
    itemtable <- cbind(itemtable, grouptbl)
    
    rm(x)
    
  }
  
  itemtable$Abbreviation <- reduce(as.character(itemtable[, title_case(option)]), 
                                   nchars = 12,
                                   minlength = 12)
  
  itemtable <- itemtable[union(c(title_case(option), "Abbreviation"), names(itemtable))]
  
  rownames(itemtable) <- 1:nrow(itemtable)
  
  #...........................................................
  # Participation during trial (response rate)
  # it is a plot showing the rate of response in each 
  # data collection moment, it takes the larger N response
  # for each data collection moment
  regsname <- "Trial-package Delivery"
  
  participation <- data.frame(n = nrow(cmdata),
                              n_tot = nrow(cmdata),
                              group = "Whole group",
                              dc = regsname)
  
  for(i in seq_along(trait_list)) {
    
    part <- data.frame(n = sum(trait_list[[i]]$keep),
                       n_tot = nrow(cmdata),
                       group = "Whole group",
                       dc = trait_list[[i]]$assessment)
    
    participation <- rbind(participation, part)
    
  }
  
  if (isTRUE(length(group) > 0)) {
    
    ngroups <- table(group)
    
    participation2 <- data.frame(n = as.vector(table(group)),
                                 n_tot = as.vector(table(group)),
                                 group = names(table(group)),
                                 dc = regsname)
    
    for(i in seq_along(trait_list)) {
      
      n <- table(group[trait_list[[i]]$keep])
      
      p <- data.frame(group = names(ngroups),
                      dc = trait_list[[i]]$assessment)
      
      part <- data.frame(n = as.vector(n),
                         n_tot = as.vector(table(group)),
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
  
  participation <- do.call("rbind", participation)
  
  # transform into proportion to make it easier to visualize
  participation$value_perc <- participation$n / participation$n_tot
  
  participation$dc <- title_case(participation$dc)
  
  participation$dc <- factor(participation$dc, levels = c(regsname, 
                                                          title_case(rank_dat$assessment_order)))
  
  participation$group <- factor(participation$group, levels = c("Whole group",
                                                                unique(group)))
  
  partiplot <- 
    ggplot(participation, aes(x = dc, y = value_perc, 
                              group = group, color = group)) +
    geom_line(linewidth = 1) +
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
  
  
  # chart with trial connectivity (network)
  keep <- trait_list[[reference_trait_index]]$keep
  
  a <- list(cmdata[keep, c(technologies_index, trait_list[[reference_trait_index]]$strings)],
            items = technologies_index,
            input = trait_list[[reference_trait_index]]$strings, 
            group = TRUE)
  
  G <- do.call("rankTricot", args = a)
  
  trial_connectivity <- network(G)
  
  result <- list(partipation_plot = partiplot,
                 summary_table_trait = tbl_section1,
                 summary_table_tech = itemtable,
                 trial_connectivity = trial_connectivity)
  
  return(result)
  
}

# .......................................
# Error in data 
# this is a file that is generated to be used in case of errors
error_data_overview_and_summaries <- list(partipation_plot = 0L,
                                          summary_table_trait = data.frame(),
                                          summary_table_tech = data.frame(),
                                          trial_connectivity = 0L)


