#' This module computes kendallTau
#' 
#' 
#' @param cmdata a data frame with the ClimMob data
#' @param rank_dat a list with parameters
get_kendallTau = function(cmdata, rank_dat) {
  
  trait_list = rank_dat[["trait_list"]]
  option = rank_dat[["option"]]
  technologies_index = rank_dat[["technologies_index"]]
  technologies = rank_dat[["technologies"]]
  trait_names = rank_dat[["trait_names"]]
  reference_trait_index = rank_dat[["reference_trait_index"]]
  
  #...........................................................
  # Get the Kendall tau from individual trait rankings
  kendall = lapply(trait_list[-reference_trait_index], function(x){
    
    r1 = rank_tricot(cmdata[, c(technologies_index, 
                                trait_list[[reference_trait_index]]$strings)],
                     items = technologies_index,
                     input = trait_list[[reference_trait_index]]$strings,
                     validate.rankings = TRUE)
    
    r2 = rank_tricot(cmdata[, c(technologies_index, x$strings)],
                     items = technologies_index,
                     input = x$strings,
                     validate.rankings = TRUE)
    
    kendall = kendallTau(r1, r2, na.omit = FALSE)
    
    kendall
    
  })
  
  kendall = do.call("rbind", kendall)
  
  kendall$trait = unlist(lapply(trait_list[-reference_trait_index], function(x){
    paste0(x$name, " [" , x$assessment, "]")
  }))
  
  kendall = kendall[rev(order(kendall$kendallTau)), ]
  
  kendall = kendall[, -2]
  
  strongest_link = c(kendall$trait[1],
                     round(kendall$kendallTau[1], 2))
  
  weakest_link = c(kendall$trait[nrow(kendall)],
                   round(kendall$kendallTau[nrow(kendall)], 2))
  
  kendall = kendall[,c("trait", "kendallTau", "Zvalue", "Pr(>|z|)")]
  
  kendall[,"kendallTau"] = round(kendall[,"kendallTau"], 3)
  
  kendall[,"Zvalue"] = round(kendall[,"Zvalue"], 3)
  
  stars = stars.pval(kendall[,"Pr(>|z|)"])
  
  kendall[, "Pr(>|z|)"] = format(kendall[, "Pr(>|z|)"], 
                                 scientific = TRUE,
                                 digits = 3)
  
  kendall[, "Pr(>|z|)"] = paste(kendall[, "Pr(>|z|)"], stars)
  
  kendall = kendall[rev(order(kendall$kendallTau)), ]
  
  kendall$trait = factor(kendall$trait, levels = rev(kendall$trait))
  
  # make a bar plot plot 
  kendall_plot = 
    ggplot(data = kendall, 
           aes(x = kendallTau,
               y = trait, 
               fill = trait)) +
    geom_bar(stat = "identity", 
             position = "dodge",
             show.legend = FALSE,
             width = 1, 
             color = "#ffffff") + 
    scale_fill_manual(values = rev(col_pallet(nrow(kendall)))) +
    theme_classic() +
    theme(legend.position="bottom",
          legend.text = element_text(size = 9),
          axis.text.y = element_text(color = "grey20"),
          axis.text.x = element_text(vjust = 1,
                                     hjust=1, 
                                     color = "grey20")) +
    labs(y = "Trait",
         x = "Kendall tau") 
  
  
  names(kendall) = c("Trait", "Kendall tau", "Z value", "Pr(>|z|)")
  
  row.names(kendall) = 1:nrow(kendall)
  
  
  return(list(isKendall = TRUE,
              kendall = kendall,
              strongest_link = strongest_link, 
              weakest_link = weakest_link,
              kendall_plot = kendall_plot))
  
}


# .......................................
# Error in data 
# this is a file that is generated to be used in case of errors
error_data_kendallTau = list(isKendall = FALSE,
                             kendall = data.frame(),
                             strongest_link = c("", ""), 
                             weakest_link = c("", ""),
                             kendall_plot = 0L)

