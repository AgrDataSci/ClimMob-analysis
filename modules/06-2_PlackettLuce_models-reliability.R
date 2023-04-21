#' This module analyses the ranking data
#' 
#' The analysis is performed iteratively for each trait 
#' retained in the rank_dat object
#' 
#' @param cmdata a data frame with the ClimMob data
#' @param rank_dat a list with parameters
#' @param reference_tech a integer or character indicating the reference technology
get_PlackettLuce_models = function(cmdata, rank_dat) {
  
  trait_list = rank_dat[["trait_list"]]
  nranker = rank_dat[["nranker"]]
  option = rank_dat[["option"]]
  group = rank_dat[["group"]]
  technologies_index = rank_dat[["technologies_index"]]
  technologies = rank_dat[["technologies"]]
  reference_trait_index = rank_dat[["reference_trait_index"]]
  comparison_with_local = rank_dat[["comparison_with_local"]]
  trait_names = rank_dat[["trait_names"]]
  reference_tech = rank_dat[["reference_tech"]]
  isKendall = FALSE
  strongest_link = ""
  weakest_link = ""
  kendall_plot = NULL
  logworth_group_plot = list()
  
  #...........................................................
  # Get the Kendall tau from individual trait rankings
  isKendall = FALSE
  strongest_link = c("", "")
  weakest_link = c("", "")
  kendall_plot = 0L
  
  if (isTRUE(length(trait_list) > 1)) {
    
    kendall = lapply(trait_list[-reference_trait_index], function(x){
      
      r1 = rank_tricot(cmdata[, c(technologies_index, trait_list[[reference_trait_index]]$strings)],
                       items = technologies_index,
                       input = trait_list[[reference_trait_index]]$strings,
                       validate.rankings = TRUE)
      
      r2 = rank_tricot(cmdata[, c(technologies_index, x$strings)],
                       items = technologies_index,
                       input = x$strings,
                       validate.rankings = TRUE)
      
      kendall = kendallTau(r1, r2, na.omit = FALSE)
      
      kendall$n = nrow(cmdata)
      
      kendall
      
    })
    
    kendall = do.call("rbind", kendall)
    
    kendall$trait = unlist(lapply(trait_list[-reference_trait_index], function(x){
      paste0(x$name, " [" , x$assessment, "]")
    }))
    
    kendall = kendall[rev(order(kendall$kendallTau)), ]
    
    kendall = kendall[,-2]
  
    isKendall = nrow(kendall) > 1
    
    if (isKendall) {
      
      strongest_link = c(kendall$trait[1],
                          round(kendall$kendallTau[1], 2))
      
      weakest_link = c(kendall$trait[nrow(kendall)],
                        round(kendall$kendallTau[nrow(kendall)], 2))
      
      kendall = kendall[,c("trait", "kendallTau", "Zvalue", "Pr(>|z|)")]
      
      kendall[,"kendallTau"] = round(kendall[,"kendallTau"], 3)
      
      kendall[,"Zvalue"] = round(kendall[,"Zvalue"], 3)
      
      stars = stars.pval(kendall[,"Pr(>|z|)"])
      
      kendall[, "Pr(>|z|)"] = format(kendall[, "Pr(>|z|)"], 
                                     scientific = TRUE, digits = 3)
      
      kendall[, "Pr(>|z|)"] = paste(kendall[, "Pr(>|z|)"], stars)
      
      kendall = kendall[rev(order(kendall$kendallTau)), ]
      
      kendall$trait = factor(kendall$trait, levels = rev(kendall$trait))
      
      # make a bar plot plot 
      kendall_plot = 
        ggplot2::ggplot(data = kendall, 
                        ggplot2::aes(x = kendallTau,
                                     y = trait, 
                                     fill = trait)) +
        ggplot2::geom_bar(stat = "identity", 
                          position = "dodge",
                          show.legend = FALSE,
                          width = 1, 
                          color = "#ffffff") + 
        ggplot2::scale_fill_manual(values = rev(col_pallet(nrow(kendall)))) +
        ggplot2::theme_classic() +
        ggplot2::theme(legend.position="bottom",
                       legend.text = ggplot2::element_text(size = 9),
                       axis.text.y = ggplot2::element_text(color = "grey20"),
                       axis.text.x = ggplot2::element_text(vjust = 1,
                                                           hjust=1, 
                                                           color = "grey20")) +
        ggplot2::labs(y = "Trait",
                      x = "Kendall tau") 
      
      
      names(kendall) = c("Trait", "Kendall tau", "Z value", "Pr(>|z|)")
      
      row.names(kendall) = 1:nrow(kendall)
      
    }
    
  }
  
  #...........................................................
  # Fit PlackettLuce Model
  # first a list with rankings
  R = list()
  
  for (i in seq_along(trait_list)) {
    
    keep = trait_list[[i]]$keep
    
    # list of arguments for the function that will be used to 
    # create the rankings
    a = list(cmdata[, c(technologies_index, trait_list[[i]]$strings)],
             items = technologies_index,
             input = trait_list[[i]]$strings,
             validate.rankings = TRUE)
    
    R[[i]] = do.call("rank_tricot", args = a)
    
    if (isTRUE(comparison_with_local)) {
      
      keep = trait_list[[reference_trait_index]]$keep2 & trait_list[[i]]$keep
      
      a = list(cmdata[keep, c(technologies_index, 
                               trait_list[[i]]$strings, 
                               trait_list[[reference_trait_index]]$tricotVSlocal)],
                items = technologies_index,
                input = trait_list[[i]]$strings,
                additional.rank = cmdata[keep, trait_list[[reference_trait_index]]$tricotVSlocal],
               validate.rankings = TRUE)
      
      R[[i]] = do.call("rank_tricot", args = a)
      
    }
  }
  
  # fit the model 
  mod = lapply(R, function(x){
    PlackettLuce(x)
  })
  
  #...........................................................
  # Analysis of variance
  anovas = lapply(mod, function(x){
    a = anova.PL(x)
    a
  })
  
  # and the tables with rounded p-values and sig stars
  aov_tbl = list()
  for (i in seq_along(trait_list)) {
    a = anovas[[i]]
    a[2, "Model"] = trait_list[[i]]$name
    a[,5] = paste(formatC(a[,5], format = "e", digits = 2),
                   stars.pval(a[,5]))
    aov_tbl[[i]] = a
  }
  
  #...........................................................
  # Bar plot with worth parameters for each trait
  worth_plot = lapply(mod, function(x){
    worth_bar(x, ref = reference_tech[1]) +
      labs(y = title_case(option), x = "Worth")
  })
  
  # Plot log worth
  logworth_plot = list()
  for(m in seq_along(mod)) {
    logworth_plot[[m]] = 
      plot_logworth(mod[[m]], ref = reference_tech, ci.level = 0.5) + 
      labs(title = paste0(rank_dat$trait_names[m],
                          " (n = ", length(mod[[m]]$rankings),")"))
  }
  
  #...........................................................
  # Head to head visualization of each technology performance by trait
  worth_map_data = data.frame()
  
  for(i in seq_along(reference_tech)) {
    wmd_i = gosset:::.combine_coeffs(mod,
                                     ref = reference_tech[i], 
                                     log = TRUE,
                                     vcov = FALSE)
    
    names(wmd_i) = trait_names
    
    # reshape to long format
    wmd_i = data.frame(items = rep(dimnames(wmd_i)[[1]], 
                                   times = ncol(wmd_i)),
                       labels = rep(dimnames(wmd_i)[[2]], 
                                    each = nrow(wmd_i)), 
                       winprob = as.numeric(unlist(wmd_i)),
                       check = reference_tech[i])
    
    worth_map_data = rbind(worth_map_data, wmd_i)
    
  }
  
  worth_map_data$labels = factor(worth_map_data$labels, 
                                  levels = rev(trait_names))
  
  lims = max(abs(worth_map_data$winprob)) * c(-1, 1)
  
  worthmap = ggplot(worth_map_data, 
                    aes(x = items, 
                        y = labels, 
                        fill = winprob,
                        label = as.character(round(winprob, 2)))) + 
    geom_tile() + 
    scale_fill_distiller(palette = "RdBu", 
                         limit = lims, 
                         direction = 1, 
                         na.value = "white", 
                         name = "") + 
    facet_wrap(~ check, strip.position = "bottom") +
    theme_bw() +  
    theme(axis.text = element_text(color = "grey20"), 
          strip.text.x = element_text(color = "grey20"), 
          axis.text.x = element_text(angle = 40, 
                                     vjust = 1, 
                                     hjust = 1), 
          axis.text.y = element_text(angle = 0, 
                                     vjust = 1, hjust = 1), 
          panel.grid = element_blank(),
          strip.background.x = element_blank(),
          strip.placement = "outside") +
    labs(x = "", y = "", fill = "")
    
  #.......................
  # Log worth plot
  logworth_grouped_rank = 
    plot_logworth(mod[[reference_trait_index]],
                  ref = reference_tech, ci.level = 0.5) 
  
  
  #...........................................................
  # Reliability 
  # run reliability over the different check varieties
  items = sort(unique(unlist(cmdata[technologies_index])))
  
  checks = reference_tech
  
  reliability_data = data.frame()
  
  for (i in seq_along(checks)) {
    
    rel_i = lapply(mod, function(x){
      
      r = try(reliability(x, ref = checks[i]), silent = TRUE)
      
      if ("try-error" %in% class(r)) {
        return()
      }
      
      r$Check = checks[i]
      
      r
      
    })
    
    for(j in seq_along(trait_names)) {
      
      if (is.null(rel_i[[j]])) next
      
      rel_i[[j]]$Trait = trait_names[j]
      
    }
    
    rel_i = do.call("rbind", rel_i)
    
    reliability_data = rbind(reliability_data, rel_i)
    
  }
  
  # remove the checks
  reliability_data = reliability_data[!reliability_data$item %in% checks, ]
  
  # put traits in the right order
  reliability_data$Trait = factor(reliability_data$Trait,
                                  levels = rev(trait_names))
  
  #set.seed(113)
  shapes = c(15:19, 21:24, 0:7, 9:10, 12)
  shapes = suppressWarnings(as.vector(matrix(shapes, 
                                             nrow = length(unique(reliability_data$item)))))
  
  # plot the reliability
  reliability_plot = ggplot(data = reliability_data,
                   aes(x = reliability, 
                       y = Trait,
                       shape = item,
                       color = item)) +
    geom_vline(xintercept = 0.5, 
               colour = "#7f2704",
               linewidth = 0.3) +
    scale_x_continuous(limits=c(0, 1)) +
    geom_point() +
    scale_shape_manual(values = shapes) +
    #scale_color_brewer(palette = "Dark2") +
    facet_wrap(~ Check, strip.position = "bottom") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          strip.background =element_rect(fill="white"),
          text = element_text(color = "grey20"),
          #legend.position = "bottom",
          legend.title = element_blank(),
          strip.background.x = element_blank(),
          strip.placement = "outside") +
    labs(x = "Prob. in outperforming",
         y = "")
  

  # split by groups, if any 
  if (length(unique(rank_dat$group) > 1)) {
    
    group = group[rank_dat$trait_list[[reference_trait_index]]$keep]
    
    unique_groups = unique(rank_dat$group)
    
    mod_group = list()
    
    for (i in seq_along(unique_groups)) {
      
      mod_group[[i]] = try(PlackettLuce(RG[group == unique_groups[i], ]),
                            silent = TRUE)
      
      logworth_group_plot[[i]] = 
        try(plot_logworth(mod_group[[i]], ref = reference_tech, ci.level = 0.5) +
        labs(title = unique_groups[i]),
        silent = TRUE)
      
      if("try-error" %in% class(logworth_group_plot[[i]])) {
        logworth_group_plot[[i]] = 0L
      }
      
    }
    
  }
  
  result = list(PL_models = mod,
                 logworth_grouped_rank = logworth_grouped_rank,
                 worthmap = worthmap,
                 logworth_plot = logworth_plot,
                 logworth_plot_groups = logworth_group_plot,
                 kendall = list(isKendall = isKendall,
                                kendall = kendall,
                                strongest_link = strongest_link, 
                                weakest_link = weakest_link,
                                kendall_plot = kendall_plot),
                reliability_plot = reliability_plot,
                reliability_data = reliability_data)
  
}


# .......................................
# Error in data 
# this is a file that is generated to be used in case of errors
error_data_PL_model = list(PL_models = list(),
                            logworth_grouped_rank = 0L,
                            worthmap = 0L,
                            logworth_plot = 0L,
                            logworth_plot_groups = 0L,
                            kendall = list(isKendall = FALSE,
                                           kendall = data.frame(),
                                           strongest_link = c("", ""), 
                                           weakest_link = c("", ""),
                                           kendall_plot = 0L),
                           reliability_plot = 0L,
                           reliability_data = data.frame())



# Disable this as the implementation needs to be tested first! 
# #..........................................................
# # PlackettLuce with aggregated rankings
# # this put the rankings from all traits into a single 
# # grouped rankings to assess "overall technology performance"
# # reference trait must be the first in this vector
# othertraits = union(names(trait_list[reference_trait_index]),
#                      names(trait_list[-reference_trait_index]))
# indicesbase = as.vector(which(trait_list[[reference_trait_index]]$keep))
# resetindices = 1:length(indicesbase)
# 
# RG = list()
# 
# index = c()
# 
# for(i in seq_along(othertraits)) {
#   
#   trait_i = which(names(trait_list) %in% othertraits[i])
#   
#   # this should be combined with the baseline trait
#   index_i = as.vector(which(trait_list[[trait_i]]$keep))
#   
#   keep_i = index_i %in% indicesbase
#   
#   index_i = index_i[keep_i]
#   
#   r_i = rankTricot(cmdata[index_i, ],
#                     technologies_index,
#                     c(trait_list[[trait_i]]$strings),
#                     group = FALSE)
#   
#   # reset indices to match with grouped_rankings later
#   index_i = resetindices[indicesbase %in% index_i]
#   
#   index = c(index, index_i)
#   
#   RG[[i]] = r_i
#   
# }
# 
# # make weights based on response
# weight = as.vector(table(index))
# weight = weight / max(weight)
# 
# RG = do.call("rbind", RG)
# 
# RG = group(RG, index = index)

# # PlackettLuce of aggregated rankings
# modRG = PlackettLuce(RG, weights = weight)

