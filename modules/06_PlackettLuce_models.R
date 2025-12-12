#' This module analyses the ranking data
#' 
#' The analysis is performed iteratively for each trait 
#' retained in the rank_dat object
#' 
#' @param cmdata a data frame with the ClimMob data
#' @param rank_dat a list with parameters
#' @examples 
#' 
#' modules = list.files("modules",
#'                      full.names = TRUE,
#'                      pattern = ".R")
#' modules = modules[-which(grepl("check_packages.R", modules))]
#' for (i in seq_along(modules)) {
#'   source(modules[i])
#' }
#' 
#' load("modules/example-data-structure.rda")
#' 
#' models = get_PlackettLuce_models(cmdata, rank_dat)
#' @export
get_PlackettLuce_models = function(cmdata, rank_dat) {
  
  trait_list = rank_dat[["trait_list"]]
  nranker = rank_dat[["nranker"]]
  option = rank_dat[["option"]]
  technologies_index = rank_dat[["technologies_index"]]
  technologies = rank_dat[["technologies"]]
  reference_trait_index = rank_dat[["reference_trait_index"]]
  comparison_with_local = rank_dat[["comparison_with_local"]]
  trait_names = rank_dat[["trait_names"]]
  reference_tech = rank_dat[["reference_tech"]]

  #...........................................................
  # Fit PlackettLuce Model ####
  # first a list with rankings
  R = lapply(trait_list, function(x){
    rank_tricot(cmdata[, c(technologies_index, x$strings)],
                    items = technologies_index,
                    input = x$strings,
                    validate.rankings = TRUE)
  })
  
  # # Handle rankings with poor connectivity
  connection = lapply(R, function(x) connectivity(x, verbose = FALSE)$no)
  
  connection = as.vector(unlist(connection) > 1)
  
  # force a pseudo rank  
  R[connection] = lapply(R[connection], force_pseudo_rank)

  # fit the model 
  mod = lapply(R, function(x){
    PlackettLuce(x)
  })
  
  # Plot log worth
  logworth_plot = list()
  
  for(m in seq_along(mod)) {
    lwp = try(plot_logworth(mod[[m]], 
                        ref = reference_tech, 
                        ci.level = 0.5) + 
      labs(title = paste0(rank_dat$trait_names[m],
                          " (n = ", length(mod[[m]]$rankings),")")),
      silent = TRUE)
    
    if ("try-error" %in% class(lwp)) next
    
    logworth_plot[[m]] = lwp
    
  }
  
  logworth_overall = logworth_plot[[reference_trait_index]]
  
  if ("try-error" %in% class(logworth_overall)) {
    logworth_overall = ggplot()
  }
  
  rmv = unlist(lapply(logworth_plot, is.null))
  
  logworth_plot = logworth_plot[!rmv]
  
  #...........................................................
  # Head to head visualization of each technology performance by trait
  worthmap = 
    worth_map(mod, labels = trait_names, labels.order = rev(trait_names)) +
    labs(x = "", y = "") + 
    theme(axis.text = element_text(color = "grey20"),
          strip.text.x = element_text(color = "grey20"),
          axis.text.x = element_text(size = 10,
                                     angle = 45,
                                     vjust = 1,
                                     hjust = 1,
                                     color = "grey20"),
          axis.text.y = element_text(size = 10,
                                     angle = 0,
                                     vjust = 1,
                                     hjust = 1,
                                     color = "grey20"))
  
  #...........................................................
  # Analysis of variance ####
  anovas = lapply(mod, function(x){
    a = anova.PL(x)
    a
  })
  
  # and the tables with rounded p-values and sig stars
  aov_tbl = list()
  for (i in seq_along(trait_list)) {
    a = anovas[[i]]
    a = a[-1, ]
    a[1, "Model"] = paste0(trait_list[[i]]$name, " [" , trait_list[[i]]$assessment, "]")
    aov_tbl[[i]] = a
  }
  
  aov_tbl = do.call("rbind", aov_tbl)
  
  rownames(aov_tbl) = 1:nrow(aov_tbl)
  
  names(aov_tbl)[names(aov_tbl) == "Model"] = "Trait"
  
  #...........................................................
  # Net winning score
  netwinning = lapply(R, function(x){
    preference_index(x)[,1:4]
  })
  
  netwinning = do.call("rbind", netwinning)
  
  netwinning$trait = rep(trait_names, each = length(technologies))
  
  netwinning$trait[duplicated(netwinning$trait)] = ""
  
  names(netwinning)[1] = option
  
  netwinning = netwinning[c(5, 1, 2, 3, 4)]
  
  names(netwinning) = ClimMobTools:::.title_case(names(netwinning))
  
  #...........................................................
  # Reliability ####
  # run reliability over the different check varieties
  checks = reference_tech
  
  reliability_data = data.frame()
  
  reliability_plot = list()
  
  for(i in seq_along(checks)) {
    rel = lapply(mod, function(x){
      reliability(x, ref = checks[i])
    })
    
    rel = do.call("rbind", rel)
    
    rel$trait = rep(trait_names, each = length(technologies))
    
    rel$trait = factor(rel$trait, levels = trait_names)
    
    rel$improvement = round((rel$reliability / 0.5 - 1), 2)
    
    rel = rel[order(rel$improvement), ]
    
    rel$item = factor(rel$item, levels = unique(rel$item))
    
    rel = rel[!rel$item %in% checks[i], ]
    
    reliability_data = rel
    
    reliability_plot[[i]] =
      ggplot(data = rel,
             aes(x = improvement, 
                 y = trait,
                 fill = "white")) +
      geom_bar(stat = "identity",
               width = 0.7,
               position = "dodge", 
               show.legend = FALSE) +
      scale_fill_manual(values = "#5aae61") +
      facet_wrap(~ item, strip.position = "bottom") +
      geom_vline(xintercept = 0,
                 colour = "grey40",
                 linewidth = 1) +
      theme_classic() +
      labs(x = paste("Probability of outperforming", checks[i]),
           y = "")
  }
  

  #.....................................................
  #.....................................................
  # Get the Kendall tau ####
  if (length(trait_list) > 1){
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
             aes(y = kendallTau,
                 x = trait, 
                 fill = trait)) +
      geom_chicklet(show.legend = FALSE) +
      coord_flip() +
      scale_fill_manual(values = col_pallet(nrow(kendall))) +
      theme_classic() +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 12, color = "grey20"),
            axis.text.y = element_text(size = 12, color = "grey20"),
            axis.title = element_text(size = 12, color = "grey20"),
            axis.text.x = element_text(size = 12,
                                       vjust = 1,
                                       hjust=1, 
                                       color = "grey20")) +
      labs(x = "Trait",
           y = "Kendall tau") 
    
    names(kendall) = c("Trait", "Kendall tau", "Z value", "Pr(>|z|)")
    
    row.names(kendall) = 1:nrow(kendall)
    
  }else{
    
    kendall = data.frame()
    strongest_link = ""
    weakest_link = ""
    kendall_plot = 0L
    
  }
  
  # export results
  result = list(PL_models = mod,
                logworth_overall = logworth_overall,
                netwinning = netwinning,
                worthmap = worthmap,
                ANOVA = aov_tbl,
                logworth_plot = logworth_plot,
                kendall = list(kendall = kendall,
                               strongest_link = strongest_link, 
                               weakest_link = weakest_link,
                               kendall_plot = kendall_plot),
                reliability_plot = reliability_plot,
                reliability_data = reliability_data)
  
  
  return(result)
  
}

# .......................................
# Error in data 
# this is a file that is generated to be used in case of errors
error_data_PL_model = list(PL_models = list(),
                           logworth_overall = 0L,
                           netwinning = 0L,
                           worthmap = 0L,
                           ANOVA = data.frame(),
                           logworth_plot = list(),
                           kendall = list(kendall = data.frame(),
                                          strongest_link = "", 
                                          weakest_link = "",
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

