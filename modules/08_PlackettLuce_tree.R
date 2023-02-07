#' This module analyses the ranking data
#' 
#' The analysis is performed iteratively for each trait 
#' retained in the rank_dat object
#' 
#' @param cmdata a data frame with the ClimMob data
#' @param rank_dat a list with parameters
#' @param agroclimate a list with agroclimatic parameters 
get_PlackettLuce_tree = function(cmdata, rank_dat, agroclimate) {
  
  trait_list = rank_dat[["trait_list"]]
  option = rank_dat[["option"]]
  group = rank_dat[["group"]]
  technologies_index = rank_dat[["technologies_index"]]
  technologies = rank_dat[["technologies"]]
  reference_trait_index = rank_dat[["reference_trait_index"]]
  comparison_with_local = rank_dat[["comparison_with_local"]]
  trait_names = rank_dat[["trait_names"]]
  covarTRUE = rank_dat[["covarTRUE"]]
  covar = rank_dat[["covar"]]
  nranker = sum(trait_list[[reference_trait_index]]$keep)
  # use <<- to assign these two variables to the .GlobalEnv
  use_agroclimate = isTRUE(agroclimate$agroclimate)
  node_size <<- ceiling(nranker * 0.25)
  tree_alpha <<- 0.25
  reference_tech = rank_dat[["reference_tech"]]
  
  if (isTRUE(covarTRUE)) {
    
    # rename covariates to avoid duplicated strings, in case the same 
    # question was made in different data collection moments 
    covar$codeQst = rename_duplicates(covar$codeQst)
    covar$name    = rename_duplicates(covar$name, sep = " ")
    
    # check if longlat is required
    if (any(grepl("geotrial", covar$codeQst))) {
      
      index = which(grepl("geotrial", covar$codeQst))[1]
      
      if (is.na(index)) next
      
      covar[index, "nameString"] = paste0("ASS",covar$assessmentId[index],"__longitude")
      covar[index, "name"] = paste(covar[index, "name"], "Longitude")
      
      covar[nrow(covar) + 1, ] = covar[index, ]
      covar[nrow(covar), "nameString"] = paste0("ASS",covar$assessmentId[index],"__latitude")
      covar[nrow(covar), "name"] = paste(covar[nrow(covar), "name"], "Latitude")
      
    }
    
    # add the string $ to indicate the end of pattern
    strings = paste0(covar$nameString, "$")
    
    # check for the full names
    for(i in seq_along(covar$nameString)){
      
      index = which(grepl(strings[i], names(cmdata)))[1]
      
      if (is.na(index)){ 
        covar[i, ] = NA
        next
      }
      
      covar$nameString[i] = names(cmdata[index])
      
    }
    
    covar = na.omit(covar)
    
    strings = unique(covar$nameString)
    
    # check for missing data
    # if values are character or factor, add "Unknown" for any missing
    # if numeric or integer, add the median
    cmdata[, strings] = lapply(cmdata[, strings], function(x){
      
      if (class(x) == "integer" | class(x) == "numeric") {
        x[is.na(x)] = median(x, na.rm = TRUE)
      }
      
      if (class(x) == "numeric") {
        x = round(x, 2)
      }
      
      if (class(x) == "character") {
        x = ifelse(is.na(x), "Unknown", x)
        x = as.factor(x)
      }
      
      # if a factor with more than 10 classes then omit it
      if (class(x) == "factor") {
        lv = levels(x)
        if (length(lv) > 10) {
          x = NA
        }
      }
      
      x
      
    })
    
    # Do another check for NAs, but this time covariates with NAs 
    # will be removed
    keep = NULL
    for(i in seq_along(strings)){
      k = !is.na(cmdata[, strings[i]])
      keep = cbind(keep, k)
    }
    
    # find those that are above the threshold of missexp
    dropit = mincovar > (colSums(keep) / nranker)
    
    # drop those bellow threshold
    keep = as.data.frame(keep[, !dropit])
    
    # create a single vector that will be used to filter cmdata
    keep = rowSums(keep)
    keep = keep == max(keep)
    
    covar_dropped = covar$name[dropit]
    covar = covar[!dropit, ]
    
    trait_list[[reference_trait_index]]$keep_covariate = keep
    
    # if no covariate left out put a pseudo variable
    if (isTRUE(dim(covar)[[1]] == 0)) {
      
      covarTRUE = FALSE
      
    }
    
  }
  
  if (isFALSE(covarTRUE)) {
    
    cmdata$Intercept = rep(0, nranker)
    
    trait_list[[reference_trait]]$keep_covariate = rep(TRUE, nranker)
    
    covar = data.frame(codeQst = "xinterceptx", 
                        nameString = "Intercept",
                        name = "Intercept-only model",
                        questionAsked = "",
                        assessmentName = "",
                        assessmentId = "")
  }
  
  if (isTRUE(comparison_with_local)) {
    
    keep = trait_list[[reference_trait_index]]$keep2 & 
      trait_list[[reference_trait_index]]$keep_covariate
    
    a = list(cmdata[keep, c(technologies_index, 
                             trait_list[[reference_trait_index]]$strings, 
                             trait_list[[reference_trait_index]]$tricotVSlocal)],
              items = technologies_index,
              input = trait_list[[reference_trait_index]]$strings,
              additional.rank = cmdata[keep, trait_list[[reference_trait_index]]$tricotVSlocal], 
              group = TRUE)
    
    G = do.call("rankTricot", args = a)
    
    
  } 
  
  if (isFALSE(comparison_with_local)) {
    
    keep = trait_list[[reference_trait_index]]$keep & 
      trait_list[[reference_trait_index]]$keep_covariate
    
    a = list(cmdata[keep, c(technologies_index, 
                             trait_list[[reference_trait_index]]$strings)],
              items = technologies_index,
              input = trait_list[[reference_trait_index]]$strings, 
              group = TRUE)
    
    G = do.call("rankTricot", args = a)
    
  }
  
  #..........................................................
  # PlackettLuce tree
  # Prepare for PL tree
  # data frame of explanatory variables
  Gdata = as.data.frame(cmdata[keep, c(covar$nameString)], stringsAsFactors = TRUE)
  nvar = length(covar$nameString)
  
  # rename covariates with the name taken from ClimMob
  names(Gdata) = gsub(" ", "", title_case(covar$name))
  
  nG = nrow(Gdata)
  
  rownames(Gdata) = 1:nG
  
  # add agroclimate data if any
  if(use_agroclimate > 1) {
    
    if (isTRUE(nrow(agroclimate$rainfall[keep, ]) >= ceiling(nG * 0.95))) {
      
      Gdata = cbind(Gdata, agroclimate$rainfall[keep, ], agroclimate$temperature[keep, ])
      
    }
    
  }
  
  # remove variables with near zero variance
  out = nearZeroVar(Gdata, freqCut = 85/15)
  
  Gdata = Gdata[, -out]
  
  # check correlation in continuous values
  numbers = Gdata[, unlist(lapply(Gdata[1:ncol(Gdata)], is.numeric))]
  
  if (ncol(numbers) > 0) {
    corr = cor(numbers)
    corr[corr < 0.95 & corr > -0.95 | corr == 1] <- NA
    rmv = names(which(rowSums(corr, na.rm = TRUE) != 0))
    rmv = !names(Gdata) %in% rmv
    Gdata = Gdata[rmv]
  }
  
  # check correlation in continuous values
  categories = Gdata[, unlist(lapply(Gdata[1:ncol(Gdata)], is.factor))]
  
  if (ncol(categories) > 0) {
    categories[1:ncol(categories)] = lapply(categories[1:ncol(categories)], as.integer)
    corr = cor(categories)
    corr[corr < 0.7 & corr > -0.7 | corr == 1] <- NA
    rmv = names(which(rowSums(corr, na.rm = TRUE) != 0))
    rmv = !names(Gdata) %in% rmv
    Gdata = Gdata[rmv]
  }
  
  if (ncol(Gdata) < 2) {
    Gdata$Intercept1 = 1
    Gdata$Intercept2 = 2
  }
  
  # Combine with grouped rankings
  Gdata = cbind(G, Gdata)
  
  # perform a forward selection as pltree() 
  # sometimes don't split when G ~ . is used
  var_keep = character(0L)
  best = TRUE
  counter = 1
  exp_var = names(Gdata)[-1]
  
  cat("Selecting the best covariate for Plackett-Luce trees \n")
  
  while (best) {
    
    fs = length(exp_var)
    models = data.frame()
    
    for(i in seq_len(fs)){
      
      t_i = try(pltree(as.formula(paste0("G ~ ", paste(c(var_keep, exp_var[i]), collapse = " + "))),
                        data = Gdata,
                        minsize = node_size,
                        alpha = tree_alpha,
                        gamma = TRUE), 
                 silent = TRUE)
      
      if (isFALSE("try-error" %in% class(t_i))) {
        validations = data.frame(nnodes = length(nodeids(t_i, terminal = TRUE)),
                                  AIC = AIC(t_i),
                                  noerror = TRUE)
      }else{
        validations = data.frame(nnodes = NA,
                                  AIC = NA,
                                  noerror = FALSE)
      }
      
      models = rbind(models, validations)
      
    }
    
    counter = counter + 1
    
    if (length(exp_var) == 0) {
      best = FALSE
    }
    
    if (best) {
      # update vector with covariates to keep only those with no error
      # and those with no split
      exp_var = exp_var[models$noerror & models$nnodes > 1]
      # also take out from the models data frame
      models = models[models$noerror == TRUE & models$nnodes > 1, ]
      # find the index for the best model, the one with lowest AIC
      index_bext = which.min(models$AIC)
      # and the best model
      best_model = exp_var[index_bext]
      exp_var = exp_var[-index_bext]
      var_keep = c(var_keep, best_model)
    }
    
    if (length(exp_var) == 0) {
      best = FALSE
    }
    
  }
  
  if (length(var_keep) > 0) {
    treeformula = paste0("G ~ ", paste(c(var_keep), collapse = " + "))
  }
  
  if (length(var_keep) == 0) {
    treeformula = "G ~ 1"
  }
  
  message(treeformula)
  
  # now fit the tree with the selected covariates
  tree_f = pltree(as.formula(treeformula),
                  data = Gdata,
                  minsize = as.integer(node_size),
                  alpha = tree_alpha,
                  gamma = TRUE)
  
  isTREE = isTRUE(length(tree_f) > 1)
  
  # if the tree has splits, extract coeffs from nodes
  if (isTREE) { 
    
    # ......................................
    # ......................................
    # Reconstruct the tree using individual nodes
    # run this for the condensended rankings using predicted nodes
    nodes_tree = predict(tree_f, type = "node")
    node_id_tree = sort(unique(nodes_tree))
    
    if (isTRUE(length(nodes_tree) == length(Gdata$G))) {
      
      tree_mod = list()
      nobs_tree = integer()

      for (i in seq_along(node_id_tree)) {
        
        Gi = Gdata$G[nodes_tree == node_id_tree[i]]
        
        tree_mod[[i]] = PlackettLuce(Gi)
        
        nobs_tree = c(nobs_tree, length(Gi))
        
        rm(Gi)
      }
      
      tree_branch = gosset:::build_tree_branches(tree_f)
      tree_nodes = gosset:::build_tree_nodes(tree_mod, 
                                             log = TRUE,
                                             ref = reference_tech[1],
                                             ci.level = 0.5,
                                             node.ids = node_id_tree,
                                             n.obs = nobs_tree) +
        theme(axis.text.x = element_text(angle = 0))
      
      plottree = tree_branch / tree_nodes + plot_layout(heights =  c(1, 1))
      
      # get regret tables
      regret_tbl = regret(tree_mod, n1 = 1000)
      
      names(regret_tbl) = c(title_case(option), "Worth", "Worst Regret", "Regret")
      
      regret_tbl[2:4] = lapply(regret_tbl[2:4], function(x) round(x, 3))
      
      # .................................
      # get node summaries
      node_ids = nodeids(tree_f, terminal = TRUE)
      
      rules = node_rules(tree_f)
      
      for(i in seq_along(node_ids)){
        
        summ = coef(tree_mod[[i]], log = TRUE, ref = reference_tech)
        
        summ = summ[!grepl("tie", names(summ))]
        
        bests = names(rev(sort(summ[summ >= 0])))
        # if more than three, subset to get only three
        if (isTRUE(length(bests) > 3)) {
          bests = bests[1:3]
        }
        
        worsts = names(sort(summ[summ < 0]))
        # if more than three, subset to get only three
        if (isTRUE(length(worsts) > 3)) {
          worsts = worsts[1:3]
        }
        
        # avoid bests and worst in the same column, this when very few items are tested
        b = bests[!bests %in% worsts]
        w = worsts[!worsts %in% bests]
        
        bests = paste(b, collapse =", ")
        worsts = paste(w, collapse = ", ")
        
        rules[i, 3] = bests
        rules[i, 4] = worsts
        rules[i, 5] = nobs_tree[i]
        
      }
      
      node_summary  = rules[c(1,5,3,4,2)]
      
      names(node_summary) = c("Node", "N", "Superior Performance", 
                              "Underperformance", "Variable and Threshold")
      
    }
    
    
  } else {
    node_summary = data.frame()
    regret_tbl = data.frame()
    plottree = 0L
  }
  
  result = list(isTREE = isTREE,
                 tree_formula = treeformula,
                 nobservations_used_tree = dim(Gdata)[1],
                 PLtree = tree_f,
                 PLtree_plot = plottree,
                 node_summary = node_summary,
                 regret_table = regret_tbl)
  
}


# .......................................
# Error in data 
# this is a file that is generated to be used in case of errors
error_data_PL_tree = list(isTREE = FALSE,
                           tree_formula = "",
                           nobservations_used_tree = 0,
                           PLtree = list(),
                           PLtree_plot = 0L,
                           node_summary = data.frame(),
                           regret_table = data.frame())


