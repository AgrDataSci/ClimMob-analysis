#' This module analyses the ranking data
#' 
#' The analysis is performed iteratively for each trait 
#' retained in the rank_dat object
#' 
#' @param cmdata a data frame with the ClimMob data
#' @param rank_dat a list with parameters
#' @param reference_tech a integer or character indicating the reference technology
get_pltree <- function(cmdata, rank_dat, reference_tech) {
  
  trait_list <- rank_dat[["trait_list"]]
  option <- rank_dat[["option"]]
  group <- rank_dat[["group"]]
  technologies_index <- rank_dat[["technologies_index"]]
  technologies <- rank_dat[["technologies"]]
  reference_trait_index <- rank_dat[["reference_trait_index"]]
  comparison_with_local <- rank_dat[["comparison_with_local"]]
  trait_names <- rank_dat[["trait_names"]]
  covarTRUE <- rank_dat[["covarTRUE"]]
  covar <- rank_dat[["covar"]]
  nranker <- sum(trait_list[[reference_trait_index]]$keep)
  node_size <- nranker * 0.1
  tree_alpha <- 0.5
  
  if (isTRUE(covarTRUE)) {
    
    # rename covariates to avoid duplicated strings, in case the same 
    # question was made in different data collection moments 
    covar$codeQst <- rename_duplicates(covar$codeQst)
    covar$name    <- rename_duplicates(covar$name, sep = " ")
    
    # check if longlat is required
    if (any(grepl("geotrial", covar$codeQst))) {
      
      index <- which(grepl("geotrial", covar$codeQst))[1]
      
      if (is.na(index)) next
      
      covar[index, "nameString"] <- paste0("ASS",covar$assessmentId[index],"__longitude")
      covar[index, "name"] <- paste(covar[index, "name"], "Longitude")
      
      covar[nrow(covar) + 1, ] <- covar[index, ]
      covar[nrow(covar), "nameString"] <- paste0("ASS",covar$assessmentId[index],"__latitude")
      covar[nrow(covar), "name"] <- paste(covar[nrow(covar), "name"], "Latitude")
      
    }
    
    # add the string $ to indicate the end of pattern
    strings <- paste0(covar$nameString, "$")
    
    # check for the full names
    for(i in seq_along(covar$nameString)){
      
      index <- which(grepl(strings[i], names(cmdata)))[1]
      
      if (is.na(index)){ 
        covar[i, ] <- NA
        next
      }
      
      covar$nameString[i] <- names(cmdata[index])
      
    }
    
    covar <- na.omit(covar)
    
    strings <- covar$nameString
    
    # check for missing data
    # if values are character or factor, add "Unknown" for any missing
    # if numeric or integer, add the median
    cmdata[, strings] <- lapply(cmdata[, strings], function(x){
      
      if (class(x) == "integer" | class(x) == "numeric") {
        x[is.na(x)] <- median(x, na.rm = TRUE)
      }
      
      if (class(x) == "numeric") {
        x <- round(x, 2)
      }
      
      if (class(x) == "character") {
        x <- ifelse(is.na(x), "Unknown", x)
        x <- as.factor(x)
      }
      
      # if a factor with more than 10 classes then omit it
      if (class(x) == "factor") {
        lv <- levels(x)
        if (length(lv) > 10) {
          x <- NA
        }
      }
      
      x
      
    })
    
    # Do another check for NAs, but this time covariates with NAs 
    # will be removed
    keep <- NULL
    for(i in seq_along(strings)){
      k <- !is.na(cmdata[, strings[i]])
      keep <- cbind(keep, k)
    }
    
    # find those that are above the threshold of missexp
    dropit <- mincovar > (colSums(keep) / nranker)
    
    # drop those bellow threshold
    keep <- as.data.frame(keep[, !dropit])
    
    # create a single vector that will be used to filter cmdata
    keep <- rowSums(keep)
    keep <- keep == max(keep)
    
    covar_dropped <- covar$name[dropit]
    covar <- covar[!dropit, ]
    
    trait_list[[reference_trait_index]]$keep_covariate <- keep
    
    # if no covariate left out put a pseudo variable
    if (isTRUE(dim(covar)[[1]] == 0)) {
      
      covarTRUE <- FALSE
      
    }
    
  }
  
  if (isFALSE(covarTRUE)) {
    
    cmdata$Intercept <- rep(0, nranker)
    
    trait_list[[reference_trait]]$keep_covariate <- rep(TRUE, nranker)
    
    covar <- data.frame(codeQst = "xinterceptx", 
                        nameString = "Intercept",
                        name = "Intercept-only model",
                        questionAsked = "",
                        assessmentName = "",
                        assessmentId = "")
  }
  
  if (isTRUE(comparison_with_local)) {
    
    keep <- trait_list[[reference_trait_index]]$keep2 & 
      trait_list[[reference_trait_index]]$keep_covariate
    
    a <- list(cmdata[keep, c(technologies_index, 
                             trait_list[[reference_trait_index]]$strings, 
                             trait_list[[reference_trait_index]]$tricotVSlocal)],
              items = technologies_index,
              input = trait_list[[reference_trait_index]]$strings,
              additional.rank = cmdata[keep, trait_list[[reference_trait_index]]$tricotVSlocal], 
              group = TRUE)
    
    G <- do.call("rankTricot", args = a)
    
    
  } 
  
  if (isFALSE(comparison_with_local)) {
    
    keep <- trait_list[[reference_trait_index]]$keep & 
      trait_list[[reference_trait_index]]$keep_covariate
    
    a <- list(cmdata[keep, c(technologies_index, 
                             trait_list[[reference_trait_index]]$strings)],
              items = technologies_index,
              input = trait_list[[reference_trait_index]]$strings, 
              group = TRUE)
    
    G <- do.call("rankTricot", args = a)
    
  }
  
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
  
  # .............................................
  # Prepare for PL tree
  # data frame of explanatory variables
  Gdata <- as.data.frame(cmdata[keep, c(covar$nameString)], stringsAsFactors = TRUE)
  nvar <- length(covar$nameString)
  
  # rename covariates with the name taken from ClimMob
  names(Gdata) <- gsub(" ", "", title_case(covar$name))
  Gdata <- cbind(G, Gdata)
  nG <- nrow(Gdata)
  rownames(Gdata) <- 1:nG
  
  # perform a forward selection as pltree() 
  # sometimes don't split when G ~ . is used
  var_keep <- character(0L)
  best <- TRUE
  counter <- 1
  exp_var <- names(Gdata)[-1]
  
  cat("Selecting the best covariate for Plackett-Luce trees \n")
  
  while (best) {
    
    fs <- length(exp_var)
    models <- data.frame()
    
    for(i in seq_len(fs)){
      
      t_i <- try(pltree(as.formula(paste0("G ~ ", paste(c(var_keep, exp_var[i]), collapse = " + "))),
                        data = Gdata,
                        minsize = node_size,
                        alpha = tree_alpha,
                        gamma = TRUE), 
                 silent = TRUE)
      
      if (isFALSE("try-error" %in% class(t_i))) {
        validations <- data.frame(nnodes = length(nodeids(t_i, terminal = TRUE)),
                                  AIC = AIC(t_i),
                                  noerror = TRUE)
      }else{
        validations <- data.frame(nnodes = NA,
                                  AIC = NA,
                                  noerror = FALSE)
      }
      
      models <- rbind(models, validations)
      
    }
    
    counter <- counter + 1
    
    if (length(exp_var) == 0) {
      best <- FALSE
    }
    
    if (best) {
      # update vector with covariates to keep only those with no error
      # and those with no split
      exp_var <- exp_var[models$noerror & models$nnodes > 1]
      # also take out from the models data frame
      models <- models[models$noerror == TRUE & models$nnodes > 1, ]
      # find the index for the best model, the one with lowest AIC
      index_bext <- which.min(models$AIC)
      # and the best model
      best_model <- exp_var[index_bext]
      exp_var <- exp_var[-index_bext]
      var_keep <- c(var_keep, best_model)
    }
    
    if (length(exp_var) == 0) {
      best <- FALSE
    }
    
  }
  
  if (length(var_keep) > 0) {
    treeformula <- paste0("G ~ ", paste(c(var_keep), collapse = " + "))
  }
  
  if (length(var_keep) == 0) {
    treeformula <- "G ~ 1"
  }
  
  message(treeformula)
  
  # now fit the tree with the selected covariates
  tree_f <- pltree(as.formula(treeformula),
                   data = Gdata,
                   minsize = node_size,
                   alpha = tree_alpha,
                   gamma = TRUE)
  
  isTREE <- isTRUE(length(tree_f) > 1)
  
  # if the tree has splits, extract coeffs from nodes
  if (isTREE) { 
    
    # plot the tree (if any)
    plottree <- plot(tree_f, ci.level = 0.5)
    
    # ......................................
    # ......................................
    # Reconstruct the tree using individual nodes
    # run this for the condensended rankings using predicted nodes
    nodes_tree <- predict(tree_f, type = "node")
    node_id_tree <- sort(unique(nodes_tree))
    
    if (isTRUE(length(nodes_tree) == length(RG))) {
      
      tree_mod <- list()
      nobs_tree <- integer()
      
      for (i in seq_along(node_id_tree)) {
        
        Gi <- RG[nodes_tree == node_id_tree[i]]
        
        tree_mod[[i]] <- PlackettLuce(Gi)
        
        nobs_tree <- c(nobs_tree, length(Gi))
        
        rm(Gi)
      }
      
      tree_branch <- gosset:::build_tree_branches(tree_f)
      tree_nodes <- gosset:::build_tree_nodes(tree_mod, 
                                              log = TRUE,
                                              ci.level = 0.95,
                                              node.ids = node_id_tree,
                                              n.obs = nobs_tree) +
        theme(axis.text.x = element_text(angle = 45))
      
      plottree <- tree_branch / tree_nodes + plot_layout(heights =  c(1, 1))
      
      # get regret tables
      regret_tbl <- regret(tree_mod, n1 = 1000)
      
      names(regret_tbl) <- c(title_case(option), "Worth", "Worst Regret", "Regret")
      
      
      # .................................
      # get node summaries
      node_ids <- nodeids(tree_f, terminal = TRUE)
      
      rules <- node_rules(tree_f)
      
      for(i in seq_along(node_ids)){
        
        summ <- coef(tree_mod[[i]], log = TRUE, ref = reference_tech)
        
        summ <- summ[!grepl("tie", names(summ))]
        
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
        
        rules[i, 3] <- bests
        rules[i, 4] <- worsts
        rules[i, 5] <- nobs_tree[i]
        
      }
      
      node_summary  <- rules[c(1,5,3,4,2)]
      
      names(node_summary) <- c("Node", "N", "Best performance","Worst performance", "Split rule")
      
    }
    
    
  } else {
    node_summary <- data.frame()
    regret_tbl <- data.frame()
    plottree <- 0L
  }
  
  result <- list(isTRUE = isTRUE,
                 tree_formula = treeformula,
                 PLtree = tree_f,
                 PLtree_plot = plottree,
                 node_summary = node_summary,
                 regret_table = regret_tbl)
  
}

