if (isTRUE(covarTRUE)) {
  
  # rename covariates to avoid duplicated strings, in case the same 
  # question was made in different data collection moments 
  covar <- pars$covariates
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
  # if values are character or factor, add "Unkown" for any missing
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
  
  trait_list[[reference_trait]]$keep_covariate <- keep
  
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

if (isTRUE(tricotVSlocal)) {
  
  keep <- trait_list[[reference_trait]]$keep2 & trait_list[[reference_trait]]$keep_covariate
  
  a <- list(cmdata[keep, c(itemnames, trait_list[[reference_trait]]$strings, 
                           trait_list[[reference_trait]]$tricotVSlocal)],
            items = itemnames,
            input = trait_list[[reference_trait]]$strings,
            additional.rank = cmdata[keep, trait_list[[reference_trait]]$tricotVSlocal], 
            group = TRUE)
  
  G <- do.call(rankwith, args = a)
  
  
} 

if (isFALSE(tricotVSlocal)) {
  
  keep <- trait_list[[reference_trait]]$keep & trait_list[[reference_trait]]$keep_covariate
  
  a <- list(cmdata[keep, c(itemnames, trait_list[[reference_trait]]$strings)],
            items = itemnames,
            input = trait_list[[reference_trait]]$strings, 
            group = TRUE)
  
  G <- do.call(rankwith, args = a)
  
}


# make the plot with the network
net <- network(G)

# data frame of explanatory variables
Gdata <- as.data.frame(cmdata[keep, c(covar$nameString)], stringsAsFactors = TRUE)
nvar <- length(covar$nameString)

# rename covariates with the name taken from ClimMob
names(Gdata) <- gsub(" ", "", ClimMobTools:::.title_case(covar$name))
Gdata <- cbind(G, Gdata)
nG <- nrow(Gdata)
rownames(Gdata) <- 1:nG

# perform a forward selection as pltree() sometimes don't split 
# when G ~ . is used
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
                      minsize = minsplit,
                      alpha = sig_level_tree,
                      ref = reference,
                      gamma = TRUE), silent = TRUE)
    
    if (isFALSE("try-error" %in% class(t_i))) {
      validations <- data.frame(nnodes = length(nodeids(t_i, terminal = TRUE)),
                                AIC = AIC(t_i),
                                noerror = !"try-error" %in% class(try(itempar(t_i, vcov = TRUE), silent = TRUE)))
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

if (length(var_keep) == 0) {
  var_keep <- names(Gdata)[-1]
}

treeformula <- as.formula(paste0("G ~ ", paste(c(var_keep), collapse = " + ")))

# now fit the tree with the selected covariates
tree_f <- pltree(treeformula,
                 data = Gdata,
                 minsize = minsplit,
                 gamma = TRUE,
                 alpha = sig_level_tree,
                 ref = reference)

isTREE <- isTRUE(length(tree_f) > 1)

# if the tree has splits, extract coeffs from nodes
if (isTREE) { 
  
  # plot the tree (if any)
  plottree <- plot(tree_f, ci.level = ci_level)
  
  node_ids <- nodeids(tree_f, terminal = TRUE)
  
  rules <- node_rules(tree_f)
  
  for(i in seq_along(node_ids)){
    
    summ <- coef(tree_f[[node_ids[i]]], log = TRUE, ref = reference)
    
    summ <- summ[!grepl("tie", names(summ))]
    
    bests <- names(rev(sort(summ[summ >= 0])))
    # if more than three, subset to get only three
    if (isTRUE(length(bests) > 3)) {
      bests <- bests[1:3]
    }
    
    # get the three worst items
    # worsts <- as.character(rev(summ[grepl(summ[nrow(summ),"group"], 
    #                                         summ[, "group"]), "term"]))
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
    rules[i, 5] <- tree_f[[ node_ids[i] ]]$node$info$nobs
    
  }
  
  node_summary  <- rules[c(1,5,3,4,2)]
  
  names(node_summary) <- c("Node", "N", "Best ranked","Worst ranked", "Split rule")
  
}