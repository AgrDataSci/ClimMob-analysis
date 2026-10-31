# ................................................................
# Plackett-Luce trees, the effect of covariates on performance
# ................................................................

#' Result returned when no tree is produced
empty_PL_tree = function(reason = "") {
  list(isTREE = FALSE,
       tree_formula = "G ~ 1",
       nobservations_used_tree = 0,
       PLtree = list(),
       PLtree_plot = NULL,
       node_summary = data.frame(),
       regret_table = data.frame(),
       reason = reason)
}

#' Placeholder used when the tree step failed
error_data_PL_tree = empty_PL_tree("error")

#' Prepare the covariates used in the tree
#'
#' Steps (each one is reported with message())
#'   1. find the covariate columns in cmdata
#'   2. impute missing values (median for numbers, "Unknown" for text)
#'   3. drop factors with more than max_levels levels
#'   4. drop covariates with near zero variance
#'   5. drop one covariate of each pair of highly correlated numbers
#'
#' @param cmdata the trial data
#' @param covar the covariates data.frame from rank_dat
#' @param rows logical, the rows used in the tree
#' @return a data.frame with one column per covariate kept, named
#'  after the covariate label
prepare_tree_covariates = function(cmdata, covar, rows, extra = NULL,
                                   max_levels = 10, max_cor = 0.7) {

  if (!is.data.frame(covar) || nrow(covar) == 0) {
    covar = data.frame(codeQst = character(), name = character(),
                       nameString = character(), stringsAsFactors = FALSE)
  }

  covar$codeQst = rename_duplicates(covar$codeQst)
  covar$name = rename_duplicates(covar$name, sep = " ")

  covar$column = find_columns(covar$nameString, names(cmdata), anchor_end = TRUE)

  not_found = covar$name[is.na(covar$column)]
  if (length(not_found) > 0) {
    message("Covariates not found in the data: ", paste(not_found, collapse = ", "))
  }

  covar = covar[!is.na(covar$column) & !duplicated(covar$column), ]

  X = lapply(cmdata[covar$column], function(x) {

    if (is.numeric(x)) {
      x[is.na(x)] = stats::median(x, na.rm = TRUE)
      x = round(x, 2)
    }

    if (is.character(x) || is.logical(x)) {
      x = as.character(x)
      x[is.na(x)] = "Unknown"
      x = as.factor(x)
    }

    x

  })

  X = as.data.frame(X, stringsAsFactors = TRUE)
  if (ncol(X) > 0) {
    names(X) = make.names(gsub(" ", "", title_case(covar$name)), unique = TRUE)
  } else {
    X = data.frame(row.names = seq_len(nrow(cmdata)))
  }

  # extra covariates, such as climate indices, one row per participant
  if (is.data.frame(extra) && ncol(extra) > 0 && nrow(extra) == nrow(cmdata)) {
    extra = extra[, colSums(!is.na(extra)) > 0, drop = FALSE]
    extra[] = lapply(extra, function(x) {
      x[is.na(x)] = stats::median(x, na.rm = TRUE)
      round(x, 2)
    })
    X = cbind(X, extra)
  }

  # too many classes
  too_many = vapply(X, function(x) is.factor(x) && nlevels(x) > max_levels, logical(1))
  if (any(too_many)) {
    message("Covariates with more than ", max_levels, " classes dropped: ",
            paste(names(X)[too_many], collapse = ", "))
  }
  X = X[!too_many]

  X = X[rows, , drop = FALSE]
  rownames(X) = NULL

  if (ncol(X) == 0) {
    return(X)
  }

  # near zero variance
  nzv = caret::nearZeroVar(X, freqCut = 90/10)
  if (length(nzv) > 0) {
    message("Covariates with near zero variance dropped: ", paste(names(X)[nzv], collapse = ", "))
    X = X[-nzv]
  }

  # highly correlated numeric covariates, keep the first of each pair
  num = names(X)[vapply(X, is.numeric, logical(1))]

  if (length(num) > 1) {
    cm = abs(stats::cor(X[num], use = "pairwise.complete.obs"))
    cm[upper.tri(cm, diag = TRUE)] = 0
    drop = num[apply(cm, 1, function(r) any(r > max_cor, na.rm = TRUE))]
    if (length(drop) > 0) {
      message("Correlated covariates dropped: ", paste(drop, collapse = ", "))
      X = X[setdiff(names(X), drop)]
    }
  }

  X

}

#' Forward selection of covariates with Plackett-Luce trees
#'
#' Starting from no covariates, the covariate that gives the tree with
#' the lowest AIC (and at least one split) is added, until no
#' covariate improves the tree.
#'
#' @param data data.frame with the grouped rankings in column G and covariates
#' @param ... arguments passed to pltree()
#' @return the formula as text, "G ~ 1" when nothing was selected
forward_selection = function(data, ...) {

  tree_args = list(...)
  var_keep = character(0)
  candidates = setdiff(names(data), "G")

  message("Selecting the best covariates for the Plackett-Luce tree")

  while (length(candidates) > 0) {

    fits = lapply(candidates, function(v) {
      f = stats::as.formula(paste("G ~", paste(c(var_keep, v), collapse = " + ")))
      t_i = tryCatch(do.call(pltree, c(list(formula = f, data = data), tree_args)),
                     error = function(e) NULL)
      if (is.null(t_i)) {
        return(c(nnodes = NA, AIC = NA))
      }
      c(nnodes = length(nodeids(t_i, terminal = TRUE)), AIC = AIC(t_i))
    })

    fits = do.call("rbind", fits)

    useful = !is.na(fits[, "nnodes"]) & fits[, "nnodes"] > 1

    if (!any(useful)) {
      break
    }

    candidates = candidates[useful]
    fits = fits[useful, , drop = FALSE]

    best = which.min(fits[, "AIC"])
    var_keep = c(var_keep, candidates[best])
    candidates = candidates[-best]

  }

  if (length(var_keep) == 0) {
    return("G ~ 1")
  }

  paste("G ~", paste(var_keep, collapse = " + "))

}

#' Analyse the reference trait with Plackett-Luce trees
#'
#' @param cmdata the (cleaned) trial data
#' @param rank_dat output of organize_ranking_data()
#' @param agroclimate output of get_agroclimatic_data()
#' @param use_climate logical, add the climate indices as covariates
get_PlackettLuce_tree = function(cmdata, rank_dat, agroclimate = NULL, use_climate = TRUE) {

  climate = NULL
  if (isTRUE(use_climate) && isTRUE(agroclimate$agroclimate)) {
    climate = agroclimate$covariates
  }

  if (!isTRUE(rank_dat$covarTRUE) && is.null(climate)) {
    return(empty_PL_tree("no covariates in the request"))
  }

  ref = rank_dat$trait_list[[rank_dat$reference_trait_index]]
  reference_tech = rank_dat$reference_tech
  option = rank_dat$option

  rows = ref$keep

  node_size = ceiling(sum(rows) * 0.25)
  tree_alpha = 0.25

  X = prepare_tree_covariates(cmdata, rank_dat$covar, rows, extra = climate)

  if (ncol(X) == 0) {
    return(empty_PL_tree("no covariates left after the checks"))
  }

  G = rank_tricot(data = cmdata[rows, c(rank_dat$technologies_index, ref$strings)],
                  items = rank_dat$technologies_index,
                  input = ref$strings,
                  validate.rankings = TRUE,
                  group = TRUE)

  Gdata = cbind(G, X)

  tree_args = list(minsize = as.integer(node_size), alpha = tree_alpha, gamma = TRUE)

  treeformula = do.call(forward_selection, c(list(data = Gdata), tree_args))

  if (treeformula == "G ~ 1") {
    return(empty_PL_tree("no covariate splits the data"))
  }

  tree_f = do.call(pltree, c(list(formula = stats::as.formula(treeformula), data = Gdata), tree_args))

  if (length(tree_f) <= 1) {
    return(empty_PL_tree("the tree has no splits"))
  }

  # refit a model in each terminal node
  nodes_tree = predict(tree_f, type = "node")
  node_id_tree = sort(unique(nodes_tree))

  tree_mod = lapply(node_id_tree, function(n) PlackettLuce(Gdata$G[nodes_tree == n]))
  nobs_tree = vapply(node_id_tree, function(n) sum(nodes_tree == n), integer(1))

  tree_branch = gosset:::build_tree_branches(tree_f)
  tree_nodes = gosset:::build_tree_nodes(tree_mod,
                                         log = TRUE,
                                         ref = reference_tech[1],
                                         ci.level = 0.5,
                                         node.ids = node_id_tree,
                                         n.obs = nobs_tree) +
    theme(axis.text.x = element_text(angle = 0))

  plottree = tree_branch / tree_nodes + plot_layout(heights = c(1, 1))

  # regret
  regret_tbl = regret(tree_mod, n1 = 1000)
  names(regret_tbl) = c(title_case(option), "Worth", "Worst Regret", "Regret")
  regret_tbl[2:4] = lapply(regret_tbl[2:4], function(x) round(x, 3))
  regret_tbl = regret_tbl[1:4]

  list(isTREE = TRUE,
       tree_formula = treeformula,
       nobservations_used_tree = nrow(Gdata),
       PLtree = tree_f,
       PLtree_plot = plottree,
       node_summary = summarise_tree_nodes(tree_f, tree_mod, nobs_tree, reference_tech),
       regret_table = regret_tbl,
       reason = "")

}

#' Table with the best and worst items in each node of the tree
summarise_tree_nodes = function(tree_f, tree_mod, nobs_tree, reference_tech) {

  rules = node_rules(tree_f)

  for (i in seq_along(tree_mod)) {

    summ = coef(tree_mod[[i]], log = TRUE, ref = reference_tech)
    summ = summ[!grepl("tie", names(summ))]

    bests = utils::head(names(sort(summ[summ >= 0], decreasing = TRUE)), 3)
    worsts = utils::head(names(sort(summ[summ < 0])), 3)

    rules[i, 3] = paste(setdiff(bests, worsts), collapse = ", ")
    rules[i, 4] = paste(setdiff(worsts, bests), collapse = ", ")
    rules[i, 5] = nobs_tree[i]

  }

  node_summary = rules[c(1, 5, 3, 4, 2)]

  names(node_summary) = c("Node", "N", "Superior Performance",
                          "Underperformance", "Variable and Threshold")

  node_summary

}
