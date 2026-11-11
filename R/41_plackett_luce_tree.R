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
       nodes_plot = NULL,
       splits = data.frame(),
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
#' @param coords output of get_trial_coordinates(), one row per row of
#'  cmdata, to draw the nodes of the tree on the map
#' @param adm optional sf object with the boundaries from the trial map
get_PlackettLuce_tree = function(cmdata, rank_dat, agroclimate = NULL, use_climate = TRUE,
                                 coords = NULL, adm = NULL) {

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

  node_summary = summarise_tree_nodes(tree_f, tree_mod, nobs_tree, reference_tech)

  xy = if (!is.null(coords) && nrow(coords) == nrow(cmdata)) coords[rows, , drop = FALSE] else NULL

  nodes_plot = tryCatch(tree_nodes_plot(tree_f, Gdata, nodes_tree, node_summary, xy, adm),
                        error = function(e) {
                          message("Nodes plot not drawn: ", conditionMessage(e))
                          NULL
                        })

  list(isTREE = TRUE,
       tree_formula = treeformula,
       nobservations_used_tree = nrow(Gdata),
       PLtree = tree_f,
       PLtree_plot = plottree,
       node_summary = node_summary,
       nodes_plot = nodes_plot,
       splits = tryCatch(tree_splits(tree_f), error = function(e) data.frame()),
       regret_table = regret_tbl,
       reason = "")

}

#' The splits of the tree
#'
#' @param tree_f a pltree
#' @return a data.frame with one row per inner node, the variable, the
#'  threshold (NA for a factor) and the levels of the two branches
tree_splits = function(tree_f) {

  ids = partykit::nodeids(tree_f, terminal = FALSE)
  ids = setdiff(ids, partykit::nodeids(tree_f, terminal = TRUE))

  if (length(ids) == 0) {
    return(data.frame(node = integer(0), variable = character(0), threshold = numeric(0),
                      stringsAsFactors = FALSE))
  }

  nodes = partykit::nodeapply(tree_f, ids = ids, FUN = function(n) n)

  do.call(rbind, lapply(seq_along(ids), function(i) {
    sp = partykit::split_node(nodes[[i]])
    ch = partykit::character_split(sp, data = tree_f$data)
    br = partykit::breaks_split(sp)
    data.frame(node = ids[i],
               variable = ch$name,
               threshold = if (is.null(br)) NA_real_ else br[1],
               stringsAsFactors = FALSE)
  }))

}

#' The nodes of the tree on the map and the split variables
#'
#' Left, the trial plots coloured by the terminal node they fall in, so
#' the reader sees where each group of the tree is. Right, one panel per
#' numeric split variable with the distribution across participants and
#' the threshold of the split. Without coordinates only the split panels
#' are drawn.
#'
#' @param tree_f the pltree
#' @param Gdata the data used in the tree, rankings in G and covariates
#' @param nodes integer vector, the terminal node of each row of Gdata
#' @param node_summary from summarise_tree_nodes()
#' @param xy data.frame with longitude and latitude, one row per row of
#'  Gdata, or NULL
#' @param adm optional sf boundaries
#' @return a ggplot or patchwork, NULL when there is nothing to draw
tree_nodes_plot = function(tree_f, Gdata, nodes, node_summary, xy = NULL, adm = NULL) {

  ids = sort(unique(nodes))
  rules = node_summary[match(ids, node_summary[[1]]), 5]
  lab = paste0(tr("col_node"), " ", ids, ", ", gsub("climate_", "", rules))
  node_f = factor(nodes, levels = ids, labels = lab)
  cols = pal_qualitative(length(ids))

  theme_small = theme_bw() +
    theme(panel.grid = element_blank(), text = element_text(size = 13),
          legend.position = "bottom", legend.title = element_blank())

  map = NULL
  if (!is.null(xy) && any(stats::complete.cases(xy[c("longitude", "latitude")]))) {
    d = data.frame(longitude = xy$longitude, latitude = xy$latitude, node = node_f)
    d = d[stats::complete.cases(d[c("longitude", "latitude")]), ]
    map = ggplot()
    if (!is.null(adm)) {
      map = map + geom_sf(data = adm, fill = "#f4f5f0", colour = "grey60", linewidth = 0.2)
    }
    map = map +
      geom_point(data = d, aes(x = longitude, y = latitude, fill = node),
                 shape = 21, size = 2.6, colour = "grey20", stroke = 0.4) +
      scale_fill_manual(values = cols, drop = FALSE) +
      guides(fill = guide_legend(ncol = 1)) +
      labs(x = NULL, y = NULL) +
      coord_sf(crs = sf::st_crs(MAP_CRS), default_crs = sf::st_crs(4326)) +
      theme_minimal() +
      theme(legend.position = "bottom", legend.title = element_blank(),
            text = element_text(size = 13, colour = "grey20"))
  }

  splits = tree_splits(tree_f)
  splits = splits[!is.na(splits$threshold) & splits$variable %in% names(Gdata), ]
  splits = splits[!duplicated(splits$variable), ]

  panels = lapply(seq_len(nrow(splits)), function(i) {
    v = splits$variable[i]
    d = data.frame(x = as.numeric(Gdata[[v]]), node = node_f)
    ggplot(d, aes(x = x)) +
      geom_histogram(aes(fill = node), bins = 20, colour = "white", linewidth = 0.2) +
      geom_vline(xintercept = splits$threshold[i], linetype = "dashed", colour = "grey20") +
      scale_fill_manual(values = cols, drop = FALSE) +
      labs(x = gsub("climate_", "", v), y = tr("lab_participants")) +
      theme_small +
      theme(legend.position = "none")
  })

  if (is.null(map) && length(panels) == 0) {
    return(NULL)
  }

  if (is.null(map)) {
    return(patchwork::wrap_plots(panels, ncol = min(2, length(panels))))
  }

  if (length(panels) == 0) {
    return(map)
  }

  right = patchwork::wrap_plots(panels, ncol = 1)
  map + right + plot_layout(widths = c(1.4, 1))

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
