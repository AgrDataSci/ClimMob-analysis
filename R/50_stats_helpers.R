# ................................................................
# Statistical and plotting helpers for ranking data
# ................................................................

#' Add pseudo rankings to a poorly connected network
#'
#' The worst item of each cluster in the network is given random
#' rankings against the others so all items become connected. A fixed
#' seed is used so the same data always gives the same report.
#'
#' @param x a PlackettLuce rankings object
#' @param seed integer, random seed
force_pseudo_rank = function(x, seed = 1L) {

  members = sort(PlackettLuce::connectivity(x)$membership)

  r = unclass(x)

  performance = stats::coefficients(PlackettLuce::PlackettLuce(r))

  # the worst item in each cluster
  members = vapply(split(members, members), function(z) {
    p = performance[names(z)]
    names(p)[which.min(p)]
  }, character(1))

  members = as.character(members)

  max_rank = max(r) + 1

  # only rows that have a ranking
  to_input = rowSums(r) != 0

  half = ceiling(length(members) / 2)
  to_sample = c(rep(0, half), max_rank:(max_rank + half))

  with_seed(seed, {
    sub = r[to_input, members, drop = FALSE]
    sub = t(apply(sub, 1, function(y) {
      where = y == 0
      y[where] = sample(to_sample, size = sum(where))
      y
    }))
    r[to_input, members] = sub
  })

  as.rankings(r)

}

#' Analysis of deviance of a PlackettLuce model against the null model
#'
#' @param model an object of class PlackettLuce
#' @return a data.frame with the test
pl_anova = function(model) {

  if (!inherits(model, "PlackettLuce")) {
    stop("Model type is not Plackett-Luce")
  }

  LLs = round(c(model$null.loglik, model$loglik), 3)
  dfs = c(model$df.null, model$df.residual)
  df_diff = (-1) * diff(dfs)
  df_LL = round((-1) * diff(LLs), 3)
  p = round(1 - stats::pchisq(-2 * df_LL, df_diff), 4)

  data.frame(Model = c("NULL", "model"),
             "logLik" = LLs,
             DF = dfs,
             "Statistic" = c(NA, -2 * df_LL),
             "Pr(>Chisq)" = c(NA, p),
             " " = c("", gtools::stars.pval(p)),
             check.names = FALSE,
             stringsAsFactors = FALSE)

}

#' Build a network (igraph) of the items compared in the rankings
#'
#' @param object an object of class rankings or grouped_rankings
network = function(object, ...) {

  if (inherits(object, "grouped_rankings")) {
    object = as.rankings(object)
  }

  adj = PlackettLuce::adjacency(object)
  adj = t(matrix(as.vector(adj), nrow = ncol(object), ncol = ncol(object)))
  dimnames(adj) = list(dimnames(object)[[2]], dimnames(object)[[2]])

  wins = Matrix::Matrix(adj, sparse = TRUE)

  igraph::graph_from_adjacency_matrix(wins, weighted = TRUE, diag = FALSE)

}

#' Multiple comparisons of a PlackettLuce model, with group letters
multcompPL = function(mod, items = NULL, threshold = 0.05, adjust = "none", ...) {

  qv1 = qvcalc::qvcalc(mod, ...)$qvframe

  if (!is.null(items)) {
    qv1 = subset(qv1, rownames(qv1) %in% items)
    if (nrow(qv1) < 3) {
      stop("Less than 2 items selected")
    }
  }

  est = qv1$estimate
  qvar = qv1$quasiVar

  diffs = outer(est, est, "-")
  ses = sqrt(outer(qvar, qvar, "+"))

  z = diffs / ses
  p = 2 * (1 - stats::pt(abs(z), mod$df.residual))

  p[upper.tri(p)] = stats::p.adjust(p[upper.tri(p)], method = adjust)
  p[lower.tri(p)] = t(p)[lower.tri(p)]
  rownames(p) = colnames(p) = rownames(qv1)

  qv1$items = stats::reorder(factor(rownames(qv1)), qv1$estimate, mean)
  qv1 = qv1[order(qv1$estimate, decreasing = TRUE), ]

  let = multcompView::multcompLetters2(estimate ~ items,
                                       x = p,
                                       data = qv1,
                                       compare = "<",
                                       threshold = threshold,
                                       Letters = letters,
                                       reversed = FALSE)

  qv1$group = let$Letters
  qv1 = qv1[, union("items", names(qv1))]
  row.names(qv1) = seq_along(qv1$group)

  class(qv1) = union("multcompPL", class(qv1))

  qv1

}

#' Log-worth plot with group letters
#'
#' @param x a PlackettLuce model
#' @param ci.level the confidence interval level
#' @param ref the reference item(s), one panel per reference
#' @param multcomp logical, add group letters
#' @param levels optional order of the items
plot_logworth = function(x, ci.level = 0.95, ref = NULL,
                         multcomp = TRUE, levels = NULL, ...) {

  if (length(ref) == 0) {
    stop("No reference item given for the log-worth plot")
  }

  frame = do.call("rbind", lapply(ref, function(r) {
    fi = qvcalc::qvcalc(x, ref = r, ...)$qvframe
    fi$ref = r
    fi$items = rownames(fi)
    fi
  }))

  if (is.null(levels)) {
    levels = union(ref, sort(unique(frame$items)))
  }

  z = stats::qnorm(1 - (1 - ci.level) / 2)

  pdat = data.frame(est = frame$estimate,
                    se = frame$quasiSE,
                    items = frame$items,
                    ref = frame$ref,
                    tops = frame$estimate + z * frame$quasiSE,
                    tails = frame$estimate - z * frame$quasiSE)

  if (isTRUE(multcomp)) {
    lettersdat = multcompPL(mod = x, ...)[, c("items", "group")]
    pdat = merge(pdat, lettersdat, by = "items")
  } else {
    pdat$group = ""
  }

  pdat$items = factor(pdat$items, levels = levels)

  ggplot(data = pdat,
         aes(y = items, x = est, xmax = tops, xmin = tails, label = group)) +
    geom_vline(xintercept = 0, colour = "#E5E7E9", linewidth = 0.8) +
    geom_point() +
    geom_errorbar(width = 0.1) +
    geom_text(hjust = 1.2, vjust = 1.2) +
    theme_bw() +
    facet_wrap(~ ref, strip.position = "bottom") +
    theme(panel.grid.major = element_blank(),
          strip.background.x = element_blank(),
          axis.text.y = element_text(size = 10, color = "grey20"),
          axis.text.x = element_text(size = 10, color = "grey20"),
          text = element_text(color = "grey20"),
          legend.position = "bottom",
          legend.title = element_blank(),
          strip.background.y = element_blank(),
          strip.placement = "outside") +
    labs(y = "", x = tr("lab_logworth"))

}
