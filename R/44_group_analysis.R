# ................................................................
# Analysis by group of participants (Split parameter, issue #55)
#
# A group comparison is only as good as the smallest group. Before
# any model is fitted each group is checked
#   - groups with missing labels ("Not specified") are not compared
#   - a group needs at least min_n valid rankings of the reference trait
#   - at least two groups must pass for a comparison
# When the groups are unbalanced (the smallest eligible group holds
# less than balance_threshold of the participants) the pooled ranking
# is dominated by the largest group. The report then also shows a
# balanced ranking in which every group has the same total weight.
# ................................................................

#' Which groups have enough data to be analysed
#'
#' @param group the grouping vector, one value per participant
#' @param keep logical, the participants with a valid answer
#' @param min_n minimum number of valid answers per group
#' @param exclude labels that are never compared
#' @return a data.frame with group, n_valid, share, eligible and reason
#' @examples
#' g = c(rep("Man", 40), rep("Woman", 6), "Not specified")
#' group_eligibility(g, rep(TRUE, 47), min_n = 20)
group_eligibility = function(group, keep, min_n = 20,
                             exclude = c("Not specified", NA)) {

  g = as.character(group[keep])

  tb = table(g, useNA = "no")

  labelled = sum(tb[!names(tb) %in% exclude])

  out = data.frame(group = names(tb),
                   n_valid = as.integer(tb),
                   share = if (labelled > 0) round(as.integer(tb) / labelled, 3) else NA_real_,
                   stringsAsFactors = FALSE)

  out$eligible = out$n_valid >= min_n & !out$group %in% exclude
  out$reason = ifelse(out$group %in% exclude, "no_label",
                      ifelse(out$n_valid < min_n, "too_few", "ok"))
  out$share[out$group %in% exclude] = NA

  out[order(-out$n_valid), , drop = FALSE]

}

#' Weights that give every group the same total weight
#'
#' The weights average 1, so standard errors stay on the scale of
#' the number of participants.
#' @examples balanced_weights(c("a", "a", "a", "b"))
balanced_weights = function(group) {

  n = table(group)
  w = 1 / as.numeric(n[as.character(group)])
  w / mean(w)

}

#' Agreement of each ranking with a consensus ranking
#'
#' Kendall tau between the order given by each participant and the
#' order of the same items in the consensus (worth). 1 is full
#' agreement, -1 the reverse order.
#'
#' @param r a matrix of ranks (rows participants, columns items, 0 = not ranked)
#' @param worth named vector with the consensus worth of the items
#' @return one tau per row, NA for rows with fewer than two items
ranking_agreement = function(r, worth) {

  r = as.matrix(r)

  apply(r, 1, function(x) {

    items = names(x)[x > 0 & names(x) %in% names(worth)]

    if (length(items) < 2) {
      return(NA_real_)
    }

    pairs = utils::combn(items, 2)

    s_obs = sign(x[pairs[2, ]] - x[pairs[1, ]])      # positive when the first item is better
    s_con = sign(worth[pairs[1, ]] - worth[pairs[2, ]])

    ok = s_obs != 0 & s_con != 0

    if (!any(ok)) {
      return(NA_real_)
    }

    sum(s_obs[ok] == s_con[ok]) / sum(ok) * 2 - 1

  })

}

#' Likelihood ratio test of differences between groups
#'
#' Compares one model per group with a single pooled model.
group_lr_test = function(R, group, groups) {

  rows = group %in% groups & rowSums(unclass(R) > 0) > 1

  pooled = PlackettLuce(R[rows])

  sep = lapply(groups, function(g) PlackettLuce(R[rows & group == g]))

  ll_sep = sum(vapply(sep, function(m) as.numeric(logLik(m)), numeric(1)))
  df_sep = sum(vapply(sep, function(m) attr(logLik(m), "df"), numeric(1)))

  lr = 2 * (ll_sep - as.numeric(logLik(pooled)))
  df = df_sep - attr(logLik(pooled), "df")

  data.frame(statistic = round(lr, 2),
             df = df,
             p_value = if (df > 0) stats::pchisq(lr, df, lower.tail = FALSE) else NA_real_)

}

#' Analyse the rankings by group
#'
#' @param rank_dat output of organize_ranking_data()
#' @param rankings output of build_trait_rankings()
#' @param min_n minimum number of valid answers per group
#' @param balance_threshold share below which a group is a small minority
get_group_analysis = function(rank_dat, rankings, min_n = 20, balance_threshold = 0.25) {

  group = rank_dat$group

  if (is.null(group)) {
    return(list(requested = FALSE, available = FALSE))
  }

  ref_i = rank_dat$reference_trait_index
  keep = rank_dat$trait_list[[ref_i]]$keep

  elig = group_eligibility(group, keep, min_n = min_n)
  groups = elig$group[elig$eligible]

  out = list(requested = TRUE,
             available = length(groups) >= 2,
             eligibility = elig,
             groups = groups,
             min_n = min_n,
             unbalanced = FALSE)

  labelled = elig[elig$reason != "no_label", , drop = FALSE]
  out$smallest = labelled[which.min(labelled$share), , drop = FALSE]
  out$unbalanced = nrow(labelled) > 1 && isTRUE(min(labelled$share) < balance_threshold)

  if (!out$available) {
    return(out)
  }

  # differences between groups, trait by trait
  het = lapply(seq_along(rankings), function(t) {
    R = rankings[[t]]
    kt = rank_dat$trait_list[[t]]$keep
    n_g = vapply(groups, function(g) sum(kt & group == g), numeric(1))
    if (any(n_g < min_n)) {
      return(data.frame(trait = rank_dat$trait_names[t], statistic = NA, df = NA,
                        p_value = NA, note = "too_few"))
    }
    res = tryCatch(group_lr_test(R, group, groups), error = function(e) NULL)
    if (is.null(res)) {
      return(data.frame(trait = rank_dat$trait_names[t], statistic = NA, df = NA,
                        p_value = NA, note = "failed"))
    }
    cbind(trait = rank_dat$trait_names[t], res, note = "")
  })

  het = do.call("rbind", het)
  het$p_value = signif(het$p_value, 3)
  out$heterogeneity = het

  # reference trait, one model per group
  R = rankings[[ref_i]]
  rows = keep & group %in% groups & rowSums(unclass(R) > 0) > 1

  group_models = lapply(groups, function(g) PlackettLuce(R[rows & group == g]))
  names(group_models) = groups
  out$group_models = group_models

  out$worthmap = tryCatch(
    worth_map(group_models, labels = groups) +
      labs(x = "", y = "") +
      theme(axis.text.x = element_text(angle = 0)),
    error = function(e) NULL)

  # agreement between groups (Kendall tau of the worth of shared items)
  worths = lapply(group_models, function(m) stats::coef(m, log = FALSE))
  common = Reduce(intersect, lapply(worths, names))
  common = common[!grepl("^tie", common)]

  if (length(groups) >= 2 && length(common) >= 3) {
    pairs = utils::combn(groups, 2)
    out$between = data.frame(group_1 = pairs[1, ], group_2 = pairs[2, ],
                             kendall_tau = round(apply(pairs, 2, function(p) {
                               stats::cor(worths[[p[1]]][common], worths[[p[2]]][common],
                                          method = "kendall")
                             }), 2),
                             stringsAsFactors = FALSE)
  }

  # agreement within groups, each participant against the group consensus
  r = unclass(R)

  out$within = do.call("rbind", lapply(groups, function(g) {
    tau = ranking_agreement(r[rows & group == g, , drop = FALSE], worths[[g]])
    data.frame(group = g, n = sum(!is.na(tau)), mean_tau = round(mean(tau, na.rm = TRUE), 2))
  }))

  # pooled and balanced rankings
  pooled = PlackettLuce(R[rows])
  balanced = PlackettLuce(R[rows], weights = balanced_weights(group[rows]))

  wp = stats::coef(pooled, log = FALSE)
  wb = stats::coef(balanced, log = FALSE)
  items = names(wp)[!grepl("^tie", names(wp))]

  tbl = data.frame(item = items,
                   worth_pooled = round(100 * wp[items] / sum(wp[items]), 1),
                   worth_balanced = round(100 * wb[items] / sum(wb[items]), 1),
                   stringsAsFactors = FALSE)
  tbl$rank_pooled = rank(-tbl$worth_pooled, ties.method = "min")
  tbl$rank_balanced = rank(-tbl$worth_balanced, ties.method = "min")
  tbl = tbl[order(tbl$rank_balanced), ]
  rownames(tbl) = NULL

  out$balanced_ranking = tbl

  out

}

#' Placeholder used when the group analysis fails
error_data_group_analysis = list(requested = FALSE, available = FALSE)
