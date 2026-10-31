# ................................................................
# Evidence for variety release (issue #117)
#
# Tables that a breeding team can take to a variety release committee
#   - every item against the check, trait by trait, with the
#     probability of outperforming the check and a significance test
#   - one summary line per item with the favourability score
#     (net winning probability), the traits where it is better or
#     worse than the check and, when measured, the yield relative
#     to the check
# ................................................................

#' Compare every item with a check in one Plackett-Luce model
#'
#' @param model a PlackettLuce model
#' @param check the check item
#' @return a data.frame with the log-worth relative to the check, its
#'  standard error, the probability of outperforming the check and a
#'  two-sided p-value
compare_with_check = function(model, check) {

  qv = qvcalc::qvcalc(model, ref = check)$qvframe
  qv = qv[!grepl("^tie", rownames(qv)), , drop = FALSE]

  se = sqrt(qv$quasiVar + qv[check, "quasiVar"])
  z = qv$estimate / se

  out = data.frame(item = rownames(qv),
                   logworth_vs_check = round(qv$estimate, 3),
                   se = round(se, 3),
                   prob_beats_check = round(stats::plogis(qv$estimate), 3),
                   p_value = signif(2 * stats::pnorm(-abs(z)), 3),
                   stringsAsFactors = FALSE)

  out$p_value[out$item == check] = NA

  out

}

#' Every item against the check, for every trait
trait_check_table = function(models, rank_dat, check, alpha = 0.05) {

  out = do.call("rbind", lapply(seq_along(models), function(i) {
    x = tryCatch(compare_with_check(models[[i]], check), error = function(e) NULL)
    if (is.null(x)) return(NULL)
    cbind(trait = rank_dat$trait_names[i], check = check, x, stringsAsFactors = FALSE)
  }))

  if (is.null(out)) {
    return(data.frame())
  }

  out$result = ifelse(is.na(out$p_value) | out$p_value >= alpha, "similar",
                      ifelse(out$logworth_vs_check > 0, "better", "worse"))
  out$result[out$item == check] = "check"

  out

}

#' One line per item with the evidence against the check
#'
#' @param rank_dat output of organize_ranking_data()
#' @param models list of PlackettLuce models, one per trait
#' @param rankings list of rankings, one per trait
#' @param metric output of get_metric_analysis()
#' @param check the check item, the first reference item by default
get_release_summary = function(cmdata, rank_dat, models, rankings,
                               metric = NULL, check = rank_dat$reference_tech[1]) {

  items = rank_dat$technologies
  ref_i = rank_dat$reference_trait_index

  by_trait = trait_check_table(models, rank_dat, check)

  tested = table(unlist(cmdata[, rank_dat$technologies_index]))

  ref = by_trait[by_trait$trait == rank_dat$trait_names[ref_i], , drop = FALSE]

  fav = tryCatch(preference_index(rankings[[ref_i]]), error = function(e) NULL)

  out = data.frame(item = items, stringsAsFactors = FALSE)
  out$n_tested = as.integer(tested[items])
  out$prob_beats_check = ref$prob_beats_check[match(items, ref$item)]
  out$p_value = ref$p_value[match(items, ref$item)]

  if (!is.null(fav)) {
    out$favourability = round(fav$net_winning[match(items, fav$item)], 1)
  }

  list_traits = function(it, what) {
    x = by_trait$trait[by_trait$item == it & by_trait$result == what]
    paste(x, collapse = "; ")
  }

  out$better_than_check = vapply(items, list_traits, character(1), what = "better")
  out$worse_than_check = vapply(items, list_traits, character(1), what = "worse")

  # yield measured in the trial, relative to the check
  if (isTRUE(metric$available)) {
    yld = Filter(function(x) grepl("yield", tolower(x$trait_code)) && nrow(x$means) > 0,
                 metric$traits)
    if (length(yld) > 0) {
      ym = yld[[1]]$means
      out$yield_mean = round(ym$mean[match(items, ym$genotype)], 2)
      if ("percent_of_check" %in% names(ym)) {
        out$yield_percent_of_check = round(ym$percent_of_check[match(items, ym$genotype)], 1)
      }
    }
  }

  out = out[order(-out$prob_beats_check, na.last = TRUE), ]
  out$item_is_check = out$item == check
  rownames(out) = NULL

  # the same comparison against every check requested
  others = setdiff(rank_dat$reference_tech, check)
  all_checks = do.call("rbind", c(list(by_trait),
                                  lapply(others, function(ck) trait_check_table(models, rank_dat, ck))))

  list(check = check, summary = out, by_trait = by_trait, by_trait_all_checks = all_checks)

}

#' Placeholder used when the release summary fails
error_data_release = list(check = NA, summary = data.frame(), by_trait = data.frame())
