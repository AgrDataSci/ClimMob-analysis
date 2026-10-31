# ................................................................
# Analysis of metric (quantitative) traits, issue #107
#
# Implements the workflow proposed by Hugo Dorado for traits such as
# yield, plant population or number of cobs
#   1. zeros are treated as missing plots (optional)
#   2. quality control, missing plots and outliers by the IQR rule
#   3. linear mixed model  response ~ genotype + (1 | farm)
#      giving the mean of each genotype adjusted for the farm
#   4. comparison of every genotype with the check, as difference
#      and as percentage of the check
#   5. model with random genotype  response ~ (1 | genotype) + (1 | farm)
#      giving the variance components and the broad-sense heritability
#      by the ad hoc method of Holland et al. (2003)
# When lme4 is not installed or the mixed model fails, farm is used as a
# fixed effect (lm) and the heritability is not reported.
# ................................................................

#' Bounds of the IQR rule (Q1 - k IQR, Q3 + k IQR)
#' @examples iqr_bounds(c(1:10, 50))
iqr_bounds = function(x, k = 1.5) {
  q = stats::quantile(x, c(0.25, 0.75), na.rm = TRUE, names = FALSE)
  c(lower = q[1] - k * diff(q), upper = q[2] + k * diff(q))
}

#' Broad-sense heritability, ad hoc method of Holland et al. (2003)
#'
#' @param var_g genetic variance
#' @param var_e residual variance
#' @param n_obs number of observations of each genotype
#' @examples holland_h2(2, 4, c(10, 10, 10))
holland_h2 = function(var_g, var_e, n_obs) {
  n_h = length(n_obs) / sum(1 / n_obs)
  var_g / (var_g + var_e / n_h)
}

#' Genotype means and comparisons with the check from a fitted model
#'
#' Works with the fixed effects of any linear model with a factor
#' "genotype". Other factors (such as farm as a fixed effect) are
#' averaged with equal weights, as estimated marginal means do.
#'
#' @param beta the fixed effects
#' @param V their variance-covariance matrix
#' @param levels the genotype levels, the first one is the baseline
#' @param check the check genotype
#' @param df degrees of freedom for t tests, Inf for z tests
#' @return a data.frame with one row per genotype
genotype_means = function(beta, V, levels, check = NULL, df = Inf) {

  ok = !is.na(beta)
  beta = beta[ok]
  V = as.matrix(V)[names(beta), names(beta), drop = FALSE]

  L = matrix(0, nrow = length(levels), ncol = length(beta),
             dimnames = list(levels, names(beta)))

  L[, "(Intercept)"] = 1

  for (g in levels[-1]) {
    col = paste0("genotype", g)
    if (col %in% colnames(L)) L[g, col] = 1
  }

  # other factors averaged over their levels (the baseline level counts as 0)
  other = setdiff(colnames(L), c("(Intercept)", paste0("genotype", levels)))
  if (length(other) > 0) {
    prefixes = unique(sub("^([a-z]+).*$", "\\1", other))
    for (pf in prefixes) {
      cols = other[startsWith(other, pf)]
      L[, cols] = 1 / (length(cols) + 1)
    }
  }

  est = as.vector(L %*% beta)
  se = sqrt(pmax(diag(L %*% V %*% t(L)), 0))
  q = if (is.finite(df)) stats::qt(0.975, df) else stats::qnorm(0.975)

  out = data.frame(genotype = levels,
                   mean = est,
                   se = se,
                   lower = est - q * se,
                   upper = est + q * se,
                   stringsAsFactors = FALSE)

  if (!is.null(check) && check %in% levels) {

    Lc = L - matrix(L[check, ], nrow = nrow(L), ncol = ncol(L), byrow = TRUE)
    d = as.vector(Lc %*% beta)
    sd = sqrt(pmax(diag(Lc %*% V %*% t(Lc)), 0))
    stat = ifelse(sd > 0, d / sd, NA)

    out$diff_vs_check = d
    out$percent_of_check = 100 * est / est[levels == check]
    out$p_vs_check = if (is.finite(df)) 2 * stats::pt(-abs(stat), df) else 2 * stats::pnorm(-abs(stat))
    out$p_vs_check[levels == check] = NA

  }

  out

}

#' Analyse one metric trait
#'
#' @param d a data.frame with columns farm, genotype and value
#' @param check the check genotype (reference item)
#' @param remove_zeros logical, treat zeros as missing plots
#' @param remove_outliers logical, leave outliers out of the model
#' @param min_obs genotypes with fewer observations are left out of the model
analyse_metric_trait = function(d, check = NULL, remove_zeros = TRUE,
                                remove_outliers = TRUE, min_obs = 3) {

  d = d[!is.na(d$genotype), c("farm", "genotype", "value")]
  d$value = suppressWarnings(as.numeric(d$value))
  d$farm = as.character(d$farm)
  d$genotype = as.character(d$genotype)

  n_zero = sum(d$value == 0, na.rm = TRUE)

  if (isTRUE(remove_zeros)) {
    d$value[!is.na(d$value) & d$value == 0] = NA
  }

  b = iqr_bounds(d$value)
  is_out = !is.na(d$value) & (d$value < b[["lower"]] | d$value > b[["upper"]])

  missing_by_farm = tapply(is.na(d$value), d$farm, sum)

  qc = data.frame(plots = nrow(d),
                  farms = length(unique(d$farm)),
                  missing_plots = sum(is.na(d$value)),
                  zeros = n_zero,
                  farms_with_missing = sum(missing_by_farm > 0),
                  outliers = sum(is_out),
                  lower_bound = round(b[["lower"]], 3),
                  upper_bound = round(b[["upper"]], 3))

  outliers = d[is_out, , drop = FALSE]
  outliers = outliers[order(outliers$value), , drop = FALSE]

  d$value_used = d$value
  if (isTRUE(remove_outliers)) d$value_used[is_out] = NA

  m = d[!is.na(d$value_used), ]

  n_obs = table(m$genotype)
  keep_g = names(n_obs)[n_obs >= min_obs]
  dropped = setdiff(unique(d$genotype), keep_g)
  m = m[m$genotype %in% keep_g, ]

  result = list(qc = qc, outliers = outliers, dropped = dropped,
                data = d, means = data.frame(), variance = data.frame(),
                method = "none")

  if (length(keep_g) < 2 || length(unique(m$farm)) < 3) {
    result$note = "not_enough_data"
    return(result)
  }

  # the check is the baseline, so differences are against it
  lev = sort(keep_g)
  if (!is.null(check) && check %in% lev) lev = c(check, setdiff(lev, check))
  m$genotype = factor(m$genotype, levels = lev)
  m$farm = factor(m$farm)

  fit = NULL

  if (requireNamespace("lme4", quietly = TRUE)) {
    fit = tryCatch(lme4::lmer(value_used ~ genotype + (1 | farm), data = m),
                   error = function(e) NULL)
  }

  if (!is.null(fit)) {
    means = genotype_means(lme4::fixef(fit), as.matrix(stats::vcov(fit)), lev, check)
    result$method = "lmer"
  } else {
    fit = stats::lm(value_used ~ genotype + farm, data = m)
    means = genotype_means(stats::coef(fit), stats::vcov(fit), lev, check,
                           df = stats::df.residual(fit))
    result$method = "lm"
  }

  means$n = as.integer(table(m$genotype)[means$genotype])
  means = means[order(-means$mean), ]
  rownames(means) = NULL

  # group letters, genotypes sharing a letter are not different at p = 0.05
  means$group = ""
  if (requireNamespace("multcompView", quietly = TRUE) && nrow(means) > 2) {
    means$group = tryCatch(pairwise_letters(fit, lev, means$genotype, result$method),
                           error = function(e) "")
  }

  result$means = means

  # variance components and heritability
  if (result$method == "lmer") {
    fit2 = tryCatch(lme4::lmer(value_used ~ (1 | genotype) + (1 | farm), data = m),
                    error = function(e) NULL)
    if (!is.null(fit2)) {
      vc = as.data.frame(lme4::VarCorr(fit2))
      var_g = vc$vcov[vc$grp == "genotype"]
      var_f = vc$vcov[vc$grp == "farm"]
      var_e = vc$vcov[vc$grp == "Residual"]
      result$variance = data.frame(
        parameter = c("genetic_variance", "farm_variance", "residual_variance", "heritability"),
        estimate = round(c(var_g, var_f, var_e, holland_h2(var_g, var_e, as.numeric(table(m$genotype)))), 3))
    }
  }

  result

}

#' Group letters from all pairwise comparisons of genotype means
pairwise_letters = function(fit, lev, order, method) {

  if (method == "lmer") {
    beta = lme4::fixef(fit); V = as.matrix(stats::vcov(fit)); df = Inf
  } else {
    beta = stats::coef(fit); V = stats::vcov(fit); df = stats::df.residual(fit)
  }

  p = matrix(1, length(lev), length(lev), dimnames = list(lev, lev))

  for (a in lev) {
    gm = genotype_means(beta, V, lev, check = a, df = df)
    p[gm$genotype, a] = ifelse(is.na(gm$p_vs_check), 1, gm$p_vs_check)
  }

  let = multcompView::multcompLetters(p, threshold = 0.05)$Letters

  as.vector(let[order])

}

#' Analyse every quantitative trait in quanti_dat
#'
#' @param quanti_dat output of organize_quantitative_data()
#' @param check the check item
get_metric_analysis = function(quanti_dat, check = NULL, remove_zeros = TRUE,
                               remove_outliers = TRUE) {

  if (!isTRUE(quanti_dat$quantitative)) {
    return(list(available = FALSE, traits = list()))
  }

  traits = lapply(quanti_dat$quanti_dat, function(x) {

    d = data.frame(farm = x$id, genotype = x$technology, value = x$value)

    res = analyse_metric_trait(d, check = check, remove_zeros = remove_zeros,
                               remove_outliers = remove_outliers)

    res$trait = unique(x$trait)[1]
    res$trait_code = unique(x$trait_code)[1]
    res$moment = unique(x$data_collection)[1]
    res$plots = metric_plots(res)

    res

  })

  names(traits) = vapply(traits, function(x) paste(x$trait, x$moment, sep = " - "), character(1))

  list(available = length(traits) > 0, traits = traits, check = check)

}

#' Density and boxplot of a metric trait (outliers removed)
metric_plots = function(res) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    return(list())
  }

  d = res$data[!is.na(res$data$value_used), ]

  if (nrow(d) < 3) {
    return(list())
  }

  density = ggplot2::ggplot(d, ggplot2::aes(x = value_used)) +
    ggplot2::geom_density(fill = "steelblue", alpha = 0.4) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = res$trait, y = tr("lab_density"))

  box = ggplot2::ggplot(d, ggplot2::aes(x = value_used,
                                        y = stats::reorder(genotype, value_used, FUN = stats::median))) +
    ggplot2::geom_boxplot(fill = "steelblue", alpha = 0.4) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = res$trait, y = "")

  list(density = density, box = box)

}

#' Placeholder used when the metric analysis fails
error_data_metric = list(available = FALSE, traits = list())
