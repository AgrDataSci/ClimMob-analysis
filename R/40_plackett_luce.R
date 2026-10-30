# ................................................................
# Plackett-Luce analysis of the ranking traits
#
# Each output (log-worth plots, worth map, ANOVA, net winning,
# reliability, Kendall tau) has its own function and runs as its own
# pipeline step, so a failure in one does not remove the others from
# the report. assemble_PL_models() puts them back together in the
# PL_models list used by the report template.
# ................................................................

#' Rankings for every trait kept in rank_dat
#'
#' Rankings with a poorly connected network get pseudo rankings so
#' the model can be fitted.
build_trait_rankings = function(cmdata, rank_dat) {

  R = lapply(rank_dat$trait_list, function(x) {
    rank_tricot(cmdata[, c(rank_dat$technologies_index, x$strings)],
                items = rank_dat$technologies_index,
                input = x$strings,
                validate.rankings = TRUE)
  })

  poorly_connected = vapply(R, function(x) {
    connectivity(x, verbose = FALSE)$no > 1
  }, logical(1))

  R[poorly_connected] = lapply(R[poorly_connected], force_pseudo_rank)

  R

}

#' Fit one Plackett-Luce model per trait
fit_trait_models = function(R) {
  lapply(R, PlackettLuce)
}

#' Log-worth plot for each trait, centred on the reference item(s)
make_logworth_plots = function(mod, rank_dat) {

  plots = list()

  for (m in seq_along(mod)) {

    p = tryCatch(
      plot_logworth(mod[[m]], ref = rank_dat$reference_tech, ci.level = 0.5) +
        labs(title = paste0(rank_dat$trait_names[m], " (n = ", length(mod[[m]]$rankings), ")")),
      error = function(e) {
        message("Log-worth plot failed for '", rank_dat$trait_names[m], "': ", conditionMessage(e))
        NULL
      })

    plots[m] = list(p)

  }

  overall = plots[[rank_dat$reference_trait_index]]

  list(logworth_overall = overall,
       logworth_plot = Filter(Negate(is.null), plots))

}

#' Worth map, the performance of every item in every trait
make_worthmap = function(mod, rank_dat) {

  worth_map(mod, labels = rank_dat$trait_names, labels.order = rev(rank_dat$trait_names)) +
    labs(x = "", y = "") +
    theme(axis.text = element_text(color = "grey20"),
          strip.text.x = element_text(color = "grey20"),
          axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1, color = "grey20"),
          axis.text.y = element_text(size = 10, angle = 0, vjust = 1, hjust = 1, color = "grey20"))

}

#' ANOVA table, one line per trait
make_anova_table = function(mod, rank_dat) {

  aov_tbl = lapply(seq_along(mod), function(i) {
    tr = rank_dat$trait_list[[i]]
    a = pl_anova(mod[[i]])[-1, ]
    a[1, "Model"] = paste0(tr$name, " [", tr$assessment, "]")
    a
  })

  aov_tbl = do.call("rbind", aov_tbl)
  rownames(aov_tbl) = seq_len(nrow(aov_tbl))
  names(aov_tbl)[names(aov_tbl) == "Model"] = "Trait"

  aov_tbl

}

#' Net winning score of every item in every trait
make_netwinning_table = function(R, rank_dat) {

  nw = lapply(seq_along(R), function(i) {
    x = preference_index(R[[i]])[, 1:4]
    x$trait = rank_dat$trait_names[i]
    x
  })

  nw = do.call("rbind", nw)
  nw$trait[duplicated(nw$trait)] = ""
  names(nw)[1] = rank_dat$option
  nw = nw[c(5, 1, 2, 3, 4)]
  nw[3:4] = lapply(nw[3:4], function(x) round(x, 1))
  names(nw) = title_case(names(nw))

  nw

}

#' Reliability, the probability of outperforming each check
#'
#' The data for all checks is returned (previous versions kept only
#' the last check), with a column "check".
make_reliability = function(mod, rank_dat) {

  trait_names = rank_dat$trait_names
  reliability_data = data.frame()
  reliability_plot = list()

  for (check in rank_dat$reference_tech) {

    rel = do.call("rbind", lapply(seq_along(mod), function(i) {
      r = reliability(mod[[i]], ref = check)
      r$trait = trait_names[i]
      r
    }))

    rel$trait = factor(rel$trait, levels = trait_names)
    rel$improvement = round(rel$reliability / 0.5 - 1, 2)
    rel = rel[order(rel$improvement), ]
    rel$item = factor(rel$item, levels = unique(rel$item))
    rel = rel[!rel$item %in% check, ]
    rel$check = check

    reliability_data = rbind(reliability_data, rel)

    reliability_plot[[length(reliability_plot) + 1]] =
      ggplot(data = rel, aes(x = improvement, y = trait, fill = "white")) +
      geom_bar(stat = "identity", width = 0.7, position = "dodge", show.legend = FALSE) +
      scale_fill_manual(values = "#5aae61") +
      facet_wrap(~ item, strip.position = "bottom") +
      geom_vline(xintercept = 0, colour = "grey40", linewidth = 1) +
      theme_classic() +
      labs(x = paste("Probability of outperforming", check), y = "")

  }

  list(reliability_plot = reliability_plot,
       reliability_data = reliability_data)

}

#' Kendall tau between the reference trait and the other traits
make_kendall = function(cmdata, rank_dat) {

  trait_list = rank_dat$trait_list
  ref_i = rank_dat$reference_trait_index
  items = rank_dat$technologies_index

  if (length(trait_list) < 2) {
    return(list(kendall = data.frame(),
                strongest_link = "",
                weakest_link = "",
                kendall_plot = NULL))
  }

  rank_of = function(strings) {
    rank_tricot(cmdata[, c(items, strings)], items = items,
                input = strings, validate.rankings = TRUE)
  }

  r1 = rank_of(trait_list[[ref_i]]$strings)

  others = trait_list[-ref_i]

  kendall = do.call("rbind", lapply(others, function(x) {
    kendallTau(r1, rank_of(x$strings), na.omit = FALSE)
  }))

  kendall$trait = vapply(others, function(x) paste0(x$name, " [", x$assessment, "]"),
                         character(1), USE.NAMES = FALSE)

  kendall = kendall[order(kendall$kendallTau, decreasing = TRUE), ]

  strongest_link = c(kendall$trait[1], round(kendall$kendallTau[1], 2))
  weakest_link = c(kendall$trait[nrow(kendall)], round(kendall$kendallTau[nrow(kendall)], 2))

  kendall = kendall[, c("trait", "kendallTau", "Zvalue", "Pr(>|z|)")]
  kendall$kendallTau = round(kendall$kendallTau, 3)
  kendall$Zvalue = round(kendall$Zvalue, 3)
  stars = stars.pval(kendall[, "Pr(>|z|)"])
  kendall[, "Pr(>|z|)"] = paste(format(kendall[, "Pr(>|z|)"], scientific = TRUE, digits = 3), stars)
  kendall$trait = factor(kendall$trait, levels = rev(kendall$trait))

  kendall_plot = ggplot(data = kendall, aes(y = kendallTau, x = trait, fill = trait)) +
    geom_chicklet(show.legend = FALSE) +
    coord_flip() +
    scale_fill_manual(values = col_pallet(nrow(kendall))) +
    theme_classic() +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 12, color = "grey20"),
          axis.text.y = element_text(size = 12, color = "grey20"),
          axis.title = element_text(size = 12, color = "grey20"),
          axis.text.x = element_text(size = 12, vjust = 1, hjust = 1, color = "grey20")) +
    labs(x = "Trait", y = "Kendall tau")

  names(kendall) = c("Trait", "Kendall tau", "Z value", "Pr(>|z|)")
  row.names(kendall) = seq_len(nrow(kendall))

  list(kendall = kendall,
       strongest_link = strongest_link,
       weakest_link = weakest_link,
       kendall_plot = kendall_plot)

}

#' Put the parts back together in the list used by the report
assemble_PL_models = function(mod, logworth, worthmap, anova_tbl,
                              netwinning, reliability, kendall) {

  list(PL_models = mod,
       logworth_overall = logworth$logworth_overall,
       netwinning = netwinning,
       worthmap = worthmap,
       ANOVA = anova_tbl,
       logworth_plot = logworth$logworth_plot,
       kendall = kendall,
       reliability_plot = reliability$reliability_plot,
       reliability_data = reliability$reliability_data)

}

# Placeholders used when a part could not be produced
error_data_logworth = list(logworth_overall = NULL, logworth_plot = list())
error_data_reliability = list(reliability_plot = list(), reliability_data = data.frame())
error_data_kendall = list(kendall = data.frame(), strongest_link = "",
                          weakest_link = "", kendall_plot = NULL)
