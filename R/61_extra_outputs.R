# ................................................................
# Extra outputs sent back to ClimMob
#
# Figures (vector PDF) and tables written to <output_path>/extra-outputs/
# The file names are the same as in previous versions because ClimMob
# shows them to the users.
# ................................................................

#' Save a plot, ignoring objects that are not plots
#'
#' @return TRUE if the file was written
save_figure = function(plot, file, width, height, units = "cm", dpi = 400) {

  if (!is_plot(plot)) {
    return(invisible(FALSE))
  }

  ok = tryCatch({
    ggplot2::ggsave(file, plot = plot, width = width, height = height,
                    units = units, dpi = dpi)
    TRUE
  }, error = function(e) {
    message("Could not save ", basename(file), ": ", conditionMessage(e))
    FALSE
  })

  invisible(ok)

}

#' Write a table, ignoring empty objects
save_table = function(x, file) {

  if (!is.data.frame(x) || nrow(x) == 0) {
    return(invisible(FALSE))
  }

  ok = tryCatch({
    utils::write.csv(x, file, row.names = FALSE)
    TRUE
  }, error = function(e) {
    message("Could not save ", basename(file), ": ", conditionMessage(e))
    FALSE
  })

  invisible(ok)

}

#' Write all the extra figures and tables
write_extra_outputs = function(state, settings) {

  dir = file.path(settings$output_path, "extra-outputs")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  f = function(name) file.path(dir, name)

  PL = state$PL_models

  if (isTRUE(state$trial_map$geoTRUE)) {
    save_figure(state$trial_map$map, f("trial-map.pdf"), 18, 18)
  }

  for (m in seq_along(PL$logworth_plot)) {
    save_figure(PL$logworth_plot[[m]], f(paste0(m, "-logworth.pdf")), 21, 15)
  }

  save_figure(PL$kendall$kendall_plot, f("kendall-tau.pdf"), 15, 18)
  save_table(PL$kendall$kendall, f("kendall-tau.csv"))

  if (isTRUE(state$participation_map$available)) {
    save_figure(state$participation_map$map, f("participation-map.pdf"), 18, 18)
  }

  if (isTRUE(state$data_value$available)) {
    save_figure(state$data_value$plot, f("data-value.pdf"), 20, 13)
    save_table(state$data_value$table, f("data-value.csv"))
  }

  save_figure(PL$worthmap, f("worth-map.pdf"), 25, 25)

  for (r in seq_along(PL$reliability_plot)) {
    save_figure(PL$reliability_plot[[r]], f(paste0("reliability", r, ".pdf")), 25, 25)
  }

  save_table(PL$reliability_data, f("reliability-data.csv"))
  save_table(PL$netwinning, f("netwinning-data.csv"))

  if (isTRUE(state$PL_tree$isTREE)) {
    save_figure(state$PL_tree$PLtree_plot, f("plackettluce-tree.pdf"), 18, 25)
  }

  if (isTRUE(state$agroclimate$agroclimate)) {
    save_table(state$agroclimate$rainfall_weekly, f("weekly-precipitation-indices.csv"))
    save_table(state$agroclimate$temperature_weekly, f("weekly-temperature-indices.csv"))
    save_figure(state$agroclimate$rain_plot, f("weekly-precipitation-indices.pdf"), 20, 20)
    save_figure(state$agroclimate$temperature_plot, f("weekly-temperature-indices.pdf"), 20, 20)
  }

  if (isTRUE(state$quanti_dat$quantitative)) {
    save_table(state$quanti_dat$outliers, f("possible-outliers-quantitative-data.csv"))
  }

  # monitoring
  mon = state$monitoring
  save_table(mon$attrition, f("monitoring-attrition.csv"))
  save_table(mon$answer_quality, f("monitoring-answer-quality.csv"))
  save_table(mon$enumerators, f("monitoring-enumerators.csv"))
  save_figure(mon$attrition_plot, f("monitoring-attrition.pdf"), 20, 14)

  # evidence for variety release
  save_table(state$release$summary, f("variety-release-summary.csv"))
  save_table(state$release$by_trait_all_checks, f("variety-release-by-trait.csv"))

  # groups
  ga = state$group_analysis
  if (isTRUE(ga$requested)) {
    save_table(ga$eligibility, f("groups-eligibility.csv"))
    save_table(ga$heterogeneity, f("groups-test-by-trait.csv"))
    save_table(ga$balanced_ranking, f("groups-balanced-ranking.csv"))
    save_table(ga$within, f("groups-agreement-within.csv"))
    save_table(ga$between, f("groups-agreement-between.csv"))
    save_figure(ga$worthmap, f("groups-worth-map.pdf"), 20, 20)
  }

  # measured traits
  mt = state$metric
  if (isTRUE(mt$available)) {
    means = do.call("rbind", lapply(mt$traits, function(x) {
      if (nrow(x$means) == 0) return(NULL)
      cbind(trait = x$trait, moment = x$moment, method = x$method, x$means)
    }))
    save_table(means, f("measured-traits-means.csv"))
    vc = do.call("rbind", lapply(mt$traits, function(x) {
      if (nrow(x$variance) == 0) return(NULL)
      cbind(trait = x$trait, moment = x$moment, x$variance)
    }))
    save_table(vc, f("measured-traits-heritability.csv"))
    qc = do.call("rbind", lapply(mt$traits, function(x) cbind(trait = x$trait, moment = x$moment, x$qc)))
    save_table(qc, f("measured-traits-quality.csv"))
    for (k in seq_along(mt$traits)) {
      save_figure(mt$traits[[k]]$plots$box, f(paste0("measured-", safe_file_name(names(mt$traits)[k]), "-boxplot.pdf")), 20, 16)
    }
  }

  invisible(list.files(dir))

}
