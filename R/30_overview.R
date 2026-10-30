# ................................................................
# Trial overview, the tables and charts of the first sections
# of the report
# ................................................................

#' Produce the trial summary tables and overview charts
#'
#' @param cmdata the (cleaned) trial data as data.frame
#' @param rank_dat output of organize_ranking_data()
get_overview_summaries = function(cmdata, rank_dat) {

  list(partipation_plot = participation_plot(cmdata, rank_dat),
       summary_table_trait = trait_summary_table(rank_dat),
       summary_table_tech = item_summary_table(cmdata, rank_dat),
       trial_connectivity = trial_network(cmdata, rank_dat))

}

#' Table 1, traits and number of valid answers
trait_summary_table = function(rank_dat) {

  tbl = do.call("rbind", lapply(rank_dat$trait_list, function(x) {
    data.frame(name = x$name,
               collect = title_case(x$assessment),
               quest = x$question,
               n = sum(x$keep))
  }))

  row.names(tbl) = seq_len(nrow(tbl))

  names(tbl) = c("Trait", "Data collection moment",
                 "Question asked", "Number of valid answers")

  tbl

}

#' Table 2, how often each item was tested
#'
#' Splitting this table by groups is possible but currently switched
#' off, as in previous versions (group = NULL).
item_summary_table = function(cmdata, rank_dat, group = NULL) {

  option = rank_dat$option
  technologies_index = rank_dat$technologies_index

  itemtable = data.frame(table(unlist(cmdata[, technologies_index])))

  itemtable$x = paste0(round(itemtable$Freq / rank_dat$nranker * 100, 1), "%")

  names(itemtable) = c(title_case(option), "Freq", "Relative freq")

  if (length(group) > 0) {

    x = unlist(cmdata[, technologies_index])
    g = rep(group, length(technologies_index))
    ngroups = table(group)

    grouptbl = sapply(names(ngroups), function(gi) {
      tapply(g, x, function(y) sum(y == gi, na.rm = TRUE))
    })

    grouptbl = as.data.frame(grouptbl)
    names(grouptbl) = paste0(names(ngroups), " (n=", ngroups, ")")

    itemtable = cbind(itemtable, grouptbl)

  }

  itemtable$Abbreviation = reduce(as.character(itemtable[, title_case(option)]),
                                  nchars = 12,
                                  minlength = 12)

  itemtable = itemtable[union(c(title_case(option), "Abbreviation"), names(itemtable))]

  rownames(itemtable) = seq_len(nrow(itemtable))

  itemtable

}

#' Figure 1, rate of response in each data collection moment
participation_plot = function(cmdata, rank_dat, group = NULL) {

  trait_list = rank_dat$trait_list
  regsname = "Trial-package Delivery"

  participation = data.frame(n = nrow(cmdata),
                             n_tot = nrow(cmdata),
                             group = "Whole group",
                             dc = regsname)

  for (tr in trait_list) {
    participation = rbind(participation,
                          data.frame(n = sum(tr$keep),
                                     n_tot = nrow(cmdata),
                                     group = "Whole group",
                                     dc = tr$assessment))
  }

  if (length(group) > 0) {

    ngroups = table(group)

    participation2 = data.frame(n = as.vector(ngroups),
                                n_tot = as.vector(ngroups),
                                group = names(ngroups),
                                dc = regsname)

    for (tr in trait_list) {
      n = table(group[tr$keep])
      p = data.frame(group = names(ngroups), dc = tr$assessment)
      part = data.frame(n = as.vector(n), n_tot = as.vector(ngroups[names(n)]), group = names(n))
      participation2 = rbind(participation2, merge(p, part, by = "group", all.x = TRUE))
    }

    participation = rbind(participation, participation2)

  }

  # the largest number of answers in each data collection moment
  participation = split(participation, paste0(participation$dc, participation$group))
  participation = do.call("rbind", lapply(participation, function(x) x[which.max(x$n), ]))

  participation$value_perc = participation$n / participation$n_tot
  participation$dc = title_case(participation$dc)
  participation$dc = factor(participation$dc,
                            levels = unique(c(regsname, title_case(rank_dat$assessment_order))))
  participation$group = factor(participation$group, levels = c("Whole group", unique(group)))

  legendshow = length(levels(participation$group)) > 1

  if (!legendshow) {
    participation$group = ""
  }

  ggplot(participation, aes(x = dc, y = value_perc, group = group, color = group)) +
    geom_line(linewidth = 1.5, show.legend = legendshow) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_color_brewer(palette = "BrBG", name = "") +
    theme_bw() +
    theme(panel.background = element_blank(),
          panel.grid = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top",
          legend.text = element_text(size = 14, color = "grey20"),
          axis.text = element_text(size = 14, color = "grey20"),
          axis.title = element_text(size = 14, color = "grey20")) +
    labs(x = "Trial stage", y = "Rate of response")

}

#' Figure 5, network of items compared in the trial
trial_network = function(cmdata, rank_dat) {

  ref = rank_dat$trait_list[[rank_dat$reference_trait_index]]

  G = rankTricot(cmdata[ref$keep, c(rank_dat$technologies_index, ref$strings)],
                 items = rank_dat$technologies_index,
                 input = ref$strings,
                 group = TRUE)

  network(G)

}

#' Placeholder used when the overview could not be produced
error_data_overview_and_summaries = list(partipation_plot = NULL,
                                         summary_table_trait = data.frame(),
                                         summary_table_tech = data.frame(),
                                         trial_connectivity = NULL)
