# ................................................................
# Trial overview, the tables and charts of the first sections
# of the report
# ................................................................

#' Produce the trial summary tables and overview charts
#'
#' @param cmdata the (cleaned) trial data as data.frame
#' @param rank_dat output of organize_ranking_data()
get_overview_summaries = function(cmdata, rank_dat) {

  list(summary_table_trait = trait_summary_table(rank_dat),
       summary_table_tech = item_summary_table(cmdata, rank_dat),
       trial_connectivity = trial_network(cmdata, rank_dat))

}

#' Traits and number of valid answers
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

#' How often each item was tested
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

#' Network of items compared in the trial
trial_network = function(cmdata, rank_dat) {

  ref = rank_dat$trait_list[[rank_dat$reference_trait_index]]

  G = rankTricot(cmdata[ref$keep, c(rank_dat$technologies_index, ref$strings)],
                 items = rank_dat$technologies_index,
                 input = ref$strings,
                 group = TRUE)

  network(G)

}

#' Placeholder used when the overview could not be produced
error_data_overview_and_summaries = list(summary_table_trait = data.frame(),
                                         summary_table_tech = data.frame(),
                                         trial_connectivity = NULL)
