# ................................................................
# Data for the individual participant reports (info sheets)
# ................................................................

#' Read the multilanguage text of the participant reports
#'
#' @return a named character vector, names are the "fields" column
read_participant_text = function(file, language) {

  txt = utils::read.csv(file, stringsAsFactors = FALSE, encoding = "UTF-8")

  lang = if (language %in% names(txt)) language else names(txt)[2]

  stats::setNames(txt[[lang]], txt[[1]])

}

#' Summarise the results for the participant reports
#'
#' @param cmdata the (cleaned) trial data
#' @param rank_dat output of organize_ranking_data()
#' @param text_file the csv with the multilanguage text
#' @param language the language code, falls back to the first language
get_participant_report = function(cmdata, rank_dat, text_file, language = "en") {

  itemnames = rank_dat$technologies_index
  trait_list = rank_dat$trait_list
  reference_trait = rank_dat$reference_trait_index
  other_traits_list = trait_list[-reference_trait]
  nranker = rank_dat$nranker
  items = rank_dat$technologies
  nitems = length(items)
  ncomp = length(itemnames)
  nothertraits = length(trait_list) - 1
  reference = rank_dat$reference_tech

  overall = trait_list[[reference_trait]]
  keep = overall$keep

  # text in the chosen language, with the fields filled in
  reporttext = read_participant_text(text_file, language)

  notreplied = reporttext[["notreplied"]]

  fill = c("r rankers" = reporttext[["partictag2"]],
           "r nranker" = nranker,
           "r question_asked" = overall$question,
           "r nitems" = nitems,
           "r options" = reporttext[["techtag2"]],
           "r nothertraits" = nothertraits,
           "r ncomp" = ncomp)

  for (f in names(fill)) {
    reporttext = gsub(f, fill[[f]], reporttext, fixed = TRUE)
  }

  # overall ranking of the reference trait
  R = rankTricot(cmdata[keep, ], items = itemnames, input = overall$strings,
                 full.output = TRUE)

  mod_overall = PlackettLuce(R[["PLranking"]])

  order_items = coef(mod_overall, ref = reference, log = FALSE)
  order_items = order_items[names(order_items) %in% items]

  rank_items = gosset:::.rank_decimal(order_items)$rank
  order_items = names(order_items)

  ordering = R[["myrank"]]
  freq_items = table(unlist(cmdata[itemnames]))
  first_items = table(ordering[, 1])
  last_items = table(ordering[, ncol(ordering)])

  infotable = data.frame(item = order_items,
                         rank = rank_items,
                         freq = as.vector(freq_items[order_items]),
                         first = as.vector(first_items[order_items]),
                         last = as.vector(last_items[order_items]))

  infotable[is.na(infotable)] = 0
  infotable = infotable[order(infotable$rank), ]

  # one row per participant
  names(cmdata)[names(cmdata) == "package_farmername"] = "package_participant_name"

  sel = c("id", "package_participant_name", itemnames)
  sel = sel[sel %in% names(cmdata)]

  partitable = cmdata[, sel]
  names(partitable) = gsub("package_|farmer|participant_", "", names(partitable))

  positions = paste0("Position", seq_len(ncomp))

  pos = matrix(NA, ncol = ncomp, nrow = nrow(cmdata),
               dimnames = list(seq_len(nrow(cmdata)), positions))

  pos[keep, ] = ordering

  partitable = cbind(partitable, pos)
  partitable$Position1[is.na(partitable$Position1)] = notreplied
  partitable[is.na(partitable)] = ""

  # the other traits, one chart per participant
  otrp = list()

  if (nothertraits > 0) {

    otrnames = vapply(other_traits_list, function(x) paste0(x$name, " [", x$assessment, "]"),
                      character(1), USE.NAMES = FALSE)

    otr = lapply(other_traits_list, function(ot) {
      r = rankTricot(cmdata[ot$keep, ], items = itemnames, input = ot$strings,
                     full.output = TRUE)[["myrank"]]
      rexp = matrix(NA, nrow = nrow(partitable), ncol = ncomp,
                    dimnames = list(partitable$id, positions))
      rexp[ot$keep, ] = r
      rexp
    })

    for (i in seq_len(nrow(partitable))) {

      x = do.call("rbind", lapply(otr, function(m) m[i, ]))

      x = data.frame(Trait = rep(otrnames, ncomp),
                     Tech = as.vector(x),
                     Rank = rep(seq_len(ncomp), each = length(otrnames)))

      x = x[!is.na(x$Tech) & x$Tech != "" & x$Tech != notreplied, ]

      if (nrow(x) > 1) {
        x$Rank = factor(x$Rank)
        x$Trait = factor(x$Trait, levels = rev(otrnames))
        otrp[[i]] = ggplot(x, aes(x = Tech, y = Trait, fill = Rank)) +
          geom_tile() +
          scale_fill_manual(values = c("#FFD700", "#C0C0C0", "#cd7f32")) +
          theme_minimal() +
          labs(x = "", y = "", fill = "Rank")
      } else {
        otrp[i] = list(NULL)
      }

    }

  }

  # overall evaluation by all participants
  pover = sort(coef(mod_overall, log = FALSE))
  pover = data.frame(items = factor(names(pover), levels = names(pover)),
                     pw = as.vector(pover))

  poverp = ggplot(pover, aes(x = pw, y = items, fill = pw)) +
    geom_bar(stat = "identity", position = "dodge", show.legend = FALSE) +
    labs(x = "", y = "") +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 5),
          panel.background = element_blank())

  # a podium template, the items of each participant are added in the report
  podium = data.frame(label = factor(c("1st", "2nd", "3rd"), levels = c("2nd", "1st", "3rd")),
                      values = 3:1)

  ggpodium = ggplot(data = podium, aes(y = values, x = label, fill = label)) +
    geom_bar(stat = "identity", position = "dodge", show.legend = FALSE) +
    labs(x = "", y = "") +
    scale_fill_manual(values = c("#C0C0C0", "#FFD700", "#cd7f32")) +
    theme(axis.text.y = element_blank(),
          axis.text.x = element_text(size = 10),
          panel.background = element_blank())

  list(podium = ggpodium,
       poverp = poverp,
       partitable = partitable,
       reporttext = reporttext,
       other_traits_table = otrp,
       infotable = infotable)

}

#' Placeholder used when the participant reports could not be prepared
error_participant_report = list(podium = NULL,
                                poverp = NULL,
                                partitable = data.frame(),
                                reporttext = character(),
                                other_traits_table = list(),
                                infotable = data.frame())
