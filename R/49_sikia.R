# ................................................................
# Free-text answers analysed by Sikia
#
# ClimMob can send the results of Sikia Analytics in the data file,
# under "SIKIA_results". Sikia reads the open answers of participants
# (text or transcribed voice) and returns three outputs:
#   summary     counts of mentions by label (trait, pest, quality ...)
#               with their sentiment, and a variety x label matrix
#   table       one row per mention, with the words that produced it
#   topic_tree  categories and topics that group similar mentions
# This step turns them into a chart of what participants talked about
# and how they felt about it, a chart by variety when Sikia linked
# mentions to varieties, a table of topics and a few quotes.
# Nothing is modelled here. Sikia's labels and sentiments are reported
# as they come, with their counts, so readers can weigh them.
# ................................................................

#' Find the Sikia results in the ClimMob data
#'
#' @param cmdatajson the CM_list sent by ClimMob
#' @return a list with summary, table and topic_tree, or NULL
#' @examples
#' x = jsonlite::fromJSON("data.json")
#' sikia_payload(x)
sikia_payload = function(cmdatajson) {

  s = tryCatch(cmdatajson$SIKIA_results, error = function(e) NULL)
  if (is.null(s) || length(s) == 0) return(NULL)

  # fromJSON may simplify the list of results into a data.frame
  if (is.data.frame(s)) {
    s = lapply(seq_len(nrow(s)), function(i) list(model_name = s$model_name[i],
                                                 output_type = s$output_type[i],
                                                 json_result = s$json_result[[i]]))
  }

  out = list()
  for (item in s) {
    type = item$output_type %||% ""
    if (type %in% c("summary", "table", "topic_tree")) out[[type]] = item$json_result
  }

  if (length(out) == 0) NULL else out

}

#' Labels mentioned by participants, with sentiment
#'
#' @param summary the Sikia summary object
#' @param min_mentions keep labels with at least this many mentions
#' @param top keep at most this many labels, the most mentioned
#' @return a data.frame with label, mentions, positive, neutral, negative
#'  and net (share positive minus share negative, from -1 to 1)
sikia_labels = function(summary, min_mentions = 1, top = 15) {

  lab = summary$labels
  if (is.null(lab) || length(lab) == 0) return(data.frame())
  if (!is.data.frame(lab)) lab = do.call("rbind", lapply(lab, as.data.frame, stringsAsFactors = FALSE))

  d = data.frame(label = as.character(lab$canonical_label),
                 mentions = as.integer(lab$count),
                 positive = as.integer(lab$positive %||% 0),
                 neutral = as.integer(lab$neutral %||% 0),
                 negative = as.integer(lab$negative %||% 0),
                 stringsAsFactors = FALSE)
  d$net = round((d$positive - d$negative) / pmax(d$mentions, 1), 2)
  d = d[d$mentions >= min_mentions, , drop = FALSE]
  d = d[order(-d$mentions, -d$net), , drop = FALSE]
  d = utils::head(d, top)
  rownames(d) = NULL
  d

}

#' Mentions by variety and label
#'
#' @param summary the Sikia summary object
#' @return a long data.frame with variety, label, mentions and net
#'  sentiment, or an empty data.frame when Sikia did not link mentions
#'  to varieties
sikia_variety_labels = function(summary) {

  vx = summary$variety_x_label
  if (is.null(vx) || length(vx$varieties %||% character(0)) == 0) return(data.frame())

  m = vx$matrix
  if (is.list(m)) m = do.call("rbind", lapply(m, unlist))
  cnt = vx$counts
  if (is.list(cnt)) cnt = do.call("rbind", lapply(cnt, unlist))

  vars = unlist(vx$varieties); labs = unlist(vx$labels)
  d = expand.grid(variety = vars, label = labs, stringsAsFactors = FALSE)
  d$net = as.numeric(t(m))
  d$mentions = if (is.null(cnt)) NA_integer_ else as.integer(t(cnt))
  d = d[!is.na(d$mentions) & d$mentions > 0, , drop = FALSE]
  rownames(d) = NULL
  d

}

#' Topics found by Sikia
#'
#' @param tree the Sikia topic_tree object
#' @return a data.frame with category, topic, mentions, keywords and phrases
sikia_topics = function(tree) {

  cats = tree$categories
  if (is.null(cats) || length(cats) == 0) return(data.frame())
  if (is.data.frame(cats)) cats = lapply(seq_len(nrow(cats)), function(i) lapply(cats, function(col) col[[i]]))

  rows = list()
  for (ct in cats) {
    leaves = ct$leaves
    if (is.data.frame(leaves)) leaves = lapply(seq_len(nrow(leaves)), function(i) lapply(leaves, function(col) col[[i]]))
    for (lf in leaves %||% list()) {
      rows[[length(rows) + 1]] = data.frame(
        category = as.character(ct$label),
        topic = as.character(lf$label),
        mentions = as.integer(lf$member_count %||% NA),
        keywords = paste(unlist(lf$keywords), collapse = ", "),
        phrases = paste(utils::head(unlist(lf$representative_phrases), 3), collapse = " · "),
        stringsAsFactors = FALSE)
    }
  }

  d = do.call("rbind", rows)
  d = d[order(-d$mentions), , drop = FALSE]
  rownames(d) = NULL
  d

}

#' The mentions table, one row per observation
sikia_mentions = function(table) {

  if (is.null(table) || length(table) == 0) return(data.frame())
  m = if (is.data.frame(table)) table else do.call("rbind", lapply(table, function(r) as.data.frame(lapply(r, function(v) if (is.null(v)) NA else v), stringsAsFactors = FALSE)))
  keep = intersect(c("response_id", "target_entity_label", "entity_type", "canonical_label",
                     "phrase_en", "evidence_en", "extraction_type", "sentiment_polarity",
                     "sentiment_strength", "sentiment_confidence", "emotion_primary", "evidence_grounded"),
                   names(m))
  m = m[, keep, drop = FALSE]
  rownames(m) = NULL
  m

}

#' Quotes that illustrate the most mentioned labels
#'
#' For each of the top labels, the best grounded positive and negative
#' evidence, by sentiment confidence and strength.
#'
#' @param mentions output of sikia_mentions()
#' @param labels output of sikia_labels()
#' @param n_labels how many labels to quote
#' @return a data.frame with label, sentiment and quote
sikia_quotes = function(mentions, labels, n_labels = 5) {

  if (nrow(mentions) == 0 || nrow(labels) == 0) return(data.frame())
  if (!"evidence_en" %in% names(mentions)) return(data.frame())

  m = mentions
  if ("evidence_grounded" %in% names(m)) m = m[is.na(m$evidence_grounded) | as.logical(m$evidence_grounded), , drop = FALSE]
  m = m[!is.na(m$evidence_en) & nzchar(m$evidence_en), , drop = FALSE]
  score = as.numeric(m$sentiment_confidence %||% 0) + as.numeric(m$sentiment_strength %||% 0)
  m = m[order(-score), , drop = FALSE]

  rows = list()
  for (lb in utils::head(labels$label, n_labels)) {
    for (pol in c("positive", "negative")) {
      k = which(m$canonical_label == lb & m$sentiment_polarity == pol)
      if (length(k) > 0) {
        rows[[length(rows) + 1]] = data.frame(label = lb, sentiment = pol,
                                              quote = as.character(m$evidence_en[k[1]]),
                                              stringsAsFactors = FALSE)
      }
    }
  }

  if (length(rows) == 0) data.frame() else do.call("rbind", rows)

}

#' Diverging chart of mentions by label and sentiment
sikia_sentiment_plot = function(labels) {

  d = rbind(data.frame(label = labels$label, n = -labels$negative, sentiment = "negative"),
            data.frame(label = labels$label, n = labels$neutral, sentiment = "neutral"),
            data.frame(label = labels$label, n = labels$positive, sentiment = "positive"))
  d = d[d$n != 0, , drop = FALSE]
  d$label = factor(d$label, levels = rev(labels$label))
  d$sentiment = factor(d$sentiment, levels = c("negative", "neutral", "positive"),
                       labels = c(tr("lab_negative"), tr("lab_neutral"), tr("lab_positive")))
  tot = data.frame(label = factor(labels$label, levels = rev(labels$label)),
                   x = labels$positive + labels$neutral, n = labels$mentions)

  ggplot(d, aes(x = n, y = label, fill = sentiment)) +
    geom_col(width = 0.7) +
    geom_vline(xintercept = 0, colour = "grey30") +
    geom_text(data = tot, aes(x = x, y = label, label = n), inherit.aes = FALSE,
              hjust = -0.4, size = 3.3, colour = "grey30") +
    scale_fill_manual(values = c("#B2182B", "#BABABA", "#2166AC"), name = "")  # ColorBrewer RdBu, colour-blind safe +
    scale_x_continuous(labels = abs, expand = expansion(mult = c(0.08, 0.15))) +
    theme_classic() +
    theme(legend.position = "bottom", text = element_text(size = 13, colour = "grey20"),
          axis.title.y = element_blank()) +
    labs(x = tr("lab_mentions"))

}

#' Chart of net sentiment by variety and label
sikia_variety_plot = function(vl) {

  vl$label = factor(vl$label, levels = rev(unique(vl$label)))
  ggplot(vl, aes(x = variety, y = label, fill = net)) +
    geom_tile(colour = "white") +
    geom_text(aes(label = mentions), size = 3.3, colour = "grey15") +
    scale_fill_gradient2(low = "#B2182B", mid = "#F7F7F7", high = "#2166AC", midpoint = 0,  # ColorBrewer RdBu
                         limits = c(-1, 1), name = tr("lab_net_sentiment")) +
    theme_classic() +
    theme(legend.position = "bottom", text = element_text(size = 13, colour = "grey20"),
          axis.title = element_blank(), axis.text.x = element_text(angle = 30, hjust = 1))

}

#' Analyse the Sikia results sent with the ClimMob data
#'
#' @param cmdatajson the CM_list sent by ClimMob
#' @param top_labels labels to show in the chart
#' @param n_quotes labels to quote
#' @return a list with available, totals, labels, variety_labels, topics,
#'  mentions, quotes, sentiment_plot, variety_plot and built_at
#' @examples
#' x = jsonlite::fromJSON("tests/testdata/simulated-maize/data.json")
#' s = get_sikia(x)
#' s$labels
get_sikia = function(cmdatajson, top_labels = 15, n_quotes = 5) {

  p = sikia_payload(cmdatajson)
  if (is.null(p) || is.null(p$summary)) return(error_data_sikia)

  summary = p$summary
  labels = sikia_labels(summary, top = top_labels)
  if (nrow(labels) == 0) return(error_data_sikia)

  vl = sikia_variety_labels(summary)
  topics = if (!is.null(p$topic_tree)) sikia_topics(p$topic_tree) else data.frame()
  mentions = sikia_mentions(p$table)
  quotes = sikia_quotes(mentions, labels, n_quotes)

  tot = summary$totals %||% list()

  list(available = TRUE,
       totals = list(responses = as.integer(tot$responses %||% NA),
                     mentions = as.integer(tot$mentions %||% sum(labels$mentions)),
                     labels = as.integer(tot$labels %||% nrow(labels)),
                     varieties = as.integer(tot$varieties %||% 0)),
       labels = labels,
       variety_labels = vl,
       topics = topics,
       mentions = mentions,
       quotes = quotes,
       sentiment_plot = sikia_sentiment_plot(labels),
       variety_plot = if (nrow(vl) > 0) sikia_variety_plot(vl) else NULL,
       built_at = substr(as.character(summary$built_at %||% ""), 1, 10))

}

#' Placeholder used when there are no Sikia results
error_data_sikia = list(available = FALSE, totals = list(), labels = data.frame(),
                        variety_labels = data.frame(), topics = data.frame(),
                        mentions = data.frame(), quotes = data.frame(),
                        sentiment_plot = NULL, variety_plot = NULL, built_at = "")
