# ................................................................
# Trial monitoring and data quality
#
# These summaries use the raw data and the request only, so they are
# available while the trial is running, before any trait has enough
# answers for the ranking analysis.
# ................................................................

#' Classify each tricot answer
#'
#' @param x a data.frame with the two answer columns of one trait (best, worst)
#' @return a character vector, one value per row, with
#'  "valid", "missing", "not_observed", "tie", "same_best_worst" or "invalid"
#' @examples
#' x = data.frame(best = c("A", NA, "Not observed", "Tie", "B", "A"),
#'                worst = c("B", NA, "Not observed", "Tie", "B", "D"))
#' answer_status(x)
answer_status = function(x) {

  b = as.character(x[[1]])
  w = as.character(x[[2]])

  status = rep("valid", length(b))

  abc = LETTERS[1:3]

  status[!(b %in% abc & w %in% abc)] = "invalid"
  status[b %in% "Tie" | w %in% "Tie"] = "tie"
  status[b %in% "Not observed" | w %in% "Not observed"] = "not_observed"
  status[b %in% abc & w %in% abc & b == w] = "same_best_worst"
  status[is.na(b) & is.na(w)] = "missing"

  status

}

#' The data collection moments of the project
#'
#' @param pars the decoded parameters
#' @param cmdata the trial data
#' @return a data.frame with id, name, day and the column prefix
#'  used in the data (REG_ for registration, ASS<id>_ for the others)
collection_moments = function(pars, cmdata = NULL) {

  blocks = list(pars$traits, pars$linear, pars$covariates)
  blocks = Filter(function(x) is.data.frame(x) && nrow(x) > 0, blocks)

  m = do.call("rbind", lapply(blocks, function(b) {
    data.frame(id = as.character(b$assessmentId),
               name = as.character(b$assessmentName),
               day = if ("assessmentDay" %in% names(b)) suppressWarnings(as.numeric(b$assessmentDay)) else NA_real_,
               stringsAsFactors = FALSE)
  }))

  reg = data.frame(id = "registration", name = "Registration", day = 0, stringsAsFactors = FALSE)

  if (is.null(m)) {
    m = reg
  } else {
    m = m[!grepl("^0+$", m$id), , drop = FALSE]
    m = m[!duplicated(m$id), , drop = FALSE]
    m = rbind(reg, m)
  }

  # ClimMob codes are 12 hexadecimal characters (columns ASS<code>_...),
  # data combined with combine_projects() uses the moment name (<moment>_...)
  m$prefix = ifelse(m$id == "registration", "REG_",
                    ifelse(grepl("^[0-9a-f]{12}$", m$id), paste0("ASS", m$id, "_"), paste0(m$id, "_")))

  m = m[order(m$day, na.last = TRUE), , drop = FALSE]
  rownames(m) = NULL

  m

}

#' Which participants answered each data collection moment
#'
#' A participant answered a moment when the submission date of that
#' moment is filled. When the data has no submission date (for example
#' after combining projects), any answer in the columns of that moment counts.
#'
#' @return a logical matrix, participants in rows and moments in columns
moment_responses = function(cmdata, moments) {

  out = sapply(seq_len(nrow(moments)), function(i) {

    pre = moments$prefix[i]

    date_col = paste0(pre, "_submitted_date")

    if (date_col %in% names(cmdata)) {
      return(!is.na(cmdata[[date_col]]) & nzchar(as.character(cmdata[[date_col]])))
    }

    cols = names(cmdata)[startsWith(names(cmdata), pre)]

    if (length(cols) == 0) {
      return(rep(NA, nrow(cmdata)))
    }

    rowSums(!is.na(cmdata[cols])) > 0

  })

  out = matrix(out, nrow = nrow(cmdata))
  colnames(out) = moments$name

  out

}

#' Participation and attrition by data collection moment (issue #79)
#'
#' @param responses output of moment_responses()
#' @param moments output of collection_moments()
#' @param group optional grouping vector, one value per participant
attrition_table = function(responses, moments, group = NULL) {

  one = function(r, label) {
    n = colSums(r, na.rm = TRUE)
    n_reg = n[1]
    data.frame(group = label,
               moment = moments$name,
               day = moments$day,
               n = as.integer(n),
               percent_of_registered = if (n_reg > 0) round(100 * n / n_reg, 1) else NA_real_,
               lost_since_previous = c(NA, -diff(as.integer(n))),
               stringsAsFactors = FALSE)
  }

  out = one(responses, "all")

  if (length(group) == nrow(responses)) {
    for (g in sort(unique(group))) {
      out = rbind(out, one(responses[group == g, , drop = FALSE], g))
    }
  }

  out$moment[!is.na(out$moment) & out$moment == "Registration"] = tr("registration")

  rownames(out) = NULL

  out

}

#' Quality of the tricot answers of every trait, including dropped traits
answer_quality_table = function(cmdata, pars) {

  traits = pars$traits

  if (!is.data.frame(traits) || nrow(traits) == 0) {
    return(data.frame())
  }

  nq = traits$nQst[1]

  rows = lapply(seq_len(nrow(traits)), function(i) {

    cols = find_columns(as.character(traits[i, paste0("nameString", seq_len(nq))]), names(cmdata))

    if (any(is.na(cols))) {
      st = rep("missing", nrow(cmdata))
    } else {
      st = answer_status(cmdata[cols])
    }

    st = factor(st, levels = c("valid", "missing", "not_observed", "tie",
                               "same_best_worst", "invalid"))

    tb = table(st)

    data.frame(trait = traits$name[i],
               moment = traits$assessmentName[i],
               valid = tb[["valid"]],
               not_answered = tb[["missing"]],
               not_observed = tb[["not_observed"]],
               tie = tb[["tie"]],
               same_best_worst = tb[["same_best_worst"]],
               invalid = tb[["invalid"]],
               stringsAsFactors = FALSE)

  })

  do.call("rbind", rows)

}

#' Number of submissions by enumerator in each data collection moment
enumerator_table = function(cmdata, moments) {

  rows = lapply(seq_len(nrow(moments)), function(i) {
    col = paste0(moments$prefix[i], "_submitted_by")
    if (!col %in% names(cmdata)) return(NULL)
    by = as.character(cmdata[[col]])
    by = by[!is.na(by) & nzchar(by)]
    if (length(by) == 0) return(NULL)
    tb = table(by)
    data.frame(moment = moments$name[i],
               enumerator = names(tb),
               submissions = as.integer(tb),
               stringsAsFactors = FALSE)
  })

  out = do.call("rbind", rows)

  if (is.null(out)) data.frame() else out

}

#' All the monitoring outputs
#'
#' @param cmdata the trial data (raw, before cleaning)
#' @param pars the decoded parameters
#' @param group optional grouping vector
get_monitoring = function(cmdata, pars, group = NULL) {

  moments = collection_moments(pars, cmdata)
  responses = moment_responses(cmdata, moments)

  attrition = attrition_table(responses, moments, group)
  quality = answer_quality_table(cmdata, pars)

  list(moments = moments,
       registered = sum(responses[, 1], na.rm = TRUE),
       packages = nrow(cmdata),
       attrition = attrition,
       answer_quality = quality,
       enumerators = enumerator_table(cmdata, moments),
       attrition_plot = attrition_plot(attrition))

}

#' Chart of the share of participants who answered each moment
attrition_plot = function(attrition) {

  if (!requireNamespace("ggplot2", quietly = TRUE) || nrow(attrition) == 0) {
    return(NULL)
  }

  d = attrition
  d$moment = factor(d$moment, levels = unique(d$moment))
  d$group = ifelse(d$group == "all", tr("whole_group"), d$group)
  d$group = factor(d$group, levels = unique(d$group))

  show_legend = length(levels(d$group)) > 1

  ggplot2::ggplot(d, ggplot2::aes(x = moment, y = percent_of_registered,
                                  group = group, color = group)) +
    ggplot2::geom_line(linewidth = 1.2, show.legend = show_legend) +
    ggplot2::geom_point(size = 2.5, show.legend = show_legend) +
    ggplot2::scale_y_continuous(limits = c(0, 100)) +
    ggplot2::scale_color_brewer(palette = "Dark2", name = "") +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   legend.position = "top",
                   text = ggplot2::element_text(size = 13, color = "grey20")) +
    ggplot2::labs(x = tr("lab_trial_stage"), y = tr("lab_percent_answered"))

}

#' Placeholder used when the monitoring step fails
error_data_monitoring = list(moments = data.frame(), registered = NA, packages = NA,
                             attrition = data.frame(), answer_quality = data.frame(),
                             enumerators = data.frame(), attrition_plot = NULL)
