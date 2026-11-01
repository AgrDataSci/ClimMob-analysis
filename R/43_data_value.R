# ................................................................
# Value of more answers
#
# Shows participants and teams why each answer counts. The fitted
# Plackett-Luce model of the reference trait is taken as the truth,
# tricot trials with n participants are simulated from it (each
# participant gets three items at random), the model is refitted, and
# the recovered order is compared with the truth. The result is the
# expected accuracy of the ranking for the number of participants who
# answered, and for more or fewer of them.
# ................................................................

#' Simulate tricot rankings from known log-worths
#'
#' Each participant gets k items at random and ranks them following the
#' Plackett-Luce model, one choice at a time.
#'
#' @param logworth named numeric vector, the log-worth of every item
#' @param n integer, number of participants
#' @param k integer, items per participant
#' @return a PlackettLuce rankings object with one ranking per participant
#' @examples
#' R = simulate_tricot_rankings(c(a = 0, b = 1, c = -1, d = 0.5), n = 20)
simulate_tricot_rankings = function(logworth, n, k = 3) {

  items = names(logworth)
  p = length(items)

  if (is.null(items) || p < k) {
    stop("simulate_tricot_rankings() needs a named vector with at least ", k, " items")
  }

  m = matrix(0L, nrow = n, ncol = p, dimnames = list(NULL, items))

  for (r in seq_len(n)) {
    s = sample.int(p, k)
    o = integer(0)
    while (length(s) > 1) {
      j = sample.int(length(s), 1, prob = exp(logworth[s]))
      o = c(o, s[j])
      s = s[-j]
    }
    o = c(o, s)
    m[r, o] = seq_len(k)
  }

  PlackettLuce::as.rankings(m)

}

#' Numbers of participants to simulate
#'
#' Half, the same, twice and three times the participants who answered,
#' plus all registered participants.
#'
#' @param n_now participants with a valid answer for the reference trait
#' @param n_registered participants registered in the trial
#' @return a sorted integer vector, 10 or more
#' @examples
#' data_value_sizes(35, 93)
data_value_sizes = function(n_now, n_registered = NA) {

  s = round(c(0.5, 1, 2, 3) * n_now)

  if (!is.na(n_registered) && n_registered > 0) {
    s = c(s, n_registered)
  }

  s = sort(unique(as.integer(s[s >= 10])))

  if (length(s) == 0) {
    stop("Too few answers to simulate the value of more answers")
  }

  s

}

#' Expected accuracy of the ranking by number of participants
#'
#' @param model a PlackettLuce model, usually the reference trait
#' @param n_now participants with a valid answer for that trait
#' @param n_registered participants registered in the trial
#' @param sizes numbers of participants to simulate, see data_value_sizes()
#' @param nsim simulations for each number of participants
#' @param seed random seed, so the same data always gives the same report
#' @return a list with the table, the plot and the values for the text
#' @examples
#' # m = PlackettLuce::PlackettLuce(R)
#' # get_data_value(m, n_now = 35, n_registered = 93, nsim = 20)
get_data_value = function(model, n_now, n_registered = NA, sizes = NULL,
                          nsim = 50, seed = 1L) {

  truth = stats::coef(model, log = TRUE)
  truth = truth[!grepl("^tie", names(truth))]

  if (length(truth) < 4) {
    stop("At least four items are needed to simulate the value of more answers")
  }

  if (is.null(sizes)) {
    sizes = data_value_sizes(n_now, n_registered)
  }

  best = names(truth)[which.max(truth)]

  one_size = function(n) {

    r = vapply(seq_len(nsim), function(b) {
      R = simulate_tricot_rankings(truth, n)
      est = stats::coef(PlackettLuce::PlackettLuce(R), log = TRUE)
      est = est[names(truth)]
      c(tau = stats::cor(est, truth, method = "kendall"),
        winner = as.numeric(names(est)[which.max(est)] == best))
    }, numeric(2))

    data.frame(n = n,
               tau = round(mean(r["tau", ]), 3),
               tau_lower = round(unname(stats::quantile(r["tau", ], 0.10)), 3),
               tau_upper = round(unname(stats::quantile(r["tau", ], 0.90)), 3),
               p_winner = round(mean(r["winner", ]), 3),
               stringsAsFactors = FALSE)

  }

  tab = with_seed(seed, do.call("rbind", lapply(sizes, one_size)))

  at = function(n) {
    if (is.na(n) || !n %in% tab$n) return(NA_real_)
    tab$p_winner[tab$n == n]
  }

  list(available = TRUE,
       table = tab,
       plot = data_value_plot(tab, n_now, n_registered),
       n_now = n_now,
       n_registered = n_registered,
       p_now = at(n_now),
       p_registered = at(n_registered),
       nsim = nsim)

}

#' Chart of the expected accuracy by number of participants
data_value_plot = function(tab, n_now, n_registered = NA) {

  d = rbind(data.frame(n = tab$n, value = tab$tau, what = tr("lab_tau_truth")),
            data.frame(n = tab$n, value = tab$p_winner, what = tr("lab_p_winner")))
  d$what = factor(d$what, levels = unique(d$what))

  marks = data.frame(n = c(n_now, n_registered),
                     label = c(tr("lab_answered_now"), tr("lab_all_registered")))
  marks = marks[!is.na(marks$n), , drop = FALSE]

  ggplot(d, aes(x = n, y = value)) +
    geom_ribbon(data = tab, aes(x = n, ymin = tau_lower, ymax = tau_upper),
                inherit.aes = FALSE, fill = "#1E4532", alpha = 0.15) +
    geom_vline(data = marks, aes(xintercept = n), linetype = "dotted", colour = "grey40") +
    geom_text(data = marks, aes(x = n, y = 0.02, label = label), angle = 90,
              hjust = 0, vjust = -0.4, size = 3.5, colour = "grey30", inherit.aes = FALSE) +
    geom_line(aes(colour = what), linewidth = 1.2) +
    geom_point(aes(colour = what, shape = what), size = 2.5) +
    scale_colour_manual(values = c("#1E4532", "#D9952B"), name = "") +
    scale_shape_manual(values = c(16, 15), name = "") +
    scale_y_continuous(limits = c(0, 1)) +
    theme_classic() +
    theme(legend.position = "bottom",
          text = element_text(size = 13, colour = "grey20")) +
    labs(x = tr("lab_n_answering"), y = tr("lab_share_correlation"))

}

#' Placeholder used when the simulation fails
error_data_data_value = list(available = FALSE, table = data.frame(), plot = NULL,
                             n_now = NA, n_registered = NA, p_now = NA,
                             p_registered = NA, nsim = 0)
