# Package-first tricot ranking example
# STATUS: NOT EXECUTED. This file is a candidate for R-based CI validation.
# Scientific question: estimate relative variety worth from tricot rankings.
# Provenance: gosset/R/rank_tricot2.R documented nicabean example.
# Run only with installed gosset and PlackettLuce; retain sessionInfo().
# Assignment convention: use = rather than <-.

stopifnot(requireNamespace("gosset", quietly = TRUE))
stopifnot(requireNamespace("PlackettLuce", quietly = TRUE))

data("nicabean", package = "gosset")
trial = gosset::nicabean$trial
stopifnot(all(c("id", "item", "rank", "trait") %in% names(trial)))
trial = trial[trial$trait == "Vigor", , drop = FALSE]
stopifnot(nrow(trial) > 0L)

# Inspect source data before parsing. The current rank_tricot2 source warns
# on missing fields and duplicate id/item pairs but does NOT comprehensively
# validate tricot experimental design or the meaning of rank values.
stopifnot(!anyNA(trial[, c("id", "item", "rank")]))
stopifnot(!anyDuplicated(trial[, c("id", "item")]))
rankings = gosset::rank_tricot2(
  trial, id = "id", items = "item", input = "rank"
)
stopifnot(inherits(rankings, "rankings"), nrow(rankings) > 0L)

# Pseudo-rankings are not observations. Their use and strength must be
# justified, and sensitivity assessed before publication.
fit = PlackettLuce::PlackettLuce(rankings, npseudo = 0.5)
print(summary(fit))
print(stats::coef(fit))
print(sessionInfo())

# NOT CLAIMED: checked finite estimates, calibrated confidence intervals,
# network connectivity, out-of-sample performance or variety-release evidence.
