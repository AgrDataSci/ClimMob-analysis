# Basic package-interface smoke test. NOT EXECUTED by author.
stopifnot(requireNamespace("gosset", quietly = TRUE))
data("nicabean", package = "gosset")
x = gosset::nicabean$trial
x = x[x$trait == "Vigor", , drop = FALSE]
stopifnot(!anyNA(x[, c("id", "item", "rank")]))
stopifnot(!anyDuplicated(x[, c("id", "item")]))
r = gosset::rank_tricot2(x, id = "id", items = "item", input = "rank")
stopifnot(inherits(r, "rankings"), nrow(r) > 0L)
