simulate_metric = function(seed = 3) {
  set.seed(seed)
  g = c(A = 0, B = 2, C = 4, D = 1)
  farms = 40
  d = do.call("rbind", lapply(seq_len(farms), function(f) {
    trio = sample(names(g), 3)
    data.frame(farm = f, genotype = trio, value = 10 + rnorm(1, 0, 2) + g[trio] + rnorm(3, 0, 1))
  }))
  d$value[1:3] = 0          # a failed plot recorded as zeros
  d$value[10] = 200          # a typing mistake
  d
}

test_that("IQR bounds and Holland heritability follow their formulas", {
  b = iqr_bounds(c(1, 2, 3, 4, 100))
  expect_equal(unname(b), c(2 - 1.5 * 2, 4 + 1.5 * 2))
  expect_equal(holland_h2(2, 4, c(10, 10)), 2 / (2 + 4 / 10))
  expect_equal(holland_h2(1, 1, c(2, 4)), 1 / (1 + 1 / (2 / (1 / 2 + 1 / 4))))
})

test_that("genotype means equal the average prediction over farms", {
  d = simulate_metric()
  d = d[d$value > 0 & d$value < 100, ]
  d$genotype = factor(d$genotype); d$farm = factor(d$farm)
  fit = lm(value ~ genotype + farm, data = d)
  gm = genotype_means(coef(fit), vcov(fit), levels(d$genotype), check = "A", df = df.residual(fit))
  nd = expand.grid(genotype = levels(d$genotype), farm = levels(d$farm))
  direct = tapply(predict(fit, newdata = nd), nd$genotype, mean)
  expect_equal(gm$mean, as.numeric(direct[gm$genotype]), tolerance = 1e-8)
  expect_equal(gm$percent_of_check[gm$genotype == "A"], 100)
  expect_true(is.na(gm$p_vs_check[gm$genotype == "A"]))
})

test_that("zeros and outliers are handled before the model", {
  d = simulate_metric()
  res = analyse_metric_trait(d, check = "A")
  expect_equal(res$qc$zeros, 3)
  expect_true(res$qc$outliers >= 1)
  expect_true(200 %in% res$outliers$value)
  expect_true(res$method %in% c("lmer", "lm"))
  m = res$means
  expect_equal(m$genotype[1], "C")                 # largest true effect
  expect_true(m$p_vs_check[m$genotype == "C"] < 0.05)
  expect_equal(m$percent_of_check[m$genotype == "A"], 100)
})

test_that("too little data gives a note instead of an error", {
  d = data.frame(farm = c(1, 1, 2), genotype = c("A", "B", "A"), value = c(1, 2, 3))
  res = analyse_metric_trait(d)
  expect_equal(res$note, "not_enough_data")
})
