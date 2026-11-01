test_that("simulated tricot rankings have three items per participant", {
  R = with_seed(1, simulate_tricot_rankings(c(a = 0, b = 1, c = -1, d = 0.5), n = 25))
  m = unclass(R)
  expect_equal(nrow(m), 25)
  expect_true(all(rowSums(m > 0) == 3))
  expect_true(all(apply(m, 1, function(r) all(sort(r[r > 0]) == 1:3))))
})

test_that("simulated rankings follow the worth of the items", {
  lw = c(a = 3, b = 0, c = 0, d = -3)
  R = with_seed(2, simulate_tricot_rankings(lw, n = 400))
  est = stats::coef(PlackettLuce::PlackettLuce(R), log = TRUE)
  expect_equal(names(est)[which.max(est)], "a")
  expect_equal(names(est)[which.min(est)], "d")
})

test_that("the numbers of participants to simulate are sensible", {
  expect_equal(data_value_sizes(35, 93), c(18L, 35L, 70L, 93L, 105L))
  expect_equal(data_value_sizes(12), c(12L, 24L, 36L))
  expect_error(data_value_sizes(3))
})

test_that("more participants give a more accurate ranking", {
  set_language("en", root_path())
  lw = c(a = 1, b = 0.5, c = 0, d = -0.5, e = -1)
  m = PlackettLuce::PlackettLuce(with_seed(3, simulate_tricot_rankings(lw, n = 60)))
  dv = get_data_value(m, n_now = 20, n_registered = 60, sizes = c(20, 200), nsim = 15)
  expect_true(dv$available)
  expect_equal(dv$table$n, c(20, 200))
  expect_true(dv$table$tau[2] > dv$table$tau[1])
  expect_true(all(dv$table$tau_lower <= dv$table$tau_upper))
  expect_true(is_plot(dv$plot))
  expect_equal(dv$p_now, dv$table$p_winner[1])
  # the same seed gives the same result
  dv2 = get_data_value(m, n_now = 20, n_registered = 60, sizes = c(20, 200), nsim = 15)
  expect_equal(dv$table, dv2$table)
})

test_that("the participation map uses the latest moment with answers", {
  set_language("en", root_path())
  xy = data.frame(longitude = c(10, 11, 12, NA), latitude = c(60, 61, 62, 63))
  r = cbind(Registration = rep(TRUE, 4),
            Harvest = c(TRUE, FALSE, TRUE, TRUE),
            Late = rep(FALSE, 4))
  pm = get_participation_map(xy, r)
  expect_true(pm$available)
  expect_equal(pm$moment, "Harvest")
  expect_equal(pm$n_total, 3)
  expect_equal(pm$n_answered, 2)
  expect_true(is_plot(pm$map))
  expect_error(get_participation_map(xy[1:2, ], r))
  expect_error(get_participation_map(xy, r[, "Registration", drop = FALSE]))
})

test_that("the Kendall bootstrap interval contains the estimate", {
  d = make_tricot_data(n = 60)
  rk = function(s) rank_tricot(d, items = paste0("package_item_", LETTERS[1:3]),
                               input = s, validate.rankings = TRUE)
  r1 = rk(c("ASS1_overallperf_pos", "ASS1_overallperf_neg"))
  r2 = rk(c("ASS1_yield_pos", "ASS1_yield_neg"))
  tau = kendallTau(r1, r2, na.omit = FALSE)$kendallTau[1]
  ci = kendall_boot_interval(r1, r2, nboot = 100)
  expect_length(ci, 2)
  expect_true(ci[1] <= tau && tau <= ci[2])
})
