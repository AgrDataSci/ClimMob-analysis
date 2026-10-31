test_that("clean_tricot_answers removes invalid answers", {

  x = data.frame(best = c("A", "B", "Tie", "C", "A", NA, "A"),
                 worst = c("B", "B", "Tie", "Not observed", "D", "C", "Tie"),
                 stringsAsFactors = FALSE)

  out = clean_tricot_answers(x)

  expect_equal(out$best, c("A", NA, NA, NA, NA, NA, NA))
  expect_equal(out$worst, c("B", NA, NA, NA, NA, NA, NA))
})

test_that("build_groups caps the number of groups", {

  d = data.frame(village = rep(letters[1:10], each = 6), stringsAsFactors = FALSE)

  g = build_groups(d, "village", max_groups = 8, min_per_group = 5)

  expect_equal(length(unique(g)), 9) # 8 groups and "Others"
  expect_true("Others" %in% g)
})

test_that("build_groups returns NULL when groups are too small or missing", {

  d = data.frame(gender = c(rep("Man", 10), "Woman"), stringsAsFactors = FALSE)
  expect_null(build_groups(d, "gender", min_per_group = 5))
  expect_null(build_groups(d, "not_a_column"))
  expect_null(build_groups(d, character(0)))
  expect_equal(length(build_groups(d, "gender", min_per_group = 0)), 11)
})

test_that("organize_ranking_data keeps valid traits and cleans the data", {

  d = make_tricot_data()
  d$ASS1_yield_pos[1:3] = "Not observed"

  rd = organize_ranking_data(d, make_pars(),
                             reference_tech = "Banana",
                             option_label = "variety")

  expect_equal(names(rd$trait_list), c("yield", "overallperf"))
  expect_equal(rd$reference_trait_index, 2)
  expect_equal(rd$reference_trait, "overall performance")
  expect_equal(rd$reference_tech, "Banana")
  expect_equal(sum(rd$trait_list$yield$keep), 27)
  expect_true(all(is.na(rd$cmdata$ASS1_yield_pos[1:3])))
})

test_that("organize_ranking_data uses the first item when the reference is unknown", {
  rd = organize_ranking_data(make_tricot_data(), make_pars(), reference_tech = "Kiwi")
  expect_equal(rd$reference_tech, "Apple")
})

test_that("a trait whose columns are missing is dropped, not fatal", {

  pars = make_pars()
  pars$traits$nameString1[1] = "taste_pos"

  rd = organize_ranking_data(make_tricot_data(), pars)

  expect_equal(names(rd$trait_list), "overallperf")
  expect_equal(rd$trait_dropped, "Yield [Harvest]")
})

test_that("organize_ranking_data stops with a clear message when no trait is valid", {

  d = make_tricot_data(n = 4)

  expect_error(organize_ranking_data(d, make_pars()), "None of the 2 traits")
})

test_that("the reference trait falls back to the last kept trait", {

  d = make_tricot_data()
  d$ASS1_overallperf_pos = NA

  rd = organize_ranking_data(d, make_pars())

  expect_equal(names(rd$trait_list), "yield")
  expect_equal(rd$reference_trait_index, 1)
})

test_that("organize_quantitative_data does not depend on global objects", {

  d = make_tricot_data()
  d$ASS1_weight_a = rnorm(nrow(d), 10)
  d$ASS1_weight_b = rnorm(nrow(d), 10)
  d$ASS1_weight_c = c(100, rnorm(nrow(d) - 1, 10))

  pars = make_pars()
  pars$linear = data.frame(nameString1 = "weight_a",
                           nameString2 = "weight_b",
                           nameString3 = "weight_c",
                           nQst = 3,
                           name = "weight",
                           codeQst = "weight",
                           assessmentId = "1",
                           assessmentName = "Harvest",
                           stringsAsFactors = FALSE)

  q = organize_quantitative_data(d, pars, id = "id")

  expect_true(q$quantitative)
  expect_equal(nrow(q$quanti_dat[[1]]), 3 * nrow(d))
  expect_true(100 %in% q$outliers$value)
  expect_true(all(is.na(q$quanti_dat[[1]]$enumerator)))
})
