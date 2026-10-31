test_that("answer_status classifies every kind of answer", {
  x = data.frame(best = c("A", NA, "Not observed", "Tie", "B", "A", "C"),
                 worst = c("B", NA, "Not observed", "Tie", "B", "D", "Tie"),
                 stringsAsFactors = FALSE)
  expect_equal(answer_status(x),
               c("valid", "missing", "not_observed", "tie", "same_best_worst", "invalid", "tie"))
})

test_that("attrition is counted from the submission dates", {
  set_language("en", root_path())
  d = data.frame(REG__submitted_date = c("2022-01-01", "2022-01-01", "2022-01-02", NA),
                 ASSaaaaaaaaaaaa__submitted_date = c("2022-02-01", NA, "2022-02-01", NA))
  pars = list(traits = data.frame(assessmentId = "aaaaaaaaaaaa", assessmentName = "Harvest",
                                  assessmentDay = 90))
  m = collection_moments(pars)
  r = moment_responses(d, m)
  a = attrition_table(r, m, group = c("x", "y", "x", "y"))
  expect_equal(a$n[a$group == "all"], c(3, 2))
  expect_equal(a$percent_of_registered[a$group == "all"][2], 66.7)
  expect_equal(a$lost_since_previous[a$group == "all"], c(NA, 1))
  expect_equal(a$n[a$group == "y"], c(1, 0))
})

test_that("small groups and unlabelled participants are not compared", {
  g = c(rep("Man", 40), rep("Woman", 6), rep("Not specified", 3))
  e = group_eligibility(g, rep(TRUE, length(g)), min_n = 20)
  expect_equal(e$eligible[e$group == "Man"], TRUE)
  expect_equal(e$reason[e$group == "Woman"], "too_few")
  expect_equal(e$reason[e$group == "Not specified"], "no_label")
  expect_equal(e$share[e$group == "Woman"], round(6 / 46, 3))
})

test_that("balanced weights give every group the same total weight", {
  g = c(rep("a", 9), "b")
  w = balanced_weights(g)
  expect_equal(mean(w), 1)
  expect_equal(sum(w[g == "a"]), sum(w[g == "b"]))
})

test_that("ranking agreement is Kendall tau with the consensus order", {
  worth = c(x = 3, y = 2, z = 1)
  r = rbind(c(x = 1, y = 2, z = 3),   # same order
            c(x = 3, y = 2, z = 1),   # reverse order
            c(x = 1, y = 0, z = 2),   # two items, same order
            c(x = 0, y = 0, z = 1))   # one item
  expect_equal(ranking_agreement(r, worth), c(1, -1, 1, NA))
})

test_that("the season length follows the project design", {
  expect_equal(season_length(list(traits = data.frame(assessmentDay = c(30, 85)))), 85)
  expect_equal(season_length(list(traits = data.frame(assessmentDay = c(10, 20)))), 45)
  expect_equal(season_length(list(traits = data.frame(assessmentDay = c(400)))), 180)
  expect_equal(season_length(list()), 60)
})
