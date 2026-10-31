test_that("split_csv cleans comma separated input", {
  expect_equal(split_csv("a, b,,c"), c("a", "b", "c"))
  expect_equal(split_csv(""), character(0))
  expect_equal(split_csv(NULL), character(0))
  expect_equal(split_csv(NA), character(0))
})

test_that("pluralize handles irregular words", {
  expect_equal(pluralize("variety"), "varieties")
  expect_equal(pluralize("farmer"), "farmers")
})

test_that("paste3 joins with a conjunction", {
  expect_equal(paste3(c("A", "B", "C")), "A, B and C")
  expect_equal(paste3(c("A", "B"), lan = "fr"), "A et B")
  expect_equal(paste3("A"), "A")
})

test_that("rename_duplicates makes values unique", {
  expect_equal(rename_duplicates(c("Pear", "Apple", "Pear")),
               c("Pear1", "Apple", "Pear2"))
  expect_equal(rename_duplicates(c("a", "a"), sep = " "), c("a 1", "a 2"))
})

test_that("find_columns prefers exact matches and returns NA when missing", {
  cols = c("ASS1_yield_pos", "yield_pos", "ASS1_yield_neg", "x_age")
  expect_equal(find_columns("yield_pos", cols), "yield_pos")
  expect_equal(find_columns("yield_neg", cols), "ASS1_yield_neg")
  expect_true(is.na(find_columns("taste_pos", cols)))
  expect_equal(find_columns("age", cols, anchor_end = TRUE), "x_age")
  expect_true(is.na(find_columns("ag", cols, anchor_end = TRUE)))
})

test_that("with_seed is reproducible and does not change the global seed", {
  set.seed(10)
  before = runif(1)
  set.seed(10)
  a = with_seed(1, sample(100, 5))
  b = with_seed(1, sample(100, 5))
  expect_equal(a, b)
  expect_equal(runif(1), before)
})
