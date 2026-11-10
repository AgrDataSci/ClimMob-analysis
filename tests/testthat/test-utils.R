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

test_that("set_col_widths rewrites the separator of a pipe table", {
  skip_if_not_installed("knitr")
  k = knitr::kable(data.frame(a = c("x", "y"), bb = 1:2, ccc = 3:4), format = "pipe")
  w = set_col_widths(k, c(3, 1, 2))
  sep = as.character(w)[2]
  expect_true(grepl("^\\|:?-+:?\\|:?-+:?\\|:?-+:?\\|$", sep))
  d = nchar(gsub("[^-]", "", strsplit(gsub("^\\||\\|$", "", sep), "|", fixed = TRUE)[[1]]))
  expect_equal(d, c(30, 10, 20))
  expect_equal(as.character(w)[-2], as.character(k)[-2])
})

test_that("palettes return the requested number of colours", {
  expect_length(pal_diverging(7), 7)
  expect_length(pal_sequential(3), 3)
  expect_equal(pal_qualitative(2), BREWER$Paired[1:2])
  expect_length(pal_qualitative(15), 15)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", c(pal_diverging(5), pal_sequential(5), pal_qualitative(5)))))
  expect_equal(pal_one("dark"), BREWER$Greens[8])
  expect_error(pal_one("pink"))
})

test_that("main_traits keeps the reference trait and yield", {
  tr = c("Germination [Vegetative]", "Yield [Harvest]", "Taste [Harvest]", "Overall preference [Harvest]")
  expect_equal(main_traits(tr, "Overall Preference"), c("Yield [Harvest]", "Overall preference [Harvest]"))
  expect_equal(main_traits(tr, "taste [Harvest]"), c("Yield [Harvest]", "Taste [Harvest]"))
  expect_equal(main_traits(c("Rendimiento [Cosecha]", "Sabor"), "Sabor"), c("Rendimiento [Cosecha]", "Sabor"))
})

test_that("short_trait_list drops the moment when names stay unique", {
  x = c("Yield [Harvest]; Taste [Harvest]", "", "Taste [Harvest]")
  expect_equal(short_trait_list(x), c("Yield; Taste", "", "Taste"))
  y = c("Vigour [Vegetative]; Vigour [Harvest]")
  expect_equal(short_trait_list(y), y)
})

test_that("content_widths weighs long text columns more", {
  d = data.frame(item = c("Hybrid 01", "Hybrid 02"), n = c(10, 12),
                 note = c("a long sentence that needs room", "short"))
  w = content_widths(d)
  expect_length(w, 3)
  expect_true(w[3] > w[1] && w[1] > w[2])
  expect_true(all(w >= 4 & w <= 40))
})
