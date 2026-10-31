test_that("every key has an English text and no language has unknown keys", {
  for (f in list.files(root_path("report", "i18n"), pattern = "[.]csv$", full.names = TRUE)) {
    x = read.csv(f, stringsAsFactors = FALSE, encoding = "UTF-8", fileEncoding = "UTF-8")
    expect_true(all(nzchar(x$en)))
    expect_false(any(duplicated(x$key)))
    expect_true(all(climmob_languages %in% names(x)))
  }
})

test_that("translations only use placeholders that exist in English", {
  for (f in list.files(root_path("report", "i18n"), pattern = "[.]csv$", full.names = TRUE)) {
    x = read.csv(f, stringsAsFactors = FALSE, encoding = "UTF-8", fileEncoding = "UTF-8")
    ph = function(s) sort(unique(regmatches(s, gregexpr("\\{[a-z0-9_]+\\}", s))[[1]]))
    for (i in seq_len(nrow(x))) {
      en = ph(x$en[i])
      for (l in setdiff(climmob_languages, "en")) {
        if (nzchar(x[[l]][i])) expect_equal(setdiff(ph(x[[l]][i]), en), character(0))
      }
    }
  }
})

test_that("every key used in the code and templates exists", {
  used = unlist(lapply(c(list.files(root_path("R"), full.names = TRUE), root_path("report", "mainreport.Rmd")), function(f) {
    m = regmatches(readLines(f, warn = FALSE), gregexpr('tr\\("[a-z0-9_]+"', readLines(f, warn = FALSE)))
    sub('tr\\("', "", sub('"$', "", unlist(m)))
  }))
  have = unlist(lapply(list.files(root_path("report", "i18n"), pattern = "[.]csv$", full.names = TRUE),
                       function(f) read.csv(f, stringsAsFactors = FALSE)$key))
  expect_equal(setdiff(unique(used), have), character(0))
})

test_that("tr() fills placeholders and falls back to English", {
  set_language("no", root_path())
  expect_equal(tr("fb_points", n = 40), "40 poeng")
  expect_equal(tr("no_such_key"), "[no_such_key]")
  # a placeholder named k must not be taken as the key
  expect_equal(tr("map_text", f = 2, k = 1), tr("map_text", f = 2))
  expect_false(startsWith(tr("design_text", n = 1, participants = "x", noptions = 3,
                             options = "y", ntraits = 2, k = 4), "["))
  set_language("xx", root_path())
  expect_equal(current_language(), "en")
  expect_equal(tr("fb_points", n = 3), "3 points")
})

test_that("language codes from ClimMob are normalised", {
  expect_equal(normalize_language("pt-BR"), "pt")
  expect_equal(normalize_language("nb"), "no")
  expect_equal(normalize_language(""), "en")
  expect_equal(normalize_language("rw"), "en")
})

test_that("labels are made plural for each language", {
  expect_equal(plural_label("variety", .lang = "en"), "varieties")
  expect_equal(plural_label("farmer", .lang = "en"), "farmers")
  expect_equal(plural_label("variedad", .lang = "es"), "variedades")
  expect_equal(plural_label("aina", .lang = "sw"), "aina")
  expect_equal(plural_label("sort", .lang = "no"), "sorter")
  expect_equal(plural_label("bonde", .lang = "no"), "b\u00f8nder")
  expect_equal(plural_label("blanding", .lang = "no"), "blandinger")
})

test_that("dates are written in the language", {
  set_language("pt", root_path())
  expect_equal(format_date(as.Date("2026-10-31")), "31 de outubro de 2026")
  set_language("en", root_path())
  expect_equal(format_date(as.Date("2026-01-05")), "5 January 2026")
})
