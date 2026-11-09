# Sikia results travel in the data file under SIKIA_results. The parsing
# runs in base R, the charts need ggplot2 and are skipped in the base job.

sikia_fixture = function() {
  jsonlite::fromJSON(root_path("tests", "testdata", "simulated-maize", "data.json"))
}

test_that("the Sikia payload is found and typed", {
  x = sikia_fixture()
  p = sikia_payload(x)
  expect_setequal(names(p), c("summary", "table", "topic_tree"))
  expect_null(sikia_payload(list(project = list())))
  expect_null(sikia_payload(list(SIKIA_results = list())))
})

test_that("labels are counted and ordered by mentions", {
  p = sikia_payload(sikia_fixture())
  lab = sikia_labels(p$summary)
  expect_true(all(c("label", "mentions", "positive", "neutral", "negative", "net") %in% names(lab)))
  expect_equal(lab$mentions, sort(lab$mentions, decreasing = TRUE))
  expect_equal(lab$mentions, lab$positive + lab$neutral + lab$negative)
  expect_true(all(lab$net >= -1 & lab$net <= 1))
  expect_equal(nrow(sikia_labels(p$summary, top = 3)), 3)
  expect_equal(nrow(sikia_labels(list(labels = list()))), 0)
})

test_that("variety by label and topics are tabulated", {
  p = sikia_payload(sikia_fixture())
  vl = sikia_variety_labels(p$summary)
  expect_true(nrow(vl) > 0)
  expect_true(all(vl$mentions > 0))
  expect_equal(nrow(sikia_variety_labels(list(variety_x_label = list(varieties = list())))), 0)
  tp = sikia_topics(p$topic_tree)
  expect_equal(nrow(tp), 7)
  expect_true(all(nzchar(tp$phrases)))
})

test_that("quotes come from grounded evidence of the top labels", {
  p = sikia_payload(sikia_fixture())
  lab = sikia_labels(p$summary)
  m = sikia_mentions(p$table)
  q = sikia_quotes(m, lab, n_labels = 3)
  expect_true(all(q$label %in% lab$label[1:3]))
  expect_true(all(q$sentiment %in% c("positive", "negative")))
  expect_true(all(q$quote %in% m$evidence_en))
})

test_that("get_sikia returns the placeholder without results and the full list with them", {
  expect_false(get_sikia(list())$available)
  expect_false(get_sikia(NULL)$available)
  need_analysis_packages("ggplot2")
  s = get_sikia(sikia_fixture(), top_labels = 6)
  expect_true(s$available)
  expect_equal(nrow(s$labels), 6)
  expect_equal(s$totals$responses, 84L)
  expect_true(is_plot(s$sentiment_plot))
  expect_true(is_plot(s$variety_plot))
  expect_equal(s$built_at, "2026-10-20")
})
