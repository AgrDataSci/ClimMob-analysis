make_cmlist = function(code = "P1", moment = "Harvest", ass = "aaaaaaaaaaaa", trait = "overallperf") {
  fields = data.frame(name = c("_submitted_date", paste0("char_", trait, c("_pos", "_neg")),
                               "yield_a", "yield_b", "yield_c", "plantingdate", "slope"),
                      odktype = c("datetime", "select one", "select one", "decimal", "decimal",
                                  "decimal", "date", "select one"),
                      desc = c("date", "Best?", "Worst?", "Yield A", "Yield B", "Yield C", "Planting", "Slope"),
                      stringsAsFactors = FALSE)
  list(project = list(project_cod = code),
       registry = list(fields = data.frame(name = c("qst162", "farmername", "gender"),
                                           odktype = c("barcode", "text", "select one"),
                                           desc = c("Package", "Name", "Gender"),
                                           stringsAsFactors = FALSE)),
       assessments = structure(list(code = ass, desc = moment, intervalindays = 90,
                                    fields = list(fields)),
                               class = "data.frame", row.names = 1L))
}

test_that("the catalog finds traits, measurements and covariates", {
  cat = project_catalog(make_cmlist())
  expect_equal(sort(unique(cat$type)), c("covariate", "quantitative", "ranking"))
  expect_equal(cat$column[cat$type == "ranking" & cat$role == "pos"], "ASSaaaaaaaaaaaa_char_overallperf_pos")
  expect_equal(cat$code[cat$type == "quantitative"][1], "yield")
  expect_true("REG_gender" %in% cat$column[cat$type == "covariate"])
  expect_false("REG_farmername" %in% cat$column)
})

test_that("parameters built from the catalog look like a ClimMob request", {
  pars = build_pars_from_cmlist(make_cmlist())
  expect_equal(pars$traits$nameString2, "ASSaaaaaaaaaaaa_char_overallperf_neg")
  expect_equal(pars$traits$traitOrder, "referenceTrait")
  expect_equal(pars$traits$name, "Overall performance")
  expect_equal(pars$linear$nameString3, "ASSaaaaaaaaaaaa_yield_c")
  expect_true("REG_gender" %in% pars$covariates$nameString)
})

test_that("projects with different templates are combined", {
  c1 = make_cmlist("P1", "Harvest", "aaaaaaaaaaaa", "overallperf")
  c2 = make_cmlist("P2", "Final evaluation", "bbbbbbbbbbbb", "overallchar")
  h = suggest_harmonization(list(project_catalog(c1), project_catalog(c2)))
  # the user aligns the moments
  h$moment[h$moment == "finalevaluation"] = "harvest"
  d1 = data.frame(REG_qst162 = 1:2, package_item_A = "x", package_item_B = "y", package_item_C = "z",
                  ASSaaaaaaaaaaaa_char_overallperf_pos = c("A", "B"),
                  ASSaaaaaaaaaaaa_char_overallperf_neg = c("C", "A"),
                  ASSaaaaaaaaaaaa__submitted_date = "2022-01-01", REG_gender = c("Man", "Woman"))
  d2 = data.frame(REG_qst162 = 1, package_item_A = "y", package_item_B = "x2", package_item_C = "z",
                  ASSbbbbbbbbbbbb_char_overallchar_pos = "C",
                  ASSbbbbbbbbbbbb_char_overallchar_neg = "B", REG_gender = "Woman")
  comb = combine_projects(list(P1 = d1, P2 = d2), h,
                          item_map = data.frame(project = "P2", original = "x2", item = "x"))
  expect_equal(nrow(comb), 3)
  expect_equal(comb$harvest_overall_pos, c("A", "B", "C"))
  expect_equal(comb$REG_qst162, c("P1-1", "P1-2", "P2-1"))
  expect_equal(comb$package_item_B[3], "x")
  expect_equal(comb$cov_gender, c("Man", "Woman", "Woman"))
  pars = pars_from_harmonization(h)
  expect_equal(pars$traits$nameString1, "harvest_overall_pos")
  expect_equal(pars$traits$assessmentName, "Harvest")
  expect_true("project_code" %in% pars$covariates$nameString)
})

test_that("feedback data holds the own and the community ranking", {
  set_language("en", ".")
  d = make_tricot_data(n = 30)
  rd = organize_ranking_data(d, make_pars())
  fake = function(w) structure(list(w = w), class = "fakepl")
  registerS3method("coef", "fakepl", function(object, log = FALSE, ...) object$w)
  w = c(Apple = 1, Banana = 2, Cherry = 3, Date = 4)
  fb = get_feedback_data(rd$cmdata, rd, list(fake(w), fake(w)))
  p = fb$participants
  expect_equal(nrow(p), 30)
  ok = p$answered_reference_trait
  expect_true(all(p$first[ok] != p$third[ok]))
  pts = p[, c("points_A", "points_B", "points_C")]
  expect_true(all(abs(rowSums(pts) - 100) <= 1))
  # the community points follow the worth of the three items
  i = 1
  items = unlist(p[i, c("item_A", "item_B", "item_C")])
  expect_equal(order(unlist(pts[i, ])), order(w[items]))
  expect_equal(fb$results$rank[fb$results$item == "Date"][1], 1)
  expect_equal(nrow(fb$answers), 60)
})
