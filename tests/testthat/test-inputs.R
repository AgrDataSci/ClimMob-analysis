test_that("pick_reference_trait follows the priority rules", {
  expect_equal(pick_reference_trait(c("taste", "yield", "overallperf")), 3)
  expect_equal(pick_reference_trait(c("overall1", "taste", "overall2")), 3)
  expect_equal(pick_reference_trait(c("taste", "yield", "marketability")), 2)
  expect_equal(pick_reference_trait(c("taste", "colour")), 2)
  expect_equal(pick_reference_trait(c("OverallPerf", "yield")), 1)
})

test_that("parse_report_settings reads the ClimMob parameters", {

  p = list(OutputPath = "/tmp/out",
           InfoSheets = "TRUE",
           Languaje = "sw",
           Format = "docx",
           ReferToParticipants = "farmer",
           ReferToTechnologies = "variety",
           BackwardPath = "/srv/code",
           Split = "gender,village",
           Reference = "2,3,4,5,6")

  s = parse_report_settings(p)

  expect_equal(s$output_path, "/tmp/out")
  expect_true(s$infosheets)
  expect_equal(s$language, "sw")
  expect_equal(s$output_format, "word_document")
  expect_equal(s$groups, c("gender", "village"))
  expect_equal(s$reference, 2:5)
})

test_that("an empty reference falls back to the first item", {
  s = parse_report_settings(list(Reference = "", Split = ""))
  expect_equal(s$reference, 1L)
  expect_equal(s$groups, character(0))
})

test_that("get_reference_names reads alias names", {
  x = list(combination = list(elements = list(list(alias_name = "A"),
                                              list(alias_name = "B"))))
  expect_equal(get_reference_names(x, c(2, 5)), "B")
  expect_equal(get_reference_names(list(), 1), character(0))
})

test_that("decode_pars decodes traits and picks one reference trait", {

  x = list(Characteristics = list(
    vars = list(list("ASS1_yield_pos", "ASS1_yield_neg"),
                list("ASS1_overallperf_pos", "ASS1_overallperf_neg")),
    name = c("Yield", "Overall performance"),
    codeQst = c("yield", "overallperf"),
    questionAsked = list(c("best", "worst"), c("best", "worst")),
    code = NA))

  pars = decode_pars(x)

  expect_equal(pars$traits$nameString1, c("ASS1_yield_pos", "ASS1_overallperf_pos"))
  expect_equal(sum(pars$traits$traitOrder == "referenceTrait"), 1)
  expect_equal(pars$traits$traitOrder[2], "referenceTrait")
  expect_equal(pars$covariates, character(0))
  expect_equal(pars$linear, character(0))
})
