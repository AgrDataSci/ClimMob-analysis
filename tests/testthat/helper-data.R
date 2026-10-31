# Synthetic tricot data shared by the tests

make_tricot_data = function(n = 30, seed = 1) {

  set.seed(seed)

  items = c("Apple", "Banana", "Cherry", "Date")

  pkg = t(replicate(n, sample(items, 3)))

  answer = function() {
    t(replicate(n, sample(LETTERS[1:3], 2)))
  }

  ov = answer()
  yl = answer()

  data.frame(id = seq_len(n),
             package_item_A = pkg[, 1],
             package_item_B = pkg[, 2],
             package_item_C = pkg[, 3],
             ASS1_overallperf_pos = ov[, 1],
             ASS1_overallperf_neg = ov[, 2],
             ASS1_yield_pos = yl[, 1],
             ASS1_yield_neg = yl[, 2],
             REG_gender = sample(c("Man", "Woman"), n, replace = TRUE),
             stringsAsFactors = FALSE)

}

make_pars = function() {

  traits = data.frame(nameString1 = c("yield_pos", "overallperf_pos"),
                      nameString2 = c("yield_neg", "overallperf_neg"),
                      nQst = 2,
                      name = c("Yield", "Overall performance"),
                      codeQst = c("yield", "overallperf"),
                      questionAsked1 = c("Best yield?", "Best overall?"),
                      questionAsked2 = c("Worst yield?", "Worst overall?"),
                      assessmentId = "ASS1",
                      assessmentName = "Harvest",
                      assessmentDay = 90,
                      traitOrder = c("otherTraits", "referenceTrait"),
                      stringsAsFactors = FALSE)

  list(traits = traits,
       tricotVSlocal = character(0),
       covariates = character(0),
       linear = character(0))

}
