# Dihydroartemisinin (R/drugs_dihydroartemisinin.R): artemether's active
# metabolite, Ding 2026, Table 2.  Drafted by Claude Code, 2026-11-24.
# Its curve, formed from artemether, is checked in test-drugs-artemether.R.

test_that("Ding 2026, Table 2: one compartment, CL/F 255 L/h, VC/F 64.1 L at 45 kg", {
  for (w in c(20, 45, 80)) {
    set <- dihydroartemisinin(w, 170, 30, "male", adjustToFFM = FALSE)$PK$default
    expect_equal(set$cl1 * 60, 255 * (w / 45)^0.75)
    expect_equal(set$v1, 64.1 * w / 45)
    expect_equal(c(set$cl2, set$cl3), c(0, 0))
  }
  # Its own half-life is minutes, so its curve follows its formation
  set <- dihydroartemisinin(45, 160, 30, "male", adjustToFFM = FALSE)$PK$default
  expect_equal(round(log(2) / (set$cl1 / set$v1)), 10)
})

test_that("the metabolite artemether forms is this model", {
  x <- artemether(60, 165, 35, "female")
  set <- dihydroartemisinin(60, 165, 35, "female")$PK$default
  expect_equal(x$transitChain$metabolite$v, set$v1)
  expect_equal(x$transitChain$metabolite$kel, set$cl1 / set$v1)
})
