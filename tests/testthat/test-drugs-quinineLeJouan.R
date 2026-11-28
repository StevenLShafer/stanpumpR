# Quinine, children (R/drugs_quinineLeJouan.R): Le Jouan 2005, Table 2.
# Drafted by Claude Code, 2026-11-28.  Expectations are computed here from
# the paper, not from the drug file.

salt <- 2 * 324.42 / 782.96

test_that("Le Jouan 2005, Table 2, the unbound fraction at its 36-hour 0.15", {
  for (w in c(8, 15, 23)) {
    set <- quinineLeJouan(w, 95, 3, "male", adjustToFFM = FALSE)$PK$default
    expect_equal(set$cl1 * 60, 0.15 * 0.53 * w)
    expect_equal(set$v1, 0.15 * (57 + 3.8 * w))
    expect_equal(set$ka_PO * 60, 0.934)
    expect_equal(c(set$cl2, set$cl3), c(0, 0))
    expect_equal(set$bioavailability_PO, salt)
  }
})

test_that("15 kg: the paper's values at fu 0.114 and the model's at 0.15", {
  # The paper (Results): at fu 0.114, CL/F 0.91 L/h and V/F 13 L
  expect_equal(round(0.114 * 0.53 * 15, 2), 0.91)
  expect_equal(round(0.114 * (57 + 3.8 * 15)), 13)
  set <- quinineLeJouan(15, 95, 3, "male", FALSE)$PK$default
  expect_equal(set$cl1 * 60, 1.1925)
  expect_equal(set$v1, 17.1)
  # the half-life is independent of fu: 9.9 h (the paper prints 9.1)
  expect_equal(round(log(2) * set$v1 / set$cl1 / 60, 1), 9.9)
})

test_that("switch on: the weight terms read the pharmacokinetic weight", {
  pkW <- pkSizeFactors(15, 95, 3, "male")$pkWeight
  set <- quinineLeJouan(15, 95, 3, "male")$PK$default
  expect_equal(set$cl1 * 60, 0.15 * 0.53 * pkW)
})

test_that("finite and positive across the legal patients", {
  for (f in c("quinineLeJouan", "quinineKloprogge", "quinine"))
    for (wt in c(0.5, 3, 40, 250)) for (adj in c(TRUE, FALSE)) for (age in c(0, 10, 90)) {
      set <- get(f)(wt, 170, age, "female", adj)$PK$default
      v <- c(set$v1, set$cl1, set$ka_PO)
      expect_true(all(is.finite(v) & v > 0), info = paste(f, wt, adj, age))
    }
})
