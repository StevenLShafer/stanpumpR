# insulinLisproMix50: see R/insulinTham.R (50% lispro protamine, 50% lispro).
test_that("the components' shares are conserved", {
  m <- insulinLisproMix50(70, 170, 35, "male", FALSE)
  pk <- m$PK$default
  zero <- m$zeroOrderSC$fraction
  expect_equal(zero, 0.5 * (1 - 0.729))
  expect_equal((1 - zero) * pk$fraction_SC_slow, 0.5)
  expect_equal((1 - zero) * (1 - pk$fraction_SC_slow), 0.5 * 0.729)
})
