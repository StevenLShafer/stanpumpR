# pramipexoleER: see R/drugs_pramipexoleER.R (FDA review of NDA 22-421,
# Table 3, and the reviewers' own simulations, pp. 7 and 54).

scrForCrCl <- function(crcl) (140 - 60) * 75 / (72 * crcl)
dayAUC <- function(dose, crcl, tau = 1440, from = 6 * 1440, to = 7 * 1440)
{
  res <- pdSimulate(pdRegimen("pramipexoleER", "mg PO ER", dose, tau, ceiling(to / tau) + 1),
                    maximum = to + 60, weight = 75, height = 175,
                    creatinine = scrForCrCl(crcl))
  pdAUC(res, from, to)
}

test_that("the ER input: zero order into the gut over 1.34 h, then 0.0873/h", {
  m <- pramipexoleER(75, 175, 60, "male", FALSE)
  expect_equal(m$zeroOrderPO$duration, 1.34 * 60)
  expect_equal(m$PK$default$ka_PO * 60, 0.0873)
  expect_equal(m$PK$default$tlag_PO, 0)
  ir <- pramipexole(75, 175, 60, "male", FALSE)$PK$default
  for (f in c("v1", "v2", "cl1", "cl2")) expect_equal(m$PK$default[[f]], ir[[f]], info = f)
})

test_that("the FDA reviewers' simulated day-7 AUCs are reproduced (salt basis)", {
  # Review p. 7: 12.89, 9.43 and 13.46 ng.h/mL (AUC0-24) at CrCl 80, 50, 30
  expect_equal(dayAUC(0.375, 80), 12.89, tolerance = 0.003)
  expect_equal(dayAUC(0.1875, 50), 9.43, tolerance = 0.003)
  expect_equal(dayAUC(0.1875, 30), 13.46, tolerance = 0.003)
  # every other day (AUC0-48): 18.88 at CrCl 50
  expect_equal(dayAUC(0.375, 50, tau = 2880, from = 6 * 1440, to = 8 * 1440), 18.88,
               tolerance = 0.005)
})

test_that("4.5 mg once daily peaks at about 6 hours (label)", {
  res <- pdSimulate(pdRegimen("pramipexoleER", "mg PO ER", 4.5, 1440, 8), maximum = 8 * 1440,
                    weight = 75, height = 175, creatinine = scrForCrCl(130))
  s <- pdSite(res); s <- s[s$Time >= 7 * 1440, ]
  expect_equal(s$Time[which.max(s$Y)] / 60 - 7 * 24, 6, tolerance = 0.1)
})
