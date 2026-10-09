# Every intravenous model in the library, at pediatric profiles the Patient
# Profile accepts, in both positions of the fat-free-mass switch.  Most models
# were developed in adults, so these are extrapolations and the values are not
# pinned; the test only requires that no model turns an adult size formula
# into a nonpositive or nonfinite volume or clearance (metronidazole's Devine
# adjusted weight once gave V1 = -8.2 L for a 7 kg, 65 cm infant with the
# switch off).

pediatricProfiles <- data.frame(
  label  = c("newborn", "infant boy", "infant girl", "child boy", "child girl", "adolescent girl"),
  age    = c(0.01, 0.5, 0.5, 5, 5, 12),
  weight = c(3.5, 7, 7, 20, 20, 40),
  height = c(50, 65, 65, 110, 110, 150),
  sex    = c(SEX_MALE, SEX_MALE, SEX_FEMALE, SEX_MALE, SEX_FEMALE, SEX_FEMALE),
  stringsAsFactors = FALSE
)

test_that("every drug model gives positive, finite volumes and clearances in children", {
  drugDefaults <- getDrugDefaultsGlobal()
  drugs <- drugDefaults$Drug[!isGasDrug(drugDefaults$Drug)]
  expect_gt(length(drugs), 40)
  for (drug in drugs) {
    fn <- get(drug, mode = "function")
    for (i in seq_len(nrow(pediatricProfiles))) {
      p <- pediatricProfiles[i, ]
      for (ffm in c(TRUE, FALSE)) {
        where <- sprintf("%s, %s, adjustToFFM = %s", drug, p$label, ffm)
        out <- fn(p$weight, p$height, p$age, p$sex, adjustToFFM = ffm)
        for (event in names(out$PK)) {
          x <- out$PK[[event]]
          volumes <- unlist(x[c("v1", "v2", "v3")])
          clearances <- unlist(x[c("cl1", "cl2", "cl3")])
          info <- paste0(where, ", event ", event)
          expect_true(all(is.finite(volumes)) && all(volumes > 0), info = info)
          expect_true(all(is.finite(clearances)) && all(clearances >= 0), info = info)
          expect_true(x$cl1 > 0, info = info)
        }
      }
    }
  }
})
