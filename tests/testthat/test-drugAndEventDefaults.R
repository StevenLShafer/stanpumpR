test_that("desired objects can be obtained", {
    objs <- getDrugAndEventDefaultsGlobal()
    expect_true(names(objs[[1]])[[1]]=="Drug" && names(objs[[2]])[[1]]=="Event")
})
