library(lubridate)

## tests should possibly be added for invalid times

## tests for validateTime

test_that("'MM' gets parsed correctly", {
    expect_equal(validateTime("80"),"80")
})

test_that("'HH:MM' gets parsed correctly", {
    expect_equal(validateTime("08:44"),"08:44")
})

test_that("'HH:MM' gets parsed correctly", {
    expect_equal(validateTime("08:80"),"09:20")
})

## tests for getReferenceTime

test_that("'HH:MM:SS AM' gets parsed correctly", {
    expect_equal(getReferenceTime("08:30:00 AM"),"08:30")
})

test_that("'HH:MM:SS PM' gets parsed correctly", {
    expect_equal(getReferenceTime("08:44:55 PM"),"20:30")
})

test_that("'HH:MM AM' gets parsed correctly", {
    expect_equal(getReferenceTime("08:30 AM"),"08:30")
})

test_that("'HH:MM PM' gets parsed correctly", {
    expect_equal(getReferenceTime("08:44 PM"),"20:30")
})

test_that("'HH:MM' gets parsed correctly", {
    expect_equal(getReferenceTime("08:44"),"08:30")
})
