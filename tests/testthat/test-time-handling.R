# ---------------------------------------------------------------------------
# test-time-handling.R
#
# End-to-end coverage of the four time helpers that sit between what a user
# types into the dose/event/target tables and the elapsed-minute scale the PK
# engine actually simulates on:
#
#   R/validateTime.R      validateTime()      free text  -> canonical time text
#                         getReferenceTime()  browser clock -> quarter-hour "HH:MM"
#   R/hourMinute.R        hourMinute()        "HH:MM"    -> minutes past midnight
#   R/clockTimeToDelta.R  clockTimeToDelta()  clock time -> minutes after reference
#   R/deltaToClockTime.R  deltaToClockTime()  minutes after reference -> clock time
#
# The existing files test-validateTime.R, test-hourMinute.R,
# test-clockTimeToDelta.R and test-deltaToClockTime.R cover a handful of happy
# paths each; test-validateTime.R carries the standing note "tests should
# possibly be added for invalid times". This file is that missing half: garbage
# input, arithmetic edge cases, and the clock <-> delta round trip. Every case
# here is deliberately disjoint from the cases already in those four files.
#
# Expected values are derived from the algebra the functions implement, not
# copied out of the console:
#   * validateTime's rollover is exactly  HH' = HH + MM %/% 60,  MM' = MM %% 60,
#     so the invariant is that 60*HH + MM is conserved. The tests assert the
#     invariant as well as the formatted string.
#   * getReferenceTime's flooring is exactly  floor(minutesPastMidnight/15)*15.
#   * clockTimeToDelta's "add 1440 if negative" is, for any two same-day clock
#     times, identical to  (t - reference) %% 1440 -- so the expected deltas are
#     computed with %% 1440 from a private string parser that never touches the
#     package code.
#
# KNOWN LIMITATIONS / pinned quirks (each marked "pinned quirk" at its test).
# These tests assert what the code does today, NOT what it arguably should do;
# fixing any of them should deliberately update the corresponding block here:
#   1. getReferenceTime() strips lowercase "am"/"pm" before parsing, so
#      "11:59 pm" yields "11:45" rather than "23:45" -- a silent 12-hour error.
#   2. validateTime() is documented as never erroring, but sprintf("%02d", .)
#      throws once the hour count exceeds .Machine$integer.max, and a
#      zero-length input trips an if() on NA.
#   3. clockTimeToDelta() throws "NAs are not allowed in subscripted
#      assignments" when the same vector holds an unparseable clock time AND a
#      clock time earlier than the reference.
#   4. deltaToClockTime() rounds the minute field independently of the hour
#      field, so a half-minute delta can format as the impossible "HH:60"; and
#      it errors (rather than returning NA) on a reference hourMinute cannot
#      parse, whereas clockTimeToDelta returns NA for the same reference.
# Issue #270 (effect-site target time before the start time hangs the app) is
# NOT exercised here: no simulation is driven from this file, so nothing can
# hang. The wrap behaviour that plausibly feeds it -- a target time two hours
# before the reference silently becoming +1320 minutes -- is pinned in
# "clockTimeToDelta wraps clock times before the reference past midnight".
#
# Deterministic (no RNG) and locale independent: no test matches error-message
# text (R translates those), and the non-ASCII inputs are built with
# intToUtf8() so this source file itself stays pure ASCII.
#
# Provenance: drafted by Claude Code (Fable 5), 2026-08-14, for the
# pre-deployment test plan (input validation & robustness). Expected values
# derived from the closed-form rollover / flooring / modulo algebra described
# above, cross-checked against the source of R/validateTime.R, R/hourMinute.R,
# R/clockTimeToDelta.R and R/deltaToClockTime.R.
# ---------------------------------------------------------------------------


# --- private helpers: independent of the package under test -----------------

# Parse "HH:MM" (or "HHH:MM") into total minutes WITHOUT calling hourMinute,
# so that the round-trip and rollover assertions are not checked against the
# same parser they are testing. Unlike hourMinute this happily accepts hour
# fields above 23, which validateTime can legitimately produce.
totalMinutes <- function(hhmm) {
  parts <- as.numeric(strsplit(hhmm, ":", fixed = TRUE)[[1]])
  60 * parts[1] + parts[2]
}

# Closed form of clockTimeToDelta's single "+1440 if negative" wrap, valid for
# any pair of same-day clock times (the difference always lies in (-1440,1440)).
expectedDelta <- function(reference, clock) {
  (totalMinutes(clock) - totalMinutes(reference)) %% 1440
}


# ===========================================================================
# 1. validateTime: the invalid-time cases the existing file says are missing
# ===========================================================================

test_that("validateTime coerces nonsense clock strings instead of erroring", {
  # validateTime's header promises it is "designed to accept pretty much
  # anything and not return an error". These are the inputs a user can actually
  # produce by fat-fingering a dose table cell. For each one we state the exact
  # coerced value, because downstream code (clockTimeToDelta) treats a string
  # with a colon and a string without one completely differently.
  cases <- list(
    # Out-of-range fields are rolled over, never clamped: 25 h 99 min is
    # 25*60 + 99 = 1599 min = 26 h 39 min, and 99 h 99 min = 6039 min =
    # 100 h 39 min. Note there is no 24-hour ceiling.
    list("25:99", "26:39"),
    list("99:99", "100:39"),
    # Anything with no digits at all collapses to the string "0".
    list("abcdef", "0"),
    list(" ", "0"),
    list("!@#$%^&*", "0"),
    # Embedded control characters are stripped by the [^0-9:.] filter.
    list("\t8:30\n", "08:30"),
    # A minus sign is simply deleted, so negative input becomes positive.
    list("-30", "30"),
    list("-0:30", "00:30"),
    list("- 1:30", "01:30"),
    list("+8:30", "08:30"),
    # A meridiem suffix is silently discarded -- validateTime has no notion of
    # PM, so 8:30 PM is stored as 08:30. (getReferenceTime is the only helper
    # that understands AM/PM at all.)
    list("8:30 PM", "08:30"),
    # Separators other than ":" and "." are deleted, gluing the digits
    # together; "8;30" becomes 830 elapsed minutes, not 8 h 30 min.
    list("8;30", "830"),
    list("8-30", "830"),
    list("8 30", "830"),
    list("08h30", "0830"),
    # Degenerate colon placement: empty hour or empty minute reads as zero.
    list(":", "00:00"),
    list("::", "00:00"),
    list(":30", "00:30"),
    list("30:", "30:00"),
    # Degenerate decimal points short-circuit to "0" via the explicit guard.
    list("..", "0"),
    list(".:", "0"),
    list(":.", "0"),
    # Leading zeros in either field are absorbed by as.numeric().
    list("8:3", "08:03"),
    list("008:0030", "08:30"),
    # Extra colon groups are concatenated onto the minute field, so a
    # seconds-bearing time is mis-read: "12:34:56" -> hour 12, minute "3456",
    # and 3456 min = 57 h 36 min, giving 12 + 57 = 69 hours.
    list("12:34:56", "69:36"),
    # Same rule with four groups: hour 1, minute "234" = 3 h 54 min -> 04:54.
    list("1:2:3:4", "04:54"),
    # A very long pure-digit string is passed straight through untouched: with
    # no colon and no dot there is no arithmetic and no numeric conversion.
    list("12345678901234567890", "12345678901234567890")
  )
  for (case in cases) {
    result <- validateTime(case[[1]])
    # The contract is "always a character scalar, never an error".
    expect_type(result, "character")
    expect_length(result, 1)
    expect_equal(result, case[[2]])
  }
})

test_that("validateTime strips control characters and non-ASCII characters", {
  # Built with intToUtf8() so that this source file stays pure ASCII -- no raw
  # control bytes, and no dependence on how an editor or a checkout re-encodes
  # the file.
  soh       <- intToUtf8(0x0001)                # START OF HEADING
  bel       <- intToUtf8(0x0007)                # BELL
  eAcute    <- intToUtf8(0x00E9)                # LATIN SMALL LETTER E WITH ACUTE
  smartOpen <- intToUtf8(0x201C)                # LEFT DOUBLE QUOTATION MARK
  smartShut <- intToUtf8(0x201D)                # RIGHT DOUBLE QUOTATION MARK
  fullWidth <- intToUtf8(c(0xFF10, 0xFF18))     # FULLWIDTH DIGIT ZERO / EIGHT

  # Non-printing bytes that can ride along with a clipboard paste are removed
  # by the [^0-9:.] filter along with everything else.
  expect_equal(validateTime(paste0(soh, "08:30", bel)), "08:30")

  # A stray accented letter, or the smart quotes a word processor substitutes
  # when a time is pasted in, are removed like any other non-[0-9:.] character.
  expect_equal(validateTime(paste0(eAcute, "08:30")), "08:30")
  expect_equal(validateTime(paste0(smartOpen, "8:30", smartShut)), "08:30")
  # Full-width digits are NOT in the [0-9] range, so "08" typed with a CJK
  # input method is erased entirely and degrades to "0" -- not to 8 minutes.
  expect_equal(validateTime(fullWidth), "0")
})

test_that("validateTime normalises non-character scalars", {
  # The dose table hands validateTime factors and numerics as well as strings
  # (see the "Stored as factors... Arrgh....." comment in R/suggest.R), and
  # empty cells arrive as NA.
  expect_equal(validateTime(NA), "0")
  expect_equal(validateTime(NA_character_), "0")
  expect_equal(validateTime(NaN), "0")
  expect_equal(validateTime(NULL), "0")
  # A logical is pasted as "TRUE", which contains no digits -> "0".
  expect_equal(validateTime(TRUE), "0")
  expect_equal(validateTime(90), "90")
  expect_equal(validateTime(90.5), "90.5")
  expect_equal(validateTime(factor("8:30")), "08:30")
})


# ===========================================================================
# 2. validateTime: rollover algebra
# ===========================================================================

test_that("validateTime rollover conserves total minutes", {
  # The rollover is HH' = HH + MM %/% 60 and MM' = MM %% 60, so 60*HH + MM is
  # invariant. Assert both the invariant (the property that matters to the PK
  # engine) and the exact formatted string (the property that matters to the
  # UI). Exact multiples of 60 are included on purpose because they are where
  # an off-by-one in floor()/%% would show up.
  HH <- c(0, 0, 0, 0, 1, 2, 2, 3, 5, 10, 23, 0, 1, 0, 12)
  MM <- c(0, 59, 60, 120, 60, 59, 60, 119, 600, 0, 59, 180, 1439, 1440, 61)
  expected <- c("00:00", "00:59", "01:00", "02:00", "02:00", "02:59", "03:00",
                "04:59", "15:00", "10:00", "23:59", "03:00", "24:59", "24:00",
                "13:01")
  for (i in seq_along(HH)) {
    input  <- sprintf("%d:%d", HH[i], MM[i])
    result <- validateTime(input)
    expect_equal(result, expected[i])
    # Independent check: no minutes were created or destroyed by the rollover.
    expect_equal(totalMinutes(result), 60 * HH[i] + MM[i])
  }
  # Nothing caps the result at 24 hours -- "0:1440" is a full day and formats
  # as hour 24, which hourMinute() would later refuse to parse (see below).
  expect_equal(validateTime("0:1440"), "24:00")
})


# ===========================================================================
# 3. validateTime: a decimal point beats a colon
# ===========================================================================

test_that("validateTime lets a decimal point short-circuit colon handling", {
  # Reading R/validateTime.R: the dot branch
  #     if (as.numeric(regexpr("[.]", x) > -1)) return(gsub("[^[:digit:].]","",x))
  # runs BEFORE the colon branch and returns immediately, stripping the colon
  # instead of interpreting it. So the dot wins, and any colon in the same
  # string is deleted rather than honoured.
  expect_equal(validateTime("1.5"), "1.5")
  expect_equal(validateTime(".5"), ".5")
  expect_equal(validateTime("5."), "5.")
  # Only the first dot is kept as a separator; later ones are concatenated.
  expect_equal(validateTime("1.2.3"), "1.23")

  # pinned quirk: "1:30.5" is a user writing 1 h 30.5 min = 90.5 minutes, but
  # the dot branch fires first, deletes the colon, and yields 130.5 minutes.
  # Pinned deliberately; if the dot/colon precedence is ever fixed this
  # expectation should be updated to "90.5" (or whatever the fix produces).
  expect_equal(validateTime("1:30.5"), "130.5")
  # Same branch, other ordering: the colon after the dot is deleted too.
  expect_equal(validateTime("1.30:5"), "1.305")
})


# ===========================================================================
# 4. validateTime: documented vector rejection, plus its undocumented throws
# ===========================================================================

test_that("validateTime rejects vector input", {
  # The guard is explicit in the source; match on no message text because R
  # translates error messages under a non-English locale.
  expect_error(validateTime(c("1", "2")))
  expect_error(validateTime(c(60, 90)))
})

test_that("validateTime throws on inputs its header promises to survive", {
  # pinned quirk / KNOWN LIMITATION. validateTime advertises that it never
  # errors, but two paths do. Pinned so that a fix is a deliberate, visible
  # change to this test rather than a silent behaviour drift.

  # (a) The final sprintf("%02d", HH) cannot format a double above
  #     .Machine$integer.max. 2147483647 hours is fine ...
  expect_equal(validateTime("2147483647:00"), "2147483647:00")
  #     ... one hour more is not.
  expect_error(validateTime("2147483648:00"))
  #     The same overflow is reachable through the minute field alone, because
  #     the rollover pushes minutes into the hour count:
  #     128849018880 min / 60 = 2147483648 h.
  expect_error(validateTime("0:128849018880"))

  # (b) NULL is handled explicitly, but a zero-length character vector reaches
  #     `if (is.null(x) || is.na(x) ...)` with a zero-length is.na() result.
  expect_equal(validateTime(NULL), "0")
  expect_error(validateTime(character(0)))
})


# ===========================================================================
# 5. getReferenceTime
# ===========================================================================

test_that("getReferenceTime floors to the previous quarter hour", {
  # Expected values come from floor(minutes/15)*15, computed here rather than
  # copied, and the boundary cases (:14/:15, :29/:30, :44/:45, :59) are all
  # included so that an off-by-one in floor() cannot pass.
  clocks <- c("00:14", "00:15", "10:00", "10:14", "10:15", "10:29", "10:30",
              "10:44", "10:45", "10:59", "23:59")
  for (clock in clocks) {
    minutes  <- floor(totalMinutes(clock) / 15) * 15
    expected <- sprintf("%02d:%02d", minutes %/% 60, minutes %% 60)
    expect_equal(getReferenceTime(clock), expected)
  }
  # Spot-check two of them literally, so the derivation above is anchored to
  # values a human can read off the clock.
  expect_equal(getReferenceTime("10:14"), "10:00")
  expect_equal(getReferenceTime("10:15"), "10:15")
})

test_that("getReferenceTime always returns a zero-padded HH:MM string", {
  # The browser sends the client clock in whatever form toLocaleTimeString()
  # produces, so single-digit hours and a bare HHMM form both have to work.
  for (input in c("7:00", "07:00", "0700", "9:00", "0:00", "19:00", "0830")) {
    result <- getReferenceTime(input)
    expect_type(result, "character")
    # Exactly 5 characters, digits either side of a colon, always padded.
    expect_match(result, "^[0-9]{2}:[0-9]{2}$")
  }
  expect_equal(getReferenceTime("7:00"), "07:00")
  expect_equal(getReferenceTime("0830"), "08:30")
  expect_equal(getReferenceTime("0:00"), "00:00")
})

test_that("getReferenceTime converts uppercase AM/PM to 24-hour time", {
  # Uppercase meridiems survive the gsub() filter and are handed to
  # lubridate's Op ("optional meridiem") order, so these are correct.
  expect_equal(getReferenceTime("1:00 PM"), "13:00")
  expect_equal(getReferenceTime("01:00 PM"), "13:00")
  expect_equal(getReferenceTime("07:15:30 PM"), "19:15")
  expect_equal(getReferenceTime("11:59 PM"), "23:45")
  # The 12 o'clock hours are the classic trap and both come out right.
  expect_equal(getReferenceTime("12:00 AM"), "00:00")
  expect_equal(getReferenceTime("12:59 AM"), "00:45")
  expect_equal(getReferenceTime("12:00 PM"), "12:00")
  expect_equal(getReferenceTime("12:59 PM"), "12:45")
})

test_that("getReferenceTime silently ignores lowercase am/pm", {
  # pinned quirk / KNOWN LIMITATION. The scrubbing regex in R/validateTime.R is
  #     gsub("[^[:digit:]:. APM]", "", time)
  # whose whitelist contains only the UPPERCASE A, P and M. A lowercase "pm" is
  # therefore deleted before lubridate ever sees it, and the time is read as
  # AM: a silent 12-hour error in the reference time, which shifts every clock
  # label in the app. Pinned; fixing the regex (e.g. adding "apm", or
  # toupper()ing first) should flip these expectations to the PM values in the
  # comments.
  expect_equal(getReferenceTime("11:59 pm"), "11:45")   # should be "23:45"
  expect_equal(getReferenceTime("1:00 pm"), "01:00")    # should be "13:00"
  expect_equal(getReferenceTime("12:30 pm"), "12:30")   # correct by accident
  # Lowercase "am" is dropped too; that happens to be harmless except at noon.
  expect_equal(getReferenceTime("08:30 am"), "08:30")   # right answer, wrong way
  expect_equal(getReferenceTime("12:30 am"), "12:30")   # should be "00:30"
})

test_that("getReferenceTime returns NA for input it cannot parse", {
  # Anything lubridate cannot read under the HMSOp/HMOp/HMS/HM orders yields
  # NA, which the caller (app_server.R) feeds straight into the reference-time
  # input. No error is thrown.
  for (input in c("hello", "noon", "", "  ", "abc:def", "25:00", "99:99")) {
    expect_true(is.na(getReferenceTime(input)))
  }
  expect_true(is.na(getReferenceTime(NA)))
  # A bare hour with a meridiem has no minute field, and none of the four
  # lubridate orders is hour-only, so these are NA rather than 08:00 / 20:00.
  expect_true(is.na(getReferenceTime("8 AM")))
  expect_true(is.na(getReferenceTime("8 PM")))
  expect_true(is.na(getReferenceTime("8am")))
  # Ambiguous colon-separated triples do parse, as H:M:S -- 1:2:3 is 62 min,
  # floored to 60. Recorded here so the NA list above is not read as covering
  # every odd input.
  expect_equal(getReferenceTime("1:2:3"), "01:00")
})


# ===========================================================================
# 6. hourMinute
# ===========================================================================

test_that("hourMinute converts clock strings to minutes past midnight", {
  # Expected values are 60*HH + MM by definition; computed here from the parts
  # rather than pasted.
  clocks <- c("00:00", "0:00", "1:0", "8:30", "06:00", "09:00", "10:15",
              "12:3", "23:59")
  for (clock in clocks) {
    expect_equal(hourMinute(clock), totalMinutes(clock))
  }
  # Explicit anchors for the two ends of the day.
  expect_equal(hourMinute("0:00"), 0)
  expect_equal(hourMinute("23:59"), 1439)
  # 24:00 is accepted by lubridate as midnight of the following day, so it
  # comes back as 0 rather than 1440. This matters because validateTime can
  # produce "24:00" (from "0:1440"), and feeding that back through
  # clockTimeToDelta would silently mean midnight, not "one day later".
  expect_equal(hourMinute("24:00"), 0)
})

test_that("hourMinute returns NA for malformed or out-of-range clock strings", {
  # Extends the single "A2:34" case in test-hourMinute.R. Hours above 24 and
  # minutes above 59 are rejected outright (unlike validateTime, which rolls
  # them over), and -- importantly for clockTimeToDelta -- a bare number is NOT
  # a clock time. That is exactly why clockTimeToDelta routes only the elements
  # containing ":" through hourMinute and treats the rest as elapsed minutes.
  for (input in c("25:00", "12:60", "99:00", "830", "0", "7", "", "abc",
                  "12:34:56", ":", "8:")) {
    expect_true(is.na(hourMinute(input)))
  }
  # pinned quirk: the NA that comes back is the unconverted POSIXct NA, not a
  # plain logical/numeric NA, because the function returns px unchanged when
  # the parse fails. Callers must therefore use is.na(), never identical(NA).
  expect_s3_class(hourMinute("abc"), "POSIXct")
  # A leading minus is swallowed by lubridate, so "-1:00" is one hour, not -60.
  expect_equal(hourMinute("-1:00"), 60)
})

test_that("hourMinute is scalar-only and errors on a vector", {
  # `if (!is.na(px))` cannot take a length-2 condition under R >= 4.2. This is
  # why clockTimeToDelta wraps hourMinute in lapply() instead of calling it on
  # the whole column. No message match: R translates the condition-length error.
  expect_error(hourMinute(c("01:00", "02:00")))
})


# ===========================================================================
# 7. clockTimeToDelta / deltaToClockTime round trip
# ===========================================================================

test_that("clockTimeToDelta and deltaToClockTime round-trip across the day", {
  # For every (reference, clock time) pair: the delta must equal the closed
  # form (t - reference) %% 1440 computed by expectedDelta() above, and
  # converting that delta back must recover the original string exactly.
  # The reference list deliberately includes midnight and a late-evening
  # reference so that most of the clock times land on the wrapped branch.
  references <- c("00:00", "06:15", "08:00", "12:00", "18:45", "23:45")
  clocks     <- c("00:00", "00:01", "07:59", "08:00", "08:01", "12:00",
                  "13:37", "23:59")
  for (reference in references) {
    for (clock in clocks) {
      delta <- clockTimeToDelta(reference, clock)
      expect_equal(delta, expectedDelta(reference, clock))
      expect_equal(deltaToClockTime(reference, delta), clock)
    }
  }
})

test_that("clockTimeToDelta wraps clock times before the reference past midnight", {
  # The wrap is what turns "a time earlier in the day" into "nearly a full day
  # later". It is the intended behaviour for an overnight case, but it also
  # means a mistyped target time two hours BEFORE the start silently asks for a
  # 22-hour simulation horizon rather than being rejected -- worth keeping
  # visible, since that is the shape of the complaint in issue #270.
  expect_equal(clockTimeToDelta("09:30", "07:45"), 1335)   # 1440 - 105
  expect_equal(clockTimeToDelta("08:00", "07:59"), 1439)   # one minute early
  expect_equal(clockTimeToDelta("23:45", "00:15"), 30)     # genuine overnight
  # A time equal to the reference is 0, not 1440: the wrap only fires on a
  # strictly negative delta.
  expect_equal(clockTimeToDelta("08:00", "08:00"), 0)
  # Round-tripping the wrapped value still lands on the original clock time.
  expect_equal(deltaToClockTime("09:30", 1335), "07:45")
})

test_that("deltaToClockTime wraps deltas outside a single day", {
  # xHours %% 24 handles both directions; the minute field is recovered before
  # the modulo, so negative deltas come back on the previous day correctly.
  expect_equal(deltaToClockTime("08:00", -30), "07:30")
  expect_equal(deltaToClockTime("00:10", -30), "23:40")   # crosses midnight
  expect_equal(deltaToClockTime("22:00", 180), "01:00")   # crosses midnight
  expect_equal(deltaToClockTime("00:00", 1440), "00:00")  # exactly one day
  expect_equal(deltaToClockTime("08:00", 2880), "08:00")  # exactly two days
  # Vectorised, and empty input stays empty.
  expect_equal(deltaToClockTime("06:15", c(0, 45, 105)),
               c("06:15", "07:00", "08:00"))
  expect_equal(deltaToClockTime("08:00", numeric(0)), character(0))
})


# ===========================================================================
# 8. reference = "none" passthrough
# ===========================================================================

test_that("reference 'none' passes elapsed minutes through in both directions", {
  # In relative-time mode (referenceTime() == "none") the dose table already
  # holds elapsed minutes. clockTimeToDelta still converts any element that
  # LOOKS like a clock time into minutes past midnight, but subtracts nothing;
  # deltaToClockTime does nothing at all beyond as.numeric().
  expect_equal(clockTimeToDelta("none", c("0", "45", "1200")), c(0, 45, 1200))
  expect_equal(clockTimeToDelta("none", c("09:30", "45")), c(570, 45))
  # Negative elapsed minutes are preserved under "none" -- there is no wrap.
  expect_equal(clockTimeToDelta("none", c("-30", "09:30")), c(-30, 570))
  # deltaToClockTime("none", .) is a pure numeric coercion, so character input
  # comes back as numbers (this is the branch flagged by the "TODO check usage
  # of none" comment in test-deltaToClockTime.R).
  expect_equal(deltaToClockTime("none", c("15", "930")), c(15, 930))
  expect_equal(deltaToClockTime("none", c(-30, 0, 1500)), c(-30, 0, 1500))
})


# ===========================================================================
# 9. Mixed vectors, and the failure modes of a bad reference
# ===========================================================================

test_that("clockTimeToDelta handles mixed clock and elapsed-minute elements", {
  # The dose table lets a user mix "09:30" (a clock time) and "45" (elapsed
  # minutes) in the same column. The code splits on grepl(":", x) and maps
  # hourMinute over only the colon-bearing elements, so this is supported by
  # construction -- element order and element count must both be preserved.
  reference <- "08:00"
  x <- c("0", "09:30", "45", "11:05", "1200")
  # Expected: colon elements become (minutes past midnight - 480); the rest are
  # taken as-is.
  expect_equal(clockTimeToDelta(reference, x), c(0, 90, 45, 185, 1200))
  # All-clock and all-elapsed vectors are the degenerate ends of the same path.
  expect_equal(clockTimeToDelta(reference, c("09:30", "11:05")), c(90, 185))
  expect_equal(clockTimeToDelta(reference, c("0", "45")), c(0, 45))
  expect_equal(clockTimeToDelta(reference, character(0)), numeric(0))
  # A single unparseable element on its own degrades to NA rather than erroring
  # (contrast with the mixed case pinned in the next block).
  expect_true(is.na(clockTimeToDelta(reference, "bad:time")))
  expect_equal(clockTimeToDelta(reference, c("09:30", "bad:time")), c(90, NA))
})

test_that("clockTimeToDelta and deltaToClockTime disagree about a bad reference", {
  # pinned quirk / KNOWN LIMITATION.

  # clockTimeToDelta checks is.na(start) and returns NA -- graceful.
  expect_true(is.na(clockTimeToDelta("not a time", "09:00")))

  # deltaToClockTime has no such check: hourMinute returns a POSIXct NA, and
  # the very next line divides it, so it throws instead of returning NA. Any
  # fix should make this return NA and this expectation should change.
  expect_error(deltaToClockTime("not a time", 30))

  # Both throw on a literally NA reference, because `reference == "none"` is
  # then NA and cannot drive an if(). app_server.R can produce exactly this
  # value: getReferenceTime() returns NA when the browser clock string does not
  # parse, and that NA is written straight into input$referenceTime.
  expect_error(clockTimeToDelta(NA, "09:00"))
  expect_error(deltaToClockTime(NA, 30))
})

test_that("clockTimeToDelta errors when a bad time shares a vector with a wrapped time", {
  # pinned quirk / KNOWN LIMITATION -- a genuine crash, not just an odd value.
  #
  #   x[x < 0] <- x[x < 0] + 1440
  #
  # An unparseable element makes x hold an NA, so the logical subscript holds
  # an NA too. R permits an NA subscript in a subassignment only when the
  # replacement value has length 1. With ONE bad element and no wrapped element
  # the replacement is length 1, the NA slot is skipped, and all is well:
  expect_equal(clockTimeToDelta("08:00", c("09:00", "bad:x")), c(60, NA))
  # But add a clock time earlier than the reference and the replacement becomes
  # length 2, which R rejects outright ("NAs are not allowed in subscripted
  # assignments"). One typo plus one overnight dose is enough to take the app
  # down. Message text is not matched because R translates it.
  expect_error(clockTimeToDelta("08:00", c("06:00", "bad:x")))
})


# ===========================================================================
# 10. deltaToClockTime rounding
# ===========================================================================

test_that("deltaToClockTime rounds fractional minutes half-to-even", {
  # The minute field is round(x - 60*floor(x/60), 0), i.e. R's round-half-to-
  # even. Documented rather than pinned: this is base R's documented rule.
  expect_equal(deltaToClockTime("00:00", 0.5), "00:00")   # 0.5 -> 0 (even)
  expect_equal(deltaToClockTime("00:00", 1.5), "00:02")   # 1.5 -> 2 (even)
  expect_equal(deltaToClockTime("00:00", 58.5), "00:58")  # 58.5 -> 58 (even)
  expect_equal(deltaToClockTime("00:00", 59.4), "00:59")
})

test_that("deltaToClockTime can format an impossible :60 minute field", {
  # pinned quirk / KNOWN LIMITATION. xHours is floor()ed BEFORE the minute
  # field is rounded, so when the minute field rounds up to 60 the hour is not
  # carried and the label reads "HH:60". This is reachable from the hover
  # tooltip in app_server.R, which passes round(time, 1) -- a value such as
  # 59.5 -- straight into deltaToClockTime. Fixing the carry should change
  # these to "01:00", "02:00" and "00:00" respectively.
  expect_equal(deltaToClockTime("00:00", 59.5), "00:60")
  expect_equal(deltaToClockTime("00:00", 119.5), "01:60")
  expect_equal(deltaToClockTime("00:00", 1439.5), "23:60")
  # An NA delta formats as the string "NA:NA" rather than propagating NA.
  expect_equal(deltaToClockTime("08:00", NA), "NA:NA")
})
