library(testthat)

# Helper: minimal valid notes DF
valid_notes <- function() {
  data.frame(
    pitch = c(60, 64, 67),
    velocity = c(100, 100, 100),
    tickStart = c(0, 480, 960),
    tickEnd = c(480, 960, 1440)
  )
}

# --- Valid input passes silently ---

test_that("valid notes DF passes validation", {
  df <- valid_notes()
  expect_invisible(validateNotesDF(df))
  expect_identical(validateNotesDF(df), df)
})

test_that("boundary values (0 and 127) are accepted", {
  df <- data.frame(
    pitch = c(0, 127),
    velocity = c(0, 127),
    tickStart = c(0, 100),
    tickEnd = c(100, 200)
  )
  expect_invisible(validateNotesDF(df))
})

# --- Not a data frame ---

test_that("non-data-frame input errors", {
  expect_error(validateNotesDF(list(a = 1)), "must be a data frame")
  expect_error(validateNotesDF(c(1, 2, 3)), "must be a data frame")
  expect_error(validateNotesDF("hello"), "must be a data frame")
  expect_error(validateNotesDF(NULL), "must be a data frame")
})

# --- Empty data frame ---

test_that("empty data frame errors", {
  df <- data.frame(
    pitch = numeric(0),
    velocity = numeric(0),
    tickStart = numeric(0),
    tickEnd = numeric(0)
  )
  expect_error(validateNotesDF(df), "at least one row")
})

# --- Missing columns ---

test_that("missing single column errors with column name", {
  df <- valid_notes()
  df$pitch <- NULL
  expect_error(validateNotesDF(df), "pitch")
})

test_that("missing multiple columns errors with all names", {
  df <- data.frame(pitch = 60, velocity = 100)
  expect_error(validateNotesDF(df), "tickStart")
  expect_error(validateNotesDF(df), "tickEnd")
})

# --- Non-numeric columns ---

test_that("character column errors", {
  df <- valid_notes()
  df$pitch <- as.character(df$pitch)
  expect_error(validateNotesDF(df), "must be numeric")
  expect_error(validateNotesDF(df), "pitch")
})

test_that("logical column errors", {
  df <- valid_notes()
  df$velocity <- c(TRUE, FALSE, TRUE)
  expect_error(validateNotesDF(df), "must be numeric")
})

# --- NA values ---

test_that("NA in pitch errors with row numbers", {
  df <- valid_notes()
  df$pitch[2] <- NA
  expect_error(validateNotesDF(df), "NA")
  expect_error(validateNotesDF(df), "pitch")
})

test_that("NA in velocity errors", {
  df <- valid_notes()
  df$velocity[1] <- NA
  expect_error(validateNotesDF(df), "NA")
})

test_that("NA in tickStart errors", {
  df <- valid_notes()
  df$tickStart[3] <- NA
  expect_error(validateNotesDF(df), "NA")
  expect_error(validateNotesDF(df), "tickStart")
})

test_that("NA in tickEnd errors", {
  df <- valid_notes()
  df$tickEnd[1] <- NA
  expect_error(validateNotesDF(df), "NA")
})

# --- Pitch out of range ---

test_that("negative pitch errors", {
  df <- valid_notes()
  df$pitch[1] <- -1
  expect_error(validateNotesDF(df), "pitch")
  expect_error(validateNotesDF(df), "0-127")
})

test_that("pitch > 127 errors", {
  df <- valid_notes()
  df$pitch[1] <- 128
  expect_error(validateNotesDF(df), "pitch")
})

test_that("fractional pitch errors", {
  df <- valid_notes()
  df$pitch[1] <- 60.5
  expect_error(validateNotesDF(df), "pitch")
  expect_error(validateNotesDF(df), "integers")
})

# --- Velocity out of range ---

test_that("negative velocity errors", {
  df <- valid_notes()
  df$velocity[1] <- -1
  expect_error(validateNotesDF(df), "velocity")
})

test_that("velocity > 127 errors", {
  df <- valid_notes()
  df$velocity[1] <- 128
  expect_error(validateNotesDF(df), "velocity")
})

test_that("fractional velocity errors", {
  df <- valid_notes()
  df$velocity[1] <- 100.5
  expect_error(validateNotesDF(df), "velocity")
})

# --- tickStart < 0 ---

test_that("negative tickStart errors", {
  df <- valid_notes()
  df$tickStart[1] <- -10
  expect_error(validateNotesDF(df), "tickStart")
  expect_error(validateNotesDF(df), ">= 0")
})

# --- tickEnd <= tickStart ---

test_that("tickEnd equal to tickStart errors", {
  df <- valid_notes()
  df$tickEnd[1] <- df$tickStart[1]
  expect_error(validateNotesDF(df), "tickEnd")
  expect_error(validateNotesDF(df), "greater than")
})

test_that("tickEnd less than tickStart errors", {
  df <- valid_notes()
  df$tickEnd[2] <- df$tickStart[2] - 1
  expect_error(validateNotesDF(df), "tickEnd")
})

# --- Row number reporting ---

test_that("error messages include row numbers", {
  df <- valid_notes()
  df$pitch[3] <- 200
  expect_error(validateNotesDF(df), "3")
})

test_that("multiple bad rows are reported (capped at 5)", {
  df <- data.frame(
    pitch = rep(200, 7),
    velocity = rep(100, 7),
    tickStart = seq(0, 600, 100),
    tickEnd = seq(100, 700, 100)
  )
  expect_error(validateNotesDF(df), "and 2 more")
})
