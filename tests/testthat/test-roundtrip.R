library(testthat)
library(tuneR)

# Helper: write a notes DF to MIDI and read it back via tuneR::getMidiNotes
roundtrip_notes <- function(input_df,
                            trackname = "Test",
                            instrument = "Piano") {
  tmpfile <- tempfile(fileext = ".mid")
  on.exit(unlink(tmpfile))
  writeMidiFile(tmpfile, input_df, trackname = trackname, instrument = instrument)
  midi <- readMidi(tmpfile)
  getMidiNotes(midi)
}

# --- Basic round-trip ---

test_that("single note round-trips correctly", {
  input_df <- data.frame(
    pitch = 60,
    velocity = 100,
    tickStart = 0,
    tickEnd = 480
  )
  result <- roundtrip_notes(input_df)

  expect_equal(nrow(result), 1)
  expect_equal(result$note, 60)
  expect_equal(result$velocity, 100)
  expect_equal(result$time, 0)
  expect_equal(result$length, 480)
})

test_that("C major scale round-trips correctly", {
  pitches <- c(60, 62, 64, 65, 67, 69, 71, 72)
  input_df <- data.frame(
    pitch = pitches,
    velocity = rep(100, 8),
    tickStart = seq(0, 3360, by = 480),
    tickEnd = seq(480, 3840, by = 480)
  )
  result <- roundtrip_notes(input_df)

  expect_equal(nrow(result), 8)
  expect_equal(result$note, pitches)
  expect_equal(result$velocity, rep(100, 8))
  expect_equal(result$time, seq(0, 3360, by = 480))
  expect_equal(result$length, rep(480, 8))
})

test_that("varying velocities round-trip correctly", {
  input_df <- data.frame(
    pitch = c(60, 64, 67),
    velocity = c(30, 90, 127),
    tickStart = c(0, 480, 960),
    tickEnd = c(480, 960, 1440)
  )
  result <- roundtrip_notes(input_df)

  expect_equal(result$velocity, c(30, 90, 127))
})

test_that("simultaneous notes (chord) round-trip correctly", {
  input_df <- data.frame(
    pitch = c(60, 64, 67),
    velocity = c(100, 100, 100),
    tickStart = c(0, 0, 0),
    tickEnd = c(480, 480, 480)
  )
  result <- roundtrip_notes(input_df)

  expect_equal(nrow(result), 3)
  expect_equal(sort(result$note), c(60, 64, 67))
  expect_equal(result$length, rep(480, 3))
})

test_that("notes with different durations round-trip correctly", {
  input_df <- data.frame(
    pitch = c(60, 64),
    velocity = c(100, 100),
    tickStart = c(0, 0),
    tickEnd = c(240, 960)
  )
  result <- roundtrip_notes(input_df)

  expect_equal(nrow(result), 2)
  # Sort by pitch to ensure consistent ordering
  result <- result[order(result$note), ]
  expect_equal(result$note, c(60, 64))
  expect_equal(result$length, c(240, 960))
})

test_that("notes starting at non-zero tick round-trip correctly", {
  input_df <- data.frame(
    pitch = 60,
    velocity = 100,
    tickStart = 960,
    tickEnd = 1440
  )
  result <- roundtrip_notes(input_df)

  expect_equal(result$time, 960)
  expect_equal(result$length, 480)
})

test_that("tuneR::readMidi can parse the output file", {
  input_df <- data.frame(
    pitch = 60,
    velocity = 100,
    tickStart = 0,
    tickEnd = 480
  )
  tmpfile <- tempfile(fileext = ".mid")
  on.exit(unlink(tmpfile))
  writeMidiFile(tmpfile, input_df)

  # readMidi should not error — this validates structural correctness
  midi <- readMidi(tmpfile)
  expect_s3_class(midi, "data.frame")

  # Should contain an End of Track event
  expect_true("End of Track" %in% midi$event)
})

test_that("boundary pitches (0 and 127) round-trip correctly", {
  input_df <- data.frame(
    pitch = c(0, 127),
    velocity = c(100, 100),
    tickStart = c(0, 480),
    tickEnd = c(480, 960)
  )
  result <- roundtrip_notes(input_df)

  expect_equal(result$note, c(0, 127))
})

test_that("large delta times encode and decode correctly", {
  # tickStart of 9600 = 20 beats at 480 tpqn, requires multi-byte var-length
  input_df <- data.frame(
    pitch = 60,
    velocity = 100,
    tickStart = 9600,
    tickEnd = 10080
  )
  result <- roundtrip_notes(input_df)

  expect_equal(result$time, 9600)
  expect_equal(result$length, 480)
})
