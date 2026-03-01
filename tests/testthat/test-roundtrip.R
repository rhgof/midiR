library(testthat)
library(tuneR)

# Helper: write a notes DF to MIDI and read it back via tuneR::getMidiNotes
roundtrip_notes <- function(input_df,
                            trackname = "Test",
                            instrument = "Piano",
                            channel = 1, bpm = 120,
                            time_sig = c(4, 4), key_sig = c(0, FALSE),
                            time_division = 480, smpte = NULL) {
  tmpfile <- tempfile(fileext = ".mid")
  on.exit(unlink(tmpfile))
  writeMidiFile(tmpfile, input_df, trackname = trackname, instrument = instrument,
                channel = channel, bpm = bpm, time_sig = time_sig,
                key_sig = key_sig, time_division = time_division, smpte = smpte)
  midi <- readMidi(tmpfile)
  getMidiNotes(midi)
}

# Helper: write a notes DF to MIDI and read back the raw MIDI events
roundtrip_midi <- function(input_df,
                           trackname = "Test",
                           instrument = "Piano",
                           channel = 1, bpm = 120,
                           time_sig = c(4, 4), key_sig = c(0, FALSE),
                           time_division = 480, smpte = NULL) {
  tmpfile <- tempfile(fileext = ".mid")
  on.exit(unlink(tmpfile))
  writeMidiFile(tmpfile, input_df, trackname = trackname, instrument = instrument,
                channel = channel, bpm = bpm, time_sig = time_sig,
                key_sig = key_sig, time_division = time_division, smpte = smpte)
  readMidi(tmpfile)
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

# --- Round-trip tests for parameterized metadata ---

simple_note <- data.frame(
  pitch = 60, velocity = 100, tickStart = 0, tickEnd = 480
)

test_that("custom tempo (90 BPM) round-trips correctly", {
  midi <- roundtrip_midi(simple_note, bpm = 90)
  # Look for Set Tempo event
  tempo_rows <- midi[midi$event == "Set Tempo", ]
  expect_equal(nrow(tempo_rows), 1)
  # 90 BPM = 666667 microseconds
  expect_true(grepl("666667", tempo_rows$parameterMetaSystem[1]))
})

test_that("custom time signature (3/4) round-trips correctly", {
  midi <- roundtrip_midi(simple_note, time_sig = c(3, 4))
  ts_rows <- midi[midi$event == "Time Signature", ]
  expect_equal(nrow(ts_rows), 1)
})

test_that("custom key signature (A minor) round-trips correctly", {
  midi <- roundtrip_midi(simple_note, key_sig = c(0, TRUE))
  ks_rows <- midi[midi$event == "Key Signature", ]
  expect_equal(nrow(ks_rows), 1)
})

test_that("custom channel (5) round-trips correctly", {
  result <- roundtrip_notes(simple_note, channel = 5)
  expect_equal(nrow(result), 1)
  # tuneR reports channels 0-indexed (0-15), our API uses 1-indexed (1-16)
  expect_equal(result$channel, 4)
})

test_that("default smpte = NULL produces no SMPTE event", {
  midi <- roundtrip_midi(simple_note, smpte = NULL)
  smpte_rows <- midi[midi$event == "SMPTE Offset", ]
  expect_equal(nrow(smpte_rows), 0)
})

test_that("notes still round-trip correctly with non-default metadata", {
  pitches <- c(60, 64, 67)
  input_df <- data.frame(
    pitch = pitches,
    velocity = c(80, 100, 120),
    tickStart = c(0, 480, 960),
    tickEnd = c(480, 960, 1440)
  )
  result <- roundtrip_notes(input_df, channel = 3, bpm = 90,
                            time_sig = c(3, 4), key_sig = c(-1, FALSE))
  expect_equal(nrow(result), 3)
  expect_equal(result$note, pitches)
  # tuneR reports channels 0-indexed
  expect_equal(result$channel, rep(2, 3))
})
