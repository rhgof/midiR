library(testthat)

# --- Meta event byte-level encoding tests ---
# Each meta event should produce exactly the bytes specified
# by the MIDI 1.0 spec, with NO trailing delta-time padding.

test_that("bytesChannel() encodes FF 20 01 cc with no trailing 0x00", {
  # Channel 1 → raw channel 0x00
  expect_equal(bytesChannel(1), as.raw(c(0xFF, 0x20, 0x01, 0x00)))
  expect_equal(length(bytesChannel(1)), 4)

  # Channel 10 → raw channel 0x09
  expect_equal(bytesChannel(10), as.raw(c(0xFF, 0x20, 0x01, 0x09)))

  # Channel 16 → raw channel 0x0F
  expect_equal(bytesChannel(16), as.raw(c(0xFF, 0x20, 0x01, 0x0F)))
})

test_that("bytesTimeSig() encodes FF 58 04 nn dd cc bb with no trailing 0x00", {
  result <- bytesTimeSig()
  # 4/4 time: FF 58 04 04 02 18 08
  expect_equal(result, as.raw(c(0xFF, 0x58, 0x04, 0x04, 0x02, 0x18, 0x08)))
  expect_equal(length(result), 7)
})

test_that("bytesKeySig() encodes FF 59 02 sf mi with no trailing 0x00", {
  result <- bytesKeySig()
  # C major: FF 59 02 00 00
  expect_equal(result, as.raw(c(0xFF, 0x59, 0x02, 0x00, 0x00)))
  expect_equal(length(result), 5)
})

test_that("bytesSMPTEOffset() encodes FF 54 05 hr mn se fr ff with no trailing 0x00", {
  result <- bytesSMPTEOffset()
  # FF 54 05 21 00 00 00 00
  expect_equal(result, as.raw(c(0xFF, 0x54, 0x05, 0x21, 0x00, 0x00, 0x00, 0x00)))
  expect_equal(length(result), 8)
})

test_that("bytesTempo() encodes FF 51 03 tt tt tt with no trailing 0x00", {
  result <- bytesTempo()
  # 120 BPM = 500000 microseconds = 0x07A120
  expect_equal(result, as.raw(c(0xFF, 0x51, 0x03, 0x07, 0xA1, 0x20)))
  expect_equal(length(result), 6)
})

test_that("midiTrkEnd() encodes 00 FF 2F 00", {
  result <- midiTrkEnd()
  # delta time 0 + end of track meta event
  expect_equal(result, as.raw(c(0x00, 0xFF, 0x2F, 0x00)))
  expect_equal(length(result), 4)
})

test_that("midiTrkSetup() has consistent delta-time pattern", {
  result <- midiTrkSetup("T", "I")

  # Every meta event (FF xx ...) should be preceded by a delta time byte.
  # Find all FF bytes and verify the byte before each is a valid delta time.
  ff_positions <- which(result == as.raw(0xFF))

  for (pos in ff_positions) {
    if (pos > 1) {
      # The byte before FF should be 0x00 (delta time)
      # or part of data (which we can verify by checking it's not a MIDI status byte)
      prev <- as.integer(result[pos - 1])
      # In our track setup, all delta times are 0x00
      expect_equal(prev, 0L,
        info = sprintf("Byte before FF at position %d should be delta time 0x00, got 0x%02x", pos, prev))
    }
  }
})

# --- Parameterized bytesTempo() tests ---

test_that("bytesTempo() encodes 60 BPM correctly", {
  result <- bytesTempo(60)
  # 60 BPM = 1,000,000 microseconds = 0x0F4240
  expect_equal(result, as.raw(c(0xFF, 0x51, 0x03, 0x0F, 0x42, 0x40)))
})

test_that("bytesTempo() encodes 240 BPM correctly", {
  result <- bytesTempo(240)
  # 240 BPM = 250,000 microseconds = 0x03D090
  expect_equal(result, as.raw(c(0xFF, 0x51, 0x03, 0x03, 0xD0, 0x90)))
})

test_that("bytesTempo() validates input", {
  expect_error(bytesTempo(0), "positive number")
  expect_error(bytesTempo(-10), "positive number")
  expect_error(bytesTempo("fast"), "positive number")
  expect_warning(bytesTempo(10), "outside typical range")
  expect_warning(bytesTempo(400), "outside typical range")
})

# --- Parameterized bytesTimeSig() tests ---

test_that("bytesTimeSig() encodes 3/4 correctly", {
  result <- bytesTimeSig(3, 4)
  # 3/4: FF 58 04 03 02 18 08
  expect_equal(result, as.raw(c(0xFF, 0x58, 0x04, 0x03, 0x02, 0x18, 0x08)))
})

test_that("bytesTimeSig() encodes 6/8 correctly", {
  result <- bytesTimeSig(6, 8)
  # 6/8: FF 58 04 06 03 18 08
  expect_equal(result, as.raw(c(0xFF, 0x58, 0x04, 0x06, 0x03, 0x18, 0x08)))
})

test_that("bytesTimeSig() encodes 2/2 correctly", {
  result <- bytesTimeSig(2, 2)
  # 2/2: FF 58 04 02 01 18 08
  expect_equal(result, as.raw(c(0xFF, 0x58, 0x04, 0x02, 0x01, 0x18, 0x08)))
})

test_that("bytesTimeSig() validates input", {
  expect_error(bytesTimeSig(0, 4), "numerator must be an integer between 1 and 32")
  expect_error(bytesTimeSig(33, 4), "numerator must be an integer between 1 and 32")
  expect_error(bytesTimeSig(4, 3), "denominator must be a power of 2")
  expect_error(bytesTimeSig(4, 5), "denominator must be a power of 2")
  expect_error(bytesTimeSig(4, 0), "denominator must be a power of 2")
})

# --- Parameterized bytesKeySig() tests ---

test_that("bytesKeySig() encodes D major (2 sharps) correctly", {
  result <- bytesKeySig(2, FALSE)
  # FF 59 02 02 00
  expect_equal(result, as.raw(c(0xFF, 0x59, 0x02, 0x02, 0x00)))
})

test_that("bytesKeySig() encodes C minor (-3 flats) correctly", {
  result <- bytesKeySig(-3, TRUE)
  # FF 59 02 FD 01 (-3 in two's complement = 0xFD)
  expect_equal(result, as.raw(c(0xFF, 0x59, 0x02, 0xFD, 0x01)))
})

test_that("bytesKeySig() encodes F major (-1 flat) correctly", {
  result <- bytesKeySig(-1, FALSE)
  # FF 59 02 FF 00 (-1 in two's complement = 0xFF)
  expect_equal(result, as.raw(c(0xFF, 0x59, 0x02, 0xFF, 0x00)))
})

test_that("bytesKeySig() validates input", {
  expect_error(bytesKeySig(8, FALSE), "sharps_flats must be an integer between -7 and 7")
  expect_error(bytesKeySig(-8, FALSE), "sharps_flats must be an integer between -7 and 7")
  expect_error(bytesKeySig(0, "yes"), "minor must be TRUE or FALSE")
})

# --- midiHeader() parameterized tests ---

test_that("midiHeader() uses custom time_division", {
  result <- midiHeader(960)
  # time_division is the last 2 bytes of the 14-byte header
  expect_equal(result[13:14], bytesShort(960))
})

test_that("midiHeader() validates time_division", {
  expect_error(midiHeader(0), "positive integer")
  expect_error(midiHeader(40000), "positive integer up to 32767")
  expect_error(midiHeader(-1), "positive integer")
})

# --- midiTrkSetup() with smpte = NULL omits SMPTE ---

test_that("midiTrkSetup() omits SMPTE when smpte = NULL", {
  result_no_smpte <- midiTrkSetup("T", "I", smpte = NULL)
  result_with_smpte <- midiTrkSetup("T", "I", smpte = TRUE)

  # SMPTE event is FF 54 — should NOT appear when smpte = NULL
  smpte_positions_no <- which(result_no_smpte == as.raw(0x54))
  has_smpte_no <- any(sapply(smpte_positions_no, function(pos) {
    pos > 1 && result_no_smpte[pos - 1] == as.raw(0xFF)
  }))
  expect_false(has_smpte_no)

  # Should appear when smpte is not NULL
  smpte_positions_yes <- which(result_with_smpte == as.raw(0x54))
  has_smpte_yes <- any(sapply(smpte_positions_yes, function(pos) {
    pos > 1 && result_with_smpte[pos - 1] == as.raw(0xFF)
  }))
  expect_true(has_smpte_yes)
})
