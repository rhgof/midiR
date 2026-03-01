# Midi Constants

MTHD = "MThd" # 0x4d546864
MTRK = "MTrk" # 0x4d54726b

META_ESC = as.raw(0xFF)
KEY_SIG = as.raw(0x59)
TIME_SIG = as.raw(0x58)
TEMPO = as.raw(0x51)
SMPTE = as.raw(0x54)
CHANNEL= as.raw(0x20)
TRK_NAME = as.raw(0x03)
INST_NAME = as.raw(0x04)

#' Generate midiHeader
#' \code{<Header Chunk> = <chunk type><length><format><ntrks><division>}
#' These are NOT coded as var length integers

midiHeader <- function(time_division = 480) {
  if (!is.numeric(time_division) || length(time_division) != 1 ||
      time_division != as.integer(time_division) ||
      time_division < 1 || time_division > 32767) {
    stop("time_division must be a positive integer up to 32767")
  }

  bytes = raw(0)
  # Type
  bytes = c(bytes, bytesChar(MTHD))

  headerLength = 14
  bytes = c(bytes, bytesInt(headerLength-8))

  # Format
  midiType = 0
  bytes = c(bytes, bytesShort(midiType) )

  # nTracks
  trackCount = 1
  bytes = c(bytes, bytesShort(trackCount) )

  # Division
  bytes = c(bytes, bytesShort(time_division) )

  return(bytes)
}

# Midi Track Header ----
#' Generate Midi Track Header
#' @param trackLength length of track in bytes
#' NB track length needs to be calculated once all the midi notes have been coded

midiTrkHeader <- function(trackLength=100) {
  bytes = raw(0)
  bytes = c(bytes, bytesChar(MTRK))
  bytes = c(bytes, bytesInt(trackLength))
  return(bytes)
}

midiTrkSetup <- function(trackname = "Test Track", instrument = "Instrument",
                         channel = 1, bpm = 120,
                         time_sig = c(4, 4), key_sig = c(0, FALSE),
                         smpte = NULL) {
  # Delta time before first event
  bytes = as.raw(0x00)
  # Channel
  bytes = c(bytes, bytesChannel(channel))
  bytes = c(bytes, as.raw(0x00))

  # Track Name
  bytes = c(bytes, bytesMetaMessage(TRK_NAME, trackname))
  bytes = c(bytes, as.raw(0x00))

  # Instrument Name
  bytes = c(bytes, bytesMetaMessage(INST_NAME, instrument))
  bytes = c(bytes, as.raw(0x00))
  # Time Signature
  bytes = c(bytes, bytesTimeSig(time_sig[1], time_sig[2]))
  bytes = c(bytes, as.raw(0x00))
  # Key Signature
  bytes = c(bytes, bytesKeySig(key_sig[1], as.logical(key_sig[2])))
  bytes = c(bytes, as.raw(0x00))
  # SMPTE (optional)
  if (!is.null(smpte)) {
    bytes = c(bytes, bytesSMPTEOffset())
    bytes = c(bytes, as.raw(0x00))
  }
  # Tempo
  bytes = c(bytes, bytesTempo(bpm))

  return(bytes)
}

# Track End ------
midiTrkEnd <- function() {
  bytes = c(as.raw(0x00),META_ESC,as.raw(0x2f),as.raw(0x00))
  return(bytes)
}


# Meta Message -------
# Only works for simple text based messages

bytesMetaMessage <- function(msgType,msg) {
  metaByte = META_ESC
  msgBytes = charToRaw(msg)
  msgLen = length(msgBytes)
  bytes = c(metaByte,msgType,bytesVarLenInt(msgLen),msgBytes)
  return(bytes)
}


# Midi Note Encoding -------
# assumes sequenced and so delta time is correct

# pitch = 48
# velocity = 100
# deltaTime = 5760
# on = TRUE
#
#' Midi Note Encoding -------
#' @description returns midi byte stream from midi DF
#' @param pitch note
#' @param velocity velocity 0-127
#' @param deltaTime in ticks
#' @param on note on or off
#' @param channel midi channel 1-16
#'
#' midiNote(pitch,velocity,deltaTime,on,0)

bytesMidiNote <- function(pitch,velocity,deltaTime,on,channel = 1) {
  if (channel > 16 || channel < 1) {
    warning(simpleWarning("Channel > 16 - setting channel to midi channel 1",
                          call="bytesMidiNote()"))
    channel = 1
  }
  channel = channel - 1 # midi 1-16 maps to raw 0-15

  M_NoteOn = as.raw(0x90)
  M_NoteOff = as.raw(0x80)

  msg  = dplyr::case_when(
    on == TRUE ~ M_NoteOn,
    on == FALSE ~ M_NoteOff,
    TRUE ~ M_NoteOff
  )

  bytes = c(
    bytesVarLenInt(deltaTime),
    msg | as.raw(channel),  #Note on/off OR channel
    as.raw(pitch),
    as.raw(velocity)
  )
  return(bytes)
}


# Set Time Signature FF 58 04 nn dd cc bb -----
# The time signature is expressed as four numbers. nn and dd represent the numerator and denominator of the time signature as it would be notated.
# The denominator is a negative power of two: 2 represents a quarter-note, 3 represents an eighth-note, etc.
# The cc parameter expresses the number of MIDI clocks in a metronome click.
# The bb parameter expresses the number of notated 32nd-notes in a MIDI quarter-note (24 MIDI clocks).

# ff 58 04 04  02 18 08 00

bytesTimeSig <- function(numerator = 4, denominator = 4) {
  if (!is.numeric(numerator) || length(numerator) != 1 ||
      numerator != as.integer(numerator) || numerator < 1 || numerator > 32) {
    stop("numerator must be an integer between 1 and 32")
  }
  valid_denoms = c(1, 2, 4, 8, 16, 32)
  if (!is.numeric(denominator) || length(denominator) != 1 ||
      !(denominator %in% valid_denoms)) {
    stop("denominator must be a power of 2: 1, 2, 4, 8, 16, or 32")
  }

  denom_log2 = as.integer(log2(denominator))

  bytes = c(
    META_ESC,
    TIME_SIG,
    as.raw(0x04),
    as.raw(numerator),
    as.raw(denom_log2),
    as.raw(0x18),
    as.raw(0x08)
  )

  return(bytes)
}

# Set Key Signature FF 59 02 sf mi -----
#sf = -7: 7 flats
#sf = -1: 1 flat
#sf = 0: key of C
#sf = 1: 1 sharp
#sf = 7: 7 sharps
#mi = 0: major key
#mi = 1: minor key

# ff 59 02 00 00 00 # C Major

bytesKeySig <- function(sharps_flats = 0, minor = FALSE) {
  if (!is.numeric(sharps_flats) || length(sharps_flats) != 1 ||
      sharps_flats != as.integer(sharps_flats) ||
      sharps_flats < -7 || sharps_flats > 7) {
    stop("sharps_flats must be an integer between -7 and 7")
  }
  if (!is.logical(minor) || length(minor) != 1) {
    stop("minor must be TRUE or FALSE")
  }

  # Two's complement for negative values: as.raw() on 256 + val
  sf_byte = if (sharps_flats >= 0) as.raw(sharps_flats) else as.raw(256L + sharps_flats)
  mi_byte = as.raw(as.integer(minor))

  bytes = c(
    META_ESC,
    KEY_SIG,
    as.raw(0x02),
    sf_byte,
    mi_byte
  )
  return(bytes)
}

# Set Tempo FF 51 03 tttttt Set Tempo  ------------------
# (in microseconds per MIDI quarter-note)
# ff 51 03 07 a1 20
# Is 120bpm
bytesTempo <- function(bpm = 120) {
  if (!is.numeric(bpm) || length(bpm) != 1 || bpm <= 0) {
    stop("bpm must be a single positive number")
  }
  if (bpm < 20 || bpm > 300) {
    warning("bpm ", bpm, " is outside typical range 20-300")
  }

  microseconds = round(60000000 / bpm)
  byte1 = as.raw((microseconds %/% 65536) %% 256)
  byte2 = as.raw((microseconds %/% 256) %% 256)
  byte3 = as.raw(microseconds %% 256)

  bytes = c(
    META_ESC,
    TEMPO,
    as.raw(0x03),
    byte1,
    byte2,
    byte3
  )
  return(bytes)
}


#ff 54  05 21 00 00 00 00 00 -----
bytesSMPTEOffset <- function() {
  bytes = c(
    META_ESC,
    SMPTE,
    as.raw(0x05),
    as.raw(0x21),
    as.raw(0x00),
    as.raw(0x00),
    as.raw(0x00),
    as.raw(0x00)
  )
  return(bytes)
}



# FF 20 01 cc MIDI Channel Prefix -------
bytesChannel <- function(channel = 1) {
  if (channel > 16 || channel < 1) {
    warning(simpleWarning("Channel > 16 - setting channel to midi channel 1",
                          call="bytesChannel()"))
    channel = 1
  }
  channel = channel - 1 # midi 1-16 maps to raw 0-15
  bytes = c(
    META_ESC,
    CHANNEL,
    as.raw(0x01),
    as.raw(channel)
  )
  return(bytes)
}

