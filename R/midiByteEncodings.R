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

midiHeader <- function() {
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

  # Division - Logic Default
  timeDivision = 480
  bytes = c(bytes, bytesShort(timeDivision) )

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

midiTrkSetup <- function(trackname="Test Track", instrument = "Instrument") {
  # Delta time before first event
  bytes = as.raw(0x00)
  # Channel
  bytes = c(bytes, bytesChannel())

  # Track Name Name
  bytes = c(bytes,bytesMetaMessage(TRK_NAME,trackname))
  bytes = c(bytes,as.raw(0x00))

  # Instrument Name
  bytes = c(bytes,bytesMetaMessage(INST_NAME,instrument))
  bytes = c(bytes,as.raw(0x00))
  # Time Signature
  bytes = c(bytes,bytesTimeSig())
  # Key Signature
  bytes = c(bytes,bytesKeySig())
  # SMPTE
  bytes = c(bytes,bytesSMPTEOffset())
  # tempo
  bytes = c(bytes,bytesTempo())

  return (bytes)
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

bytesTimeSig <- function() {
  T_QTR = as.raw(02) #2^-2
  bytes = c(
    META_ESC,
    TIME_SIG,
    as.raw(0x04),
    as.raw(0x04), # 4 notes
    T_QTR,
    as.raw(0x18),
    as.raw(0x08),
    as.raw(0x00)
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

bytesKeySig <- function() {
  bytes = c(
    META_ESC,
    KEY_SIG,
    as.raw(0x02),
    as.raw(0x00), # key of C
    as.raw(0x00), # major
    as.raw(0x00)
  )
  return(bytes)
}

# Set Tempo FF 51 03 tttttt Set Tempo  ------------------
# (in microseconds per MIDI quarter-note)
# ff 51 03 07 a1 20
# Is 120bpm
bytesTempo <- function() {
  bytes = c(
    META_ESC,
    TEMPO,
    as.raw(0x03),
    as.raw(0x07),
    as.raw(0xa1),
    as.raw(0x20)
    #    as.raw(0x00)
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
    as.raw(channel),
    as.raw(0x00)
  )
  return(bytes)
}

