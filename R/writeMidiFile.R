#' Write Midi File
#' @param midiFileName the file name to write
#' @param notesMidiDF data frame of pitch, velocity, duration and beat
#' @param trackname name of track - character string
#' @param instrument name of instrument - character string
#' @param channel MIDI channel 1-16 (default 1)
#' @param bpm tempo in beats per minute (default 120)
#' @param time_sig time signature as c(numerator, denominator), e.g. c(3, 4) for 3/4 (default c(4, 4))
#' @param key_sig key signature as c(sharps_flats, minor), e.g. c(2, FALSE) for D major (default c(0, FALSE))
#' @param time_division ticks per quarter note (default 480)
#' @param smpte SMPTE offset — NULL to omit (default NULL)
#' @description
#' Write a notes data frame as a MIDI file with configurable metadata.
#' @export writeMidiFile
#' @importFrom stringr str_ends str_c

writeMidiFile <- function(midiFileName, notesMidiDF,
                          trackname = "Track", instrument = "Instrument",
                          channel = 1, bpm = 120,
                          time_sig = c(4, 4), key_sig = c(0, FALSE),
                          time_division = 480, smpte = NULL) {

  validateNotesDF(notesMidiDF)
  # encode track as byte stream
  midibytes <- bytesMidiTrack(notesMidiDF, trackname, instrument,
                              channel = channel, bpm = bpm,
                              time_sig = time_sig, key_sig = key_sig,
                              time_division = time_division, smpte = smpte)
  midiExt = ".mid"

  if (!str_ends(midiFileName, midiExt)) {
    midiFileName = str_c(midiFileName, midiExt)
  }

  con <- file(description = midiFileName, open = "wb")
  on.exit(close(con))

  writeBin(midibytes, con, endian = "big")

}
