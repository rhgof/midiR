#' Write Midi File
#' #' Generate bytes for one midi Track
#' @param notesMidiDF data frame of pitch, velocity, duration and beat
#' @param midiFileName the file name to write
#' @param trackname name of track - character string
#' @param instrument name of instrument - character string
#' @description
#' write midiDF as midi rile
#' @export writeMidiFile
#' @importFrom stringr str_ends str_c

writeMidiFile <- function(midiFileName, notesMidiDF, trackname="Track", instrument = "Instrument") {

  validateNotesDF(notesMidiDF)
  # encode track  as byte stream
  midibytes <- bytesMidiTrack(notesMidiDF,trackname,instrument)
  midiExt = ".mid"

  if (!str_ends(midiFileName,midiExt)) {
    midiFileName = str_c(midiFileName,midiExt)
  }

  con <- file(description = midiFileName, open = "wb")
  on.exit(close(con))

  writeBin(midibytes,con,endian = "big")
  #close(con)

}
