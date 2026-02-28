# bytesMidiTrack ------
#' Generate bytes for one midi Track
#' @param notesDF data frame of pitch, velocity, duration and beat
#' @param trackname  char string
#' @param instrument  char string
#' @description
#' Return bytes from a notes data frame constructing the track header, notes, end of track messages
#' @export bytesMidiTrack

bytesMidiTrack <- function(notesDF, trackname, instrument ) {

  validateNotesDF(notesDF)
  midiNotesDF <- toDeltaTime(notesDF)
  trkNotes <- bytesMidiNotes(midiNotesDF)

  # all bytes in track chunk
  trkAll <- c(
    midiTrkSetup(trackname, instrument),
    trkNotes,
#    c(as.raw(0x81),as.raw(0x99),as.raw(0x60)),  # I do not know what this does
    midiTrkEnd()
  )

  # build the file and track header and set lengths
  midibytes <- c(
    midiHeader(),
    midiTrkHeader(trackLength=length(trkAll)),
    trkAll
  )

  return(midibytes)
}


# Convert to Delta Time ---------
#' converts midi note DF to delta time based note DF
#' @description Changes notes DF that encodes notes as pitch, velocity, start end tick to
#' DF that encodes pitch, velocity, delta time, and note-on and note-off.
#' Also adds raw midi bytes to the data frame for later generate of midi messages
#' @param midiNotesDF DF of pitch velocity start end ticks
#' @returns DF that encodes pitch, velocity, delta time, and note-on and note-off and raw messages
#' @import dplyr
#' @importFrom rlang .data

toDeltaTime <- function(midiNotesDF) {
  notesOn <- midiNotesDF %>%
    mutate(eventTick = .data$tickStart) %>%
    mutate(msg = "NoteOn")

  notesOff <- midiNotesDF %>%
    mutate(eventTick = .data$tickEnd) %>%
    mutate(velocity = 0) %>%  #decrease the velocity for midi-off
    mutate(msg = "NoteOff")

  midiNotes <-bind_rows(notesOn,notesOff) %>%
    select(-c(.data$tickStart,.data$tickEnd)) %>%
    arrange(.data$eventTick) %>%

    mutate(deltaTime = .data$eventTick-dplyr::lag(.data$eventTick,default=0) )

  # not the best way to do this but good for debugging to see what was assigned - see bytesMidiNotes() below
  midiNotes <- midiNotes %>%
    rowwise() %>%
    mutate(midiRaw = list(bytesMidiNote(.data$pitch,.data$velocity,.data$deltaTime,.data$msg == "NoteOn")))

  return(midiNotes)
}


# byteStream for Notes  --------
#' Generate midit byte stream for notes DF
#' @description simply concatenates all bytes in data frame.
#' Assumes they are in order of delta time
#' @param midiNotesDF processed notes DF with midi bytes in each row

bytesMidiNotes <- function(midiNotesDF) {
  bytes = raw(0)
  for (i in 1:nrow(midiNotesDF)) {
    event <- midiNotesDF[i,]
    bytes <- c(bytes,event$midiRaw[[1]])
  }
  return(bytes)
}

