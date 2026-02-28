library(midiR)

library(tidyverse)
library(RUtils)

midiFile = "Test.mid"

midiFile = outputFile("Climate.mid")

csvMidi <- inputFile("sst-month-GBR-harmonic_minor-2.csv")

notesFile <- read_csv(csvMidi)

notesDF = notesFile
notesDF <- slice_head(notesFile, n=12)

s = 480
notesDF <- tribble(
  ~pitch, ~velocity, ~tickStart, ~tickEnd,
  60, 80, 0, s-100,
  62, 80, s*2, s*3-100,
)

midibytes <- bytesMidiTrack(notesDF,"Test Track","Instrument")


con <- file(description = midiFile, open = "wb")
on.exit(close(con))

writeBin(midibytes,con,endian = "big")
close(con)

