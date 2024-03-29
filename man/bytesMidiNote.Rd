% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/midiByteEncodings.R
\name{bytesMidiNote}
\alias{bytesMidiNote}
\title{Midi Note Encoding -------}
\usage{
bytesMidiNote(pitch, velocity, deltaTime, on, channel = 1)
}
\arguments{
\item{pitch}{note}

\item{velocity}{velocity 0-127}

\item{deltaTime}{in ticks}

\item{on}{note on or off}

\item{channel}{midi channel 1-16

midiNote(pitch,velocity,deltaTime,on,0)}
}
\description{
returns midi byte stream from midi DF
}
