#' Validate a Notes Data Frame
#'
#' @param notesDF a data frame with columns: pitch, velocity, tickStart, tickEnd
#' @description
#' Validates that a notes data frame has the correct structure and values
#' for MIDI conversion. Stops with an informative error if validation fails.
#' @returns notesDF invisibly if valid (pipe-friendly)
#' @keywords internal

validateNotesDF <- function(notesDF) {
  # 1. Must be a data frame

if (!is.data.frame(notesDF)) {
    stop("notesDF must be a data frame, got ", class(notesDF)[[1]], call. = FALSE)
  }

  # 2. Must be non-empty
  if (nrow(notesDF) == 0) {
    stop("notesDF must have at least one row", call. = FALSE)
  }

  # 3. Required columns
  required_cols <- c("pitch", "velocity", "tickStart", "tickEnd")
  missing_cols <- setdiff(required_cols, names(notesDF))
  if (length(missing_cols) > 0) {
    stop(
      "notesDF is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # 4. All required columns must be numeric
  for (col in required_cols) {
    if (!is.numeric(notesDF[[col]])) {
      stop(
        "Column '", col, "' must be numeric, got ", class(notesDF[[col]])[[1]],
        call. = FALSE
      )
    }
  }

  # 5. No NAs in required columns
  for (col in required_cols) {
    na_rows <- which(is.na(notesDF[[col]]))
    if (length(na_rows) > 0) {
      shown <- head(na_rows, 5)
      stop(
        "Column '", col, "' contains NA values in rows: ",
        paste(shown, collapse = ", "),
        if (length(na_rows) > 5) paste0(" (and ", length(na_rows) - 5, " more)"),
        call. = FALSE
      )
    }
  }

  # 6. Pitch must be integer values in 0-127
  bad_pitch <- which(
    notesDF$pitch < 0 | notesDF$pitch > 127 | notesDF$pitch != floor(notesDF$pitch)
  )
  if (length(bad_pitch) > 0) {
    shown <- head(bad_pitch, 5)
    stop(
      "Column 'pitch' must contain integers 0-127. ",
      "Invalid values in rows: ", paste(shown, collapse = ", "),
      if (length(bad_pitch) > 5) paste0(" (and ", length(bad_pitch) - 5, " more)"),
      call. = FALSE
    )
  }

  # 7. Velocity must be integer values in 0-127
  bad_velocity <- which(
    notesDF$velocity < 0 | notesDF$velocity > 127 |
      notesDF$velocity != floor(notesDF$velocity)
  )
  if (length(bad_velocity) > 0) {
    shown <- head(bad_velocity, 5)
    stop(
      "Column 'velocity' must contain integers 0-127. ",
      "Invalid values in rows: ", paste(shown, collapse = ", "),
      if (length(bad_velocity) > 5) paste0(" (and ", length(bad_velocity) - 5, " more)"),
      call. = FALSE
    )
  }

  # 8. tickStart must be >= 0
  bad_start <- which(notesDF$tickStart < 0)
  if (length(bad_start) > 0) {
    shown <- head(bad_start, 5)
    stop(
      "Column 'tickStart' must be >= 0. ",
      "Invalid values in rows: ", paste(shown, collapse = ", "),
      if (length(bad_start) > 5) paste0(" (and ", length(bad_start) - 5, " more)"),
      call. = FALSE
    )
  }

  # 9. tickEnd must be > tickStart
  bad_duration <- which(notesDF$tickEnd <= notesDF$tickStart)
  if (length(bad_duration) > 0) {
    shown <- head(bad_duration, 5)
    stop(
      "Column 'tickEnd' must be greater than 'tickStart'. ",
      "Invalid values in rows: ", paste(shown, collapse = ", "),
      if (length(bad_duration) > 5) paste0(" (and ", length(bad_duration) - 5, " more)"),
      call. = FALSE
    )
  }

  invisible(notesDF)
}
