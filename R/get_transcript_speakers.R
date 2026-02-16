#' Get speakers present in each transcript
#'
#' Loads the bundled `speakers_per_transcript` dataset and returns one row per
#' transcript `n` with a list-column of unique speakers present in that transcript.
#'
#' @return A tibble with columns:
#'   - `n` (character): transcript id
#'   - `speakers` (list): unique, sorted character vector of speakers for that transcript
#'
#' @examples
#' # Load all unique speakers in each transcript
#' speakers <- get_transcript_speakers()
#' head(speakers)
#'
#' @seealso [read_transcripts()], [get_transcript_id()], [get_transcripts_raw()]
#' @export
get_transcript_speakers <- function() {
  rda_path <- system.file("data", "speakers_per_transcript.rda", package = "BribeR")
  if (rda_path == "") {
    stop("Could not find speakers_per_transcript.rda in the BribeR package.", call. = FALSE)
  }

  env <- new.env()
  load(rda_path, envir = env)
  df <- env$speakers_per_transcript

  if (!"n" %in% names(df)) {
    stop("Expected column 'n' in speakers_per_transcript dataset.", call. = FALSE)
  }

  # accept both correct and misspelled prefixes
  speaker_cols <- grep("^(speakrer_std_|speaker_std_)[0-9]+$", names(df), value = TRUE)
  if (!length(speaker_cols)) {
    stop(
      "No speaker columns found. Expected columns like 'speaker_std_1'.",
      call. = FALSE
    )
  }

  df |>
    dplyr::mutate(n = as.character(.data$n)) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      speakers = list({
        v <- unlist(dplyr::c_across(dplyr::all_of(speaker_cols)), use.names = FALSE)
        v <- as.character(v)
        v <- v[!is.na(v)]
        v <- trimws(v)
        v <- v[nzchar(v)]
        sort(unique(v))
      })
    ) |>
    dplyr::ungroup() |>
    dplyr::select(.data$n, .data$speakers) |>
    tibble::as_tibble()
}







