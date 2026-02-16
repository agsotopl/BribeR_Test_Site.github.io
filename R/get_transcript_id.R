#' Retrieve Available Transcript IDs
#'
#' Lists all available transcript IDs based on the `.csv` files stored in
#' the `data-raw/transcripts` folder of the **BribeR** package.
#'
#' Each transcript is named numerically (e.g., `1.csv`, `19.csv`, `104.csv`),
#' and its ID corresponds directly to that number.
#'
#' @return A sorted numeric vector of available transcript IDs.
#'
#' @examples
#' # Retrieve all available transcript IDs
#' ids <- get_transcript_id()
#' ids
#'
#' # Use those IDs to load specific transcripts
#' subset <- get_transcripts_raw(n = ids[1:3])
#'
#' @seealso [read_transcripts()], [get_transcripts_raw()], [get_transcript_speakers()]
#' @export
get_transcript_id <- function() {
  transcripts_dir <- system.file("data-raw", "transcripts", package = "BribeR")

  # Fallback: local path during development
  if (transcripts_dir == "" && dir.exists(file.path("data-raw", "transcripts"))) {
    transcripts_dir <- file.path("data-raw", "transcripts")
  }

  if (transcripts_dir == "" || !dir.exists(transcripts_dir)) {
    stop("Transcript directory not found. Is the BribeR package installed correctly?", call. = FALSE)
  }

  files <- list.files(transcripts_dir, pattern = "\\.csv$", full.names = FALSE)
  ids <- as.numeric(tools::file_path_sans_ext(files))
  ids <- sort(ids, na.last = NA)
  ids
}
