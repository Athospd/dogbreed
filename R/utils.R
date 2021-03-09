clean_string <- function(str) {
  gsub("\\W", "_", str)
}

#' Set progress bar
#'
#' @param total (int) total duration of the progress bar.
#'
#' @export
set_progress_bar <- function(total) {
  progress::progress_bar$new(
    total = total, clear = FALSE, width = 70,
    format = ":current/:total [:bar] - :elapsed - loss: :loss"
  )
}
