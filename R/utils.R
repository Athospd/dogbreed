clean_string <- function(str) {
  gsub("\\W", "_", str)
}
