system_file <- function(...) {
  system.file(..., package = "pjnz", mustWork = TRUE)
}

vlapply <- function(...) {
  vapply(..., logical(1))
}
