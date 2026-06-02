system_file <- function(...) {
  system.file(..., package = "pjnz", mustWork = TRUE)
}

vlapply <- function(...) {
  vapply(..., logical(1))
}

inform <- function() {
  inf <- getOption("pjnz.inform", default = TRUE)

  if (!rlang::is_true(inf) && !rlang::is_false(inf)) {
    inf <- TRUE
  }
  inf
}
