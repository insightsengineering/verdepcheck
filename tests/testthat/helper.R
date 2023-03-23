skip_if_offline <- function() {
  res <- tryCatch(
    pingr::ping("https://api.github.com", count = 1L),
    error = function(e) NA
  )

  if (is.na(res)) skip("No internet connection")
}

skip_if_empty_gh_token <- function() {
  if (gh::gh_token() != "") {
    return(invisible(TRUE))
  }

  skip("Not run with empty GH token")
}
