skip_if_offline <- function() {
  res <- tryCatch(
    pingr::ping("https://api.github.com", count = 1L),
    error = function(e) NA
  )

  if (is.na(res)) skip("No internet connection")
}

skip_if_empty_gh_token <- function() {
  res <- tryCatch(
    gh::gh_token() != "",
    error = function(e) FALSE
  )

  if (isFALSE(res)) skip("Not run with empty GH token")
}
