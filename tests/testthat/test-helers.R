skip_if_empty_gh_token <- function() {
  if (gh::gh_token() != "") {
    return(invisible(TRUE))
  }

  skip("Not run with empty GH token")
}
