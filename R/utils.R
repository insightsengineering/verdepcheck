`%nin%` <- Negate(`%in%`)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

.desc_field <- "Config/Needs/verdepcheck"

default_config <- function() {
  list(
    dependencies = c(.desc_field, pkgdepends::as_pkg_dependencies(TRUE)$direct),
    library = tempfile()
  )
}
append_config <- function(x1, x2) {
  append(x1, x2)[unique(c(names(x1), names(x2)))]
}

#' @importFrom utils installed.packages
base_pkgs <- function() {
  c("R", rownames(utils::installed.packages(priority = "base")))
}

#' @importFrom pkgcache ppm_snapshots
get_ppm_snapshot_by_date <- function(date) {
  snaps <- pkgcache::ppm_snapshots()
  res <- as.character(as.Date(head(snaps[as.Date(snaps$date) > as.Date(date), "date"], 1)))
  if (length(res) == 0) stop(sprintf("Cannot find PPM snapshot for date after %s.", as.character(date)))
  res
}

#' @importFrom pkgcache ppm_repo_url
parse_ppm_url <- function(snapshot) {
  file.path(pkgcache::ppm_repo_url(), snapshot)
}
