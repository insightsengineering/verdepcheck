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

#' Resolve the dependencies of package based on the release date + 1
#'
#' @keywords internal
#' @importFrom pkgcache ppm_repo_url
#' @importFrom pkgdepends new_pkg_deps parse_pkg_ref
resolve_ppm_snapshot <- function(pkg_ref_str, operator, pkg_version) {

  i_ref <- pkgdepends::parse_pkg_ref(pkg_ref_str)

  i_ref_minver <- get_ref_min_incl_cran(i_ref, operator, pkg_version)

  i_release_date <- get_release_date(i_ref_minver)

  if (is.na(i_release_date)) {
    ppm_repo <- file.path(pkgcache::ppm_repo_url(), "latest")
  } else {
    ppm_repo <- parse_ppm_url(get_ppm_snapshot_by_date(i_release_date))
  }

  i_pkg_deps <- pkgdepends::new_pkg_deps(
    ifelse(
      inherits(i_ref_minver, "remote_ref_github"),
      i_ref_minver$ref,
      i_ref$ref
    ),
    config = list(dependencies = "hard", cran_mirror = ppm_repo)
  )
  suppressMessages(i_pkg_deps$resolve())

  i_res <- i_pkg_deps$get_resolution()
  i_res$direct <- i_res$directpkg <- FALSE
  i_res$parent <- pkg_ref_str
  i_res
}
