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
  res <- as.character(as.Date(utils::head(
    snaps[as.Date(snaps$date) > as.Date(date), "date"],
    1
  )))
  if (length(res) == 0) {
    rlang::warn(sprintf(
        paste0(
          "Cannot find PPM snapshot for date after %s.",
          " Will use current CRAN instead."
        ),
        as.character(date)
    ))
    return(NA)
  }
  res
}

#' @importFrom pkgcache ppm_repo_url
parse_ppm_url <- function(snapshot = NA) {
  if (is.na(snapshot)) return(pkgcache::default_cran_mirror())
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

  if (all(is.na(i_release_date))) {
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
    config = list(dependencies = "hard", cran_mirror = ppm_repo, library = tempfile())
  )
  suppressMessages(i_pkg_deps$resolve())

  i_res <- i_pkg_deps$get_resolution()
  i_res$direct <- i_res$directpkg <- FALSE
  i_res
}

#' Create `cli` progress bar for resolving versions.
#' @importFrom cli cli_progress_bar col_green pb_current pb_elapsed pb_eta pb_extra pb_spin pb_total symbol
#' @keywords internal
cli_pb_init <- function(type, total, ...) {
  cli::cli_progress_bar(
    format = paste(
      "{cli::pb_spin} Resolving",
      "{cli::style_bold(cli::col_yellow(cli::pb_extra$type))}",
      "version of {cli::col_blue(cli::pb_extra$package)}",
      "[{cli::pb_current}/{cli::pb_total}]   ETA:{cli::pb_eta}"
    ),
    format_done = paste0(
      "{cli::col_green(cli::symbol$tick)} Resolved {cli::pb_total} packages in {cli::pb_elapsed}."
    ),
    extra = list(type = type, package = character(0)),
    total = total,
    .envir = parent.frame(2L),
    ...
  )
}
#' @importFrom cli cli_progress_update
#' @keywords internal
cli_pb_update <- function(package, n = 2L, ...) {
  cli::cli_progress_update(extra = list(package = package), .envir = parent.frame(n), ...)
}
