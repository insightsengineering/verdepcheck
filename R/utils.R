`%nin%` <- Negate(`%in%`)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

.desc_field <- "Config/Needs/verdepcheck"

pkgenv <- new.env(parent = emptyenv())

#' @importFrom pkgdepends as_pkg_dependencies
default_config <- function() {
  list(
    dependencies = c(.desc_field, pkgdepends::as_pkg_dependencies(TRUE)$direct),
    cran_mirror = pkgcache::repo_resolve("PPM@latest"),
    library = tempfile()
  )
}
append_config <- function(x1, x2) {
  utils::modifyList(x1, x2)
}

#' @importFrom utils installed.packages
base_pkgs <- function() {
  c("R", rownames(utils::installed.packages(priority = "base")))
}

#' @importFrom pkgcache ppm_snapshots repo_resolve
#'
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' get_ppm_snapshot_by_date(NA)
#' get_ppm_snapshot_by_date("2023-08-01")
#' get_ppm_snapshot_by_date(Sys.Date() + 10)
get_ppm_snapshot_by_date <- function(date = NA) {
  if (is.na(date)) {
    return(pkgcache::repo_resolve("PPM@latest"))
  }
  if (date >= tail(pkgcache::ppm_snapshots(), 1)$date) {
    return(pkgcache::repo_resolve("PPM@latest"))
  }
  if (date <= head(pkgcache::ppm_snapshots(), 1)$date) {
    return(pkgcache::repo_resolve(sprintf("PPM@%s", head(pkgcache::ppm_snapshots(), 1)$date)))
  }
  tryCatch(
    pkgcache::repo_resolve(sprintf("PPM@%s", as.character(as.Date(date) + 1))),
    error = function(err) {
      warning("Could not resolve the PPM snapshot by date. Using the latest PPM snapshot.")
      pkgcache::repo_resolve("PPM@latest")
    }
  )
}

#' Resolve the dependencies of a package based on its release date.
#'
#' @importFrom pkgdepends new_pkg_deps parse_pkg_ref
#' @keywords internal
resolve_ppm_snapshot <- function(pkg_ref_str, operator, pkg_version) {
  i_ref <- pkgdepends::parse_pkg_ref(pkg_ref_str)

  i_ref_minver <- get_ref_min_incl_cran(i_ref, operator, pkg_version)

  i_release_date <- get_release_date(i_ref_minver)
  i_avail_date <- get_avail_date(i_ref_minver, start = i_release_date)

  ppm_repo <- get_ppm_snapshot_by_date(i_avail_date)

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

#' Create `cli` progress bar to print status to the console.
#' @importFrom cli col_blue col_yellow cli_progress_bar col_green pb_current pb_elapsed pb_eta pb_extra
#' pb_spin pb_total style_bold symbol
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

#' Temporarily create a valid DESCRIPTION file to a location that will be deleted
#'
#' The file is deleted after the parent environment where this function was called
#' has exited, when the R session ends or on demand via [withr::deferred_run()]
#'
#' @param pkg_list (`vector`) named character vector or list with
#' paired name and type of dependency. It supports versions by using quotes on
#' the key
#' @param remotes (`vector`) string vector that contains remotes to add to
#' the DESCRIPTION file
#' @param need_verdepcheck (`vector`) string vector that contains
#' `Config/Need/verdepcheck` elements to add to the DESCRIPTION file
#' @param .local_envir (`envirnoment`) The environment to use for scoping.
#'
#' @importFrom desc desc
#' @importFrom withr defer
#'
#' @keywords internal
#' @examples
#' verdepcheck:::local_description(
#'   list(dplyr = "Import"),
#'   remotes = "tidyverse/dplyr",
#'   need_verdepcheck = "dplyr=tidyverse/dplyr@v1.1.0"
#' )
local_description <- function(pkg_list = c(pkgdepends = "Import"),
                              remotes = c(),
                              need_verdepcheck = c(),
                              .local_envir = parent.frame()) {
  d_std <- desc::desc("!new")

  for (pkg in names(pkg_list)) {
    d_std$set_dep(pkg, pkg_list[[pkg]])
  }

  for (remote in remotes) {
    d_std$add_remotes(remote)
  }

  if (!is.null(need_verdepcheck) && length(need_verdepcheck) > 0) {
    d_std$set(.desc_field, paste(need_verdepcheck, collapse = ", "))
  }

  path <- tempfile(pattern = "DESCRIPTION-")
  d_std$write(path)
  withr::defer(unlink(path), envir = .local_envir)

  path
}

#' Parse through vector of `remote_ref` and retrieve one of the keys of each
#' element
#'
#' Support function to reduce repetitive code
#'
#' @param x (`list`) list of lists where each internal list contain the same key
#' @param key (`character(1)`) key of field to retrieve
#'
#' @keywords internal
#'
#' @examples
#' verdepcheck:::map_key_character(
#'   list(list(a = "1", b = "2"), list(a = "3", b = "4"), list(a = "5", b = "6")),
#'   "a"
#' )
map_key_character <- function(x, key) {
  vapply(x, `[[`, character(1), key)
}
