#' Get reference to the minimal version of the package including also check in CRAN repository.
#'
#' @param remote_ref (`remote_ref`) object created with [`pkgdepends::parse_pkg_ref()`]
#' @param op (`character(1)`) optional, version condition comparison operator (e.g. `">"`, `">="`)
#' @param op_ver (`character(1)`) optional, version number against which `op` argument is applied
#'
#' @returns (`remote_ref`) object with the package reference
#'
#' @export
#'
#' @seealso [get_ref_min_incl_cran()]
get_ref_min_incl_cran <- function(remote_ref, op = "", op_ver = "") {
  UseMethod("get_ref_min_incl_cran", remote_ref)
}

#' @rdname get_ref_min_incl_cran
#' @export
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' verdepcheck:::get_ref_min_incl_cran(pkgdepends::parse_pkg_ref("dplyr"))
#' @examples
#' verdepcheck:::get_ref_min_incl_cran(pkgdepends::parse_pkg_ref("bioc::MultiAssayExperiment"))
get_ref_min_incl_cran.remote_ref <- function(remote_ref, op = "", op_ver = "") {
  get_ref_min(remote_ref, op, op_ver)
}

#' @rdname get_ref_min_incl_cran
#' @importFrom pkgdepends parse_pkg_ref
#' @export
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' verdepcheck:::get_ref_min_incl_cran(pkgdepends::parse_pkg_ref("tidyverse/dplyr"))
get_ref_min_incl_cran.remote_ref_github <- function(remote_ref, op = "", op_ver = "") {
  if (check_if_on_cran(remote_ref, op = op, op_ver = op_ver)) {
    gh_res <- get_ref_min(remote_ref, op, op_ver)
    gh_desc <- get_desc_from_gh(gh_res$username, gh_res$repo, gh_res$commitish)
    gh_ver <- gh_desc$get_version()

    cran_remote_ref <- pkgdepends::parse_pkg_ref(remote_ref$package)
    cran_res <- get_ref_min(cran_remote_ref, op, op_ver)
    cran_ver <- cran_res$version

    if (package_version(cran_ver) <= package_version(gh_ver)) {
      return(cran_res)
    } else {
      return(gh_res)
    }
  } else {
    return(get_ref_min(remote_ref, op, op_ver))
  }
}

#' Check if package is available on CRAN.
#' @importFrom pkgcache meta_cache_list
#' @keywords internal
#'
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' verdepcheck:::check_if_on_cran(pkgdepends::parse_pkg_ref("dplyr"))
#' verdepcheck:::check_if_on_cran(pkgdepends::parse_pkg_ref("dplyr"), op = ">=", op_ver = "1.1.0")
#' verdepcheck:::check_if_on_cran(pkgdepends::parse_pkg_ref("dplyr"), op = ">=", op_ver = "9999.9.99")
#' verdepcheck:::check_if_on_cran(pkgdepends::parse_pkg_ref("dplyr"), op = "<", op_ver = "0.0.0")
check_if_on_cran <- function(remote_ref, op = "", op_ver = "") {
  cran_listings <- pkgcache::meta_cache_list(remote_ref$package)
  if (op == "" || op_ver == "") {
    return(NROW(cran_listings) > 0)
  }
  # Check if minimum version exists on CRAN
  NROW(filter_valid_version(cran_listings$version, op, op_ver)) > 0
}

#' Get reference to the minimal version of the package.
#'
#' @inherit get_ref_min_incl_cran
#' @export
#'
#' @seealso [get_ref_min_incl_cran()]
get_ref_min <- function(remote_ref, op = "", op_ver = "") {
  UseMethod("get_ref_min", remote_ref)
}

#' @rdname get_ref_min
#' @export
#' @examples
#' get_ref_min(pkgdepends::parse_pkg_ref("bioc::MultiAssayExperiment"))
get_ref_min.remote_ref <- function(remote_ref, op = "", op_ver = "") {
  remote_ref
}

#' * for standard and CRAN-type of remote - this would use [`pkgcache::cran_archive_list()`]
#' to obtain historical data.
#'
#' @rdname get_ref_min
#' @export
#' @importFrom cli cli_alert_danger
#' @importFrom pkgcache cran_archive_list meta_cache_list
#' @importFrom pkgdepends parse_pkg_ref
#' @importFrom stats setNames
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' get_ref_min(pkgdepends::parse_pkg_ref("cran::dplyr"))
get_ref_min.remote_ref_cran <- function(remote_ref, op = "", op_ver = "") {
  if (remote_ref$version != "") {
    return(remote_ref)
  }

  x_pkg_cache <- pkgcache::meta_cache_list(remote_ref$package)
  x_pkg_cache_archive <- pkgcache::cran_archive_list(packages = remote_ref$package)
  pv <- unique(c(x_pkg_cache_archive$version, x_pkg_cache$version))
  pv <- stats::setNames(package_version(pv), pv)
  pv <- filter_valid_version(pv, op, op_ver)
  min_ver <- Filter(function(x) x == min(pv), pv)

  new_ref <- sprintf("%s@%s", remote_ref$package, names(min_ver))
  tryCatch(
    pkgdepends::parse_pkg_ref(new_ref),
    error = function(err) {
      cli::cli_alert_danger(
        paste(
          sep = " ",
          "Problem with finding CRAN release meeting following criteria:",
          "`{remote_ref$package} ({op} {op_ver})`.",
          "The package name or version might be invalid."
        )
      )
      stop(err)
    }
  )
}

#' @rdname get_ref_min
#' @export
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' get_ref_min(pkgdepends::parse_pkg_ref("dplyr"))
get_ref_min.remote_ref_standard <- function(remote_ref, op = "", op_ver = "") {
  get_ref_min.remote_ref_cran(remote_ref, op, op_ver)
}

#' * for GitHub type of remote - this would use [`gh::gh_gql()`] to get list of all releases or tags
#' and then [`gh::gh()`] to download `DESCRIPTION` file and then read package version.
#'
#' @rdname get_ref_min
#' @export
#' @importFrom pkgdepends parse_pkg_ref
#'
#' @examplesIf gh::gh_token() != ""
#' get_ref_min(pkgdepends::parse_pkg_ref("tidyverse/dplyr"))
get_ref_min.remote_ref_github <- function(remote_ref, op = "", op_ver = "") {
  if (remote_ref$commitish != "") {
    return(remote_ref)
  }

  refs <- get_gh_refs(remote_ref$username, remote_ref$repo)

  if (length(refs) == 0) {
    return(remote_ref)
  }

  ref_suffix <- ""
  if (op == "") {
    # loop through the refs starting from the earliest until the first valid description file
    for (ref in refs) {
      ref_desc <- get_desc_from_gh(remote_ref$username, remote_ref$repo, ref)
      if ((length(ref_desc) == 1 && is.na(ref_desc)) || ref_desc$get_field("Package") != remote_ref$package) next
      ref_suffix <- sprintf("@%s", ref)
      break
    }
  } else {
    # loop through the refs starting from the earliest until the first version condition met
    for (ref in refs) {
      ref_desc <- get_desc_from_gh(remote_ref$username, remote_ref$repo, ref)
      if ((length(ref_desc) == 1 && is.na(ref_desc)) || ref_desc$get_field("Package") != remote_ref$package) next
      ref_ver <- ref_desc$get_version()
      op_res <- check_valid_version(ref_ver, op, op_ver)
      if (op_res) {
        ref_suffix <- sprintf("@%s", ref)
        break
      }
    }
  }

  new_ref <- sprintf(
    "%s=%s/%s%s",
    remote_ref$package,
    remote_ref$username,
    remote_ref$repo,
    ref_suffix
  )
  pkgdepends::parse_pkg_ref(new_ref)
}

# Get list of releases if not empty else get list of tags
#' @keywords internal
get_gh_refs <- function(org, repo) {
  res <- get_gh_releases(org, repo)
  if (length(res) > 0) {
    return(res)
  }
  get_gh_tags(org, repo)
}

#' @importFrom gh gh_gql
#' @keywords internal
get_gh_releases <- function(org, repo, max_date = Sys.Date() + 1, min_date = as.Date("1900-01-01")) {
  gql_query <- sprintf("{
    repository(owner: \"%s\", name: \"%s\") {
      releases(last: 100, orderBy: { field: CREATED_AT, direction: ASC}) {
        nodes {
          tagName
          isPrerelease
          createdAt
        }
      }
    }
  }", org, repo)
  resp <- try(gh::gh_gql(gql_query), silent = TRUE)
  if (inherits(resp, "try-error")) {
    return(character(0))
  }
  res <- Filter(
    function(x) isFALSE(x$isPrerelease) & x$createdAt > min_date & x$createdAt < max_date,
    resp$data$repository$releases$nodes
  )
  map_key_character(res, "tagName")
}

#' @importFrom gh gh_gql
#' @keywords internal
get_gh_tags <- function(org, repo, max_date = Sys.Date() + 1, min_date = as.Date("1900-01-01")) {
  gql_query <- sprintf("{
    repository(owner: \"%s\", name: \"%s\") {
      refs(refPrefix: \"refs/tags/\", last: 100, orderBy: {field: TAG_COMMIT_DATE, direction: ASC}) {
        nodes {
          name
          target {
            ... on Commit {
              committedDate
            }
          }
        }
      }
    }
  }", org, repo)
  resp <- try(gh::gh_gql(gql_query), silent = TRUE)
  if (inherits(resp, "try-error")) {
    return(character(0))
  }
  res <- Filter(
    function(x) as.Date(x$target$committedDate) > min_date & as.Date(x$target$committedDate) < max_date,
    resp$data$repository$refs$nodes
  )
  map_key_character(res, "name")
}

#' Get DESCRIPTION from GitHub Repository
#'
#' @importFrom desc desc
#' @importFrom gh gh
#' @keywords internal
#'
#' @examplesIf gh::gh_token() != ""
#' verdepcheck:::get_desc_from_gh("tidyverse", "dplyr")
#' verdepcheck:::get_desc_from_gh("tidyverse", "dplyr", "v1.1.0")
get_desc_from_gh <- function(org, repo, ref = "") {
  if (ref == "") ref <- "HEAD"
  url_str <- sprintf("/repos/%s/%s/contents/DESCRIPTION?ref=%s", org, repo, ref)
  resp <- try(gh::gh(url_str, .accept = "application/vnd.github.v3.raw+json"), silent = TRUE)
  if (inherits(resp, "try-error")) {
    return(NA)
  }
  desc::desc(text = resp$message)
}

#' Get reference to the maximal version of the package.
#'
#' @inheritParams get_ref_min
#' @inherit get_ref_min return
#'
#' @export
#'
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' get_ref_max(pkgdepends::parse_pkg_ref("dplyr"))
#' get_ref_max(pkgdepends::parse_pkg_ref("cran::dplyr"))
#' get_ref_max(pkgdepends::parse_pkg_ref("tidyverse/dplyr"))
#' get_ref_max(pkgdepends::parse_pkg_ref("bioc::MultiAssayExperiment"))
get_ref_max <- function(remote_ref) {
  get_ref_internal(remote_ref, include_release = FALSE)
}

#' Get reference to the release version of the package.
#'
#' @inheritParams get_ref_min
#' @inherit get_ref_min return
#'
#' @importFrom pkgdepends parse_pkg_ref
#' @export
#'
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' get_ref_release(pkgdepends::parse_pkg_ref("dplyr"))
#' get_ref_release(pkgdepends::parse_pkg_ref("cran::dplyr"))
#' get_ref_release(pkgdepends::parse_pkg_ref("tidyverse/dplyr"))
#' get_ref_release(pkgdepends::parse_pkg_ref("bioc::MultiAssayExperiment"))
get_ref_release <- function(remote_ref) {
  get_ref_internal(remote_ref, include_input = FALSE)
}

#' Get reference of the maximal version of the package.
#'
#' @inheritParams get_ref_min
#' @param include_input (`logical(1)`) if `TRUE` then include input reference in search
#' @param include_cran (`logical(1)`) if `TRUE` then include CRAN reference in search
#' @param include_release (`logical(1)`) if `TRUE` then also check for released version.
#' Supported only for GitHub type of reference.
#'
#' @inherit get_ref_min return
#' @keywords internal
get_ref_internal <- function(
    remote_ref,
    include_input = TRUE,
    include_cran = TRUE,
    include_release = TRUE) {
  # create list of ref candidates to check
  # return the one of the highest version
  # this is a named list of character with version values and refs names
  ref_candidates <- list()

  if (include_input) {
    input_ref <- remote_ref$ref
    input_ver <- get_version(remote_ref)
    ref_candidates <- c(ref_candidates, setNames(list(input_ver), input_ref))
  }

  if (include_cran && check_if_on_cran(remote_ref)) {
    cran_ref <- remote_ref$package
    cran_ver <- get_version(pkgdepends::parse_pkg_ref(cran_ref))
    ref_candidates <- c(ref_candidates, setNames(list(cran_ver), cran_ref))
  }

  if (include_release && inherits(remote_ref, "remote_ref_github")) {
    gh_release_ref <- cond_parse_pkg_ref_release(remote_ref)
    if (!is.null(gh_release_ref)) {
      gh_release_ver <- get_version(gh_release_ref)
      ref_candidates <- c(ref_candidates, setNames(list(gh_release_ver), gh_release_ref$ref))
    }

    if (
      (!is.null(remote_ref$commitish) && remote_ref$commitish != "") ||
        (!is.null(remote_ref$pull) && remote_ref$pull != "")
    ) {
      gh_ref <- remote_ref
      gh_ver <- get_version(gh_ref)
      ref_candidates <- c(ref_candidates, setNames(list(gh_ver), gh_ref$ref))
    }
  }

  ref_candidates <- Filter(Negate(is.na), ref_candidates)

  if (length(ref_candidates) == 0) {
    return(NA)
  }

  max_ver <- ref_candidates[[1]]
  max_ref <- names(ref_candidates[1])
  for (i in seq_along(ref_candidates)) {
    i_ver <- ref_candidates[[i]]
    i_ref <- names(ref_candidates[i])
    if (!is.na(i_ver) && i_ver > max_ver) {
      max_ref <- i_ref
      max_ver <- i_ver
    }
  }

  return(pkgdepends::parse_pkg_ref(max_ref))
}

#' @importFrom pkgdepends parse_pkg_ref
#' @importFrom remotes github_remote
#' @keywords internal
cond_parse_pkg_ref_release <- function(remote_ref) {
  stopifnot(inherits(remote_ref, "remote_ref_github"))

  has_release <- function(remote_ref) {
    isFALSE(inherits(
      try(remotes::github_remote(sprintf("%s/%s@*release", remote_ref$username, remote_ref$repo)), silent = TRUE),
      "try-error"
    ))
  }
  parse_pkg_ref_remote <- function(remote_ref) {
    parse_pkg_ref(sprintf("%s/%s@*release", remote_ref$username, remote_ref$repo)) # nolint
  }

  if (has_release(remote_ref)) {
    parse_pkg_ref_remote(remote_ref)
  } else {
    NULL
  }
}

#' Get package version.
#'
#' @inheritParams get_ref_min
#' @returns Package version created with `package_version`.
#'
#' @export
get_version <- function(remote_ref) {
  UseMethod("get_version", remote_ref)
}

#' @rdname get_version
#' @importFrom pkgdepends new_pkg_deps
#' @export
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' get_version(pkgdepends::parse_pkg_ref("dplyr"))
#' get_version(pkgdepends::parse_pkg_ref("tidyverse/dplyr"))
#' get_version(pkgdepends::parse_pkg_ref("tidyverse/dplyr@v1.1.0"))
#' get_version(pkgdepends::parse_pkg_ref("bioc::MultiAssayExperiment"))
get_version.remote_ref <- function(remote_ref) {
  x <- pkgdepends::new_pkg_deps(remote_ref$ref, config = list(dependencies = FALSE))
  x$solve()
  if (x$get_solution()$status == "FAILED") {
    return(NA)
  } else {
    as.package_version(x$get_resolution()[1, "version"])
  }
}


#' Get release date.
#'
#' @inheritParams get_ref_min
#' @returns Date
#'
#' @export
get_release_date <- function(remote_ref) {
  UseMethod("get_release_date", remote_ref)
}

#' @rdname get_release_date
#' @export
#' @importFrom gh gh_gql
#'
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != "" && gh::gh_token() != ""
#' get_release_date(pkgdepends::parse_pkg_ref("tidyverse/dplyr@v1.1.0"))
get_release_date.remote_ref_github <- function(remote_ref) {
  x <- pkgdepends::new_pkg_deps(remote_ref$ref, config = list(dependencies = FALSE))
  x$solve()
  if (x$get_solution()$status == "FAILED") {
    return(as.Date(NA))
  }

  sha <- x$get_resolution()[1, "extra"][[1]]$remotesha
  if (is.null(sha)) {
    return(as.Date(NA))
  }

  gql_query <- sprintf("{
    repository(owner: \"%s\", name: \"%s\") {
      object(oid: \"%s\") {
        ... on Commit {
          committedDate
        }
      }
    }
  }", remote_ref$username, remote_ref$repo, sha)

  resp <- try(gh::gh_gql(gql_query), silent = TRUE)
  if (inherits(resp, "try-error") || is.null(resp$data$repository$object$committedDate)) {
    return(as.Date(NA))
  }

  as.Date(resp$data$repository$object$committedDate)
}

#' @rdname get_release_date
#' @export
#'
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' get_release_date(pkgdepends::parse_pkg_ref("cran::dplyr"))
#' get_release_date(pkgdepends::parse_pkg_ref("cran::dplyr@1.1.0"))
get_release_date.remote_ref_cran <- function(remote_ref) {
  rel_data <- get_release_data(remote_ref$package)

  if (nrow(rel_data) == 0) {
    return(as.Date(NA))
  }

  if (remote_ref$atleast != "") {
    idx <- do.call(
      remote_ref$atleast,
      list(
        package_version(rel_data$version, strict = FALSE),
        package_version(remote_ref$version, strict = FALSE)
      )
    )
    as.Date(utils::tail(rel_data[idx, "mtime"], 1))
  } else {
    as.Date(utils::tail(rel_data$mtime, 1))
  }
}

#' @rdname get_release_date
#' @export
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' get_release_date(pkgdepends::parse_pkg_ref("dplyr"))
get_release_date.remote_ref_standard <- function(remote_ref) {
  get_release_date.remote_ref_cran(remote_ref)
}

#' @rdname get_release_date
#' @export
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' get_release_date(pkgdepends::parse_pkg_ref("MultiAssayExperiment"))
get_release_date.remote_ref_bioc <- function(remote_ref) {
  get_release_date.remote_ref_cran(remote_ref)
}

#' @rdname get_release_date
#' @export
get_release_date.remote_ref <- function(remote_ref) {
  as.Date(NA_real_)
}

#' Get data for CRAN/Bioconductor package releases
#'
#' @importFrom pkgcache cran_archive_list meta_cache_list
#' @importFrom jsonlite fromJSON
#'
#' @keywords internal
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' verdepcheck:::get_release_data("dplyr")
#' verdepcheck:::get_release_data("MultiAssayExperiment")
get_release_data <- function(package) {
  cran_archive <- pkgcache::cran_archive_list(packages = package)[, c("package", "version", "mtime")]
  cran_current <- head(
    pkgcache::meta_cache_list(packages = package)[, c("type", "package", "version", "published")],
    1
  )

  # handle missing dates
  if (nrow(cran_current) > 0) {
    if (is.na(cran_current$published)) {
      if (cran_current$type == "cran") {
        # in general, this should not happen for cran - https://github.com/r-lib/pkgcache/issues/109
        # this is a temporary workaround
        if (is.null(pkgenv$cache_db)) {
          pkgenv$cache_db <- tools::CRAN_package_db()
        }
        db <- subset(pkgenv$cache_db, pkgenv$cache_db$Package == package)
        cran_current <- data.frame(
          type = "cran",
          package = package,
          version = db$Version[1],
          published = as.POSIXct(db$`Date/Publication`[1])
        )
      } else if (cran_current$type == "bioc") {
        url <- sprintf(
          "https://packagemanager.posit.co/__api__/repos/bioconductor/packages/%s?bioc_version=%s",
          package,
          pkgcache::bioc_version()
        )
        data <- jsonlite::fromJSON(readLines(url, warn = FALSE))
        release_date <- as.POSIXct(data$occurred) %||%
          as.POSIXct(data$package_date) %||%
          as.POSIXct(data$date_publication) %||%
          as.POSIXct(NA)
        cran_current <- data.frame(
          type = "bioc",
          package = package,
          version = cran_current$version,
          published = release_date
        )
      }
    }
  }

  # Remove extra columns
  cran_current <- cran_current[, setdiff(names(cran_current), c("type"))]

  cran_current <- setNames(cran_current, names(cran_archive))
  rbind(cran_archive, cran_current)
}


#' Get available date for the package.
#'
#' Oftentimes, the release date of the package does not correspond to the date when the package is
#' available in the PPM. Usually it takes one day for the PPM to sync with the CRAN.
#' This function will return the date when the package is available in the PPM.
#'
#' @inheritParams get_ref_min
#' @param start (`Date`) optional, the date when the package was released
#' @returns Date. This can be safely used as a snapshot date for the PPM.
#'
#' @export
get_avail_date <- function(remote_ref, start = get_release_date(remote_ref)) {
  if (is.na(start)) {
    return(as.Date(NA_real_))
  }
  UseMethod("get_avail_date", remote_ref)
}

#' @rdname get_avail_date
#'
#' @importFrom pkgcache ppm_snapshots
#'
#' @export
#'
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' get_avail_date(pkgdepends::parse_pkg_ref("cran::dplyr"))
#' get_avail_date(pkgdepends::parse_pkg_ref("cran::dplyr@1.1.0"))
get_avail_date.remote_ref_cran <- function(remote_ref, start = get_release_date(remote_ref)) {
  max_iter <- 5
  i <- 0
  date <- start
  while (i <= max_iter) {
    # there are some gaps in the snapshots so it's important to at first find the closest date
    ppm_url <- get_ppm_snapshot_by_date(date)
    date <- `if`(
      grepl("/latest$", ppm_url),
      tail(pkgcache::ppm_snapshots(), 1)$date,
      unname(as.Date(sub(".*/", "", ppm_url)))
    )
    if (remote_ref$atleast != "") {
      data <- available.packages(
        repos = ppm_url,
        filters = list(
          function(db) {
            db[
              db[, "Package"] == remote_ref$package &
                do.call(
                  # don't use `remote_ref$atleast` and hardcode `>=` instead
                  # requested version might not be available even in the oldest PPM
                  # example: `jsonlite@0.9.0` and the oldest PPM has `jsonlite@1.5`
                  ">=",
                  list(
                    package_version(db[, "Version"], strict = FALSE),
                    package_version(remote_ref$version, strict = FALSE)
                  )
                ),
            ]
          }
        )
      )
    } else {
      data <- available.packages(
        repos = ppm_url,
        filters = list(function(db) db[db[, "Package"] == remote_ref$package, ])
      )
    }
    if (length(data) > 0) {
      return(date)
    }
    date <- date + 1
    i <- i + 1
  }
  warning("No available date found.")
  as.Date(NA_real_)
}

#' @rdname get_avail_date
#' @export
#'
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' get_avail_date(pkgdepends::parse_pkg_ref("dplyr"))
#' get_avail_date(pkgdepends::parse_pkg_ref("dplyr@1.1.0"))
get_avail_date.remote_ref_standard <- function(remote_ref, start = get_release_date(remote_ref)) {
  get_avail_date.remote_ref_cran(remote_ref, start = start)
}

#' @rdname get_avail_date
#' @export
#'
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != "" && gh::gh_token() != ""
#' get_avail_date(pkgdepends::parse_pkg_ref("bioc::MultiAssayExperiment"))
#' get_avail_date(pkgdepends::parse_pkg_ref("tidyverse/dplyr@v1.1.0"))
get_avail_date.remote_ref <- function(remote_ref, start = get_release_date(remote_ref)) {
  start + 1
}
