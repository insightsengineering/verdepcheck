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
#' @exportS3Method get_ref_min_incl_cran remote_ref
#' @examples
#' verdepcheck:::get_ref_min_incl_cran(pkgdepends::parse_pkg_ref("bioc::MultiAssayExperiment"))
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' verdepcheck:::get_ref_min_incl_cran(pkgdepends::parse_pkg_ref("dplyr"))
get_ref_min_incl_cran.remote_ref <- function(remote_ref, op = "", op_ver = "") {
  get_ref_min(remote_ref, op, op_ver)
}

#' @rdname get_ref_min_incl_cran
#' @importFrom pkgdepends parse_pkg_ref
#' @exportS3Method get_ref_min_incl_cran remote_ref_github
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' verdepcheck:::get_ref_min_incl_cran(pkgdepends::parse_pkg_ref("cran/dplyr"))
get_ref_min_incl_cran.remote_ref_github <- function(remote_ref, op = "", op_ver = "") {
  if (check_if_on_cran(remote_ref, list(op = op, op_ver = package_version(op_ver)))) {
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
#' @examples
#' check_if_on_cran(list(package = "magrittr"))
#' check_if_on_cran(list(package = "magrittr"), list(op = ">=", op_ver = "0.5.0"))
#' check_if_on_cran(list(package = "magrittr"), list(op = ">=", op_ver = "9999.9.99"))
#' check_if_on_cran(list(package = "magrittr"), list(op = "<", op_ver = "0.0.0"))
check_if_on_cran <- function(remote_ref, version = NULL) {
  cran_listings <- pkgcache::meta_cache_list(remote_ref$package)
  if (is.null(version)) return(NROW(cran_listings) > 0)
  # Check if minimum version exists on CRAN
  NROW(filter_valid_version(cran_listings$version, version$op, version$op_ver)) > 0
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
#' @exportS3Method get_ref_min remote_ref
#' @examples
#' verdepcheck:::get_ref_min(pkgdepends::parse_pkg_ref("bioc::MultiAssayExperiment"))
get_ref_min.remote_ref <- function(remote_ref, op = "", op_ver = "") {
  remote_ref
}

#' * for standard and CRAN-type of remote - this would use [`pkgcache::cran_archive_list()`]
#' to obtain historical data.
#'
#' @rdname get_ref_min
#' @exportS3Method get_ref_min remote_ref_cran
#' @importFrom pkgcache cran_archive_list meta_cache_list
#' @importFrom pkgdepends parse_pkg_ref
#' @importFrom stats setNames
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' verdepcheck:::get_ref_min(pkgdepends::parse_pkg_ref("cran::dplyr"))
get_ref_min.remote_ref_cran <- function(remote_ref, op = "", op_ver = "") {
  if (remote_ref$atleast == "" && remote_ref$version != "") {
    return(remote_ref)
  }

  x_pkg_cache <- pkgcache::meta_cache_list(remote_ref$package)
  x_pkg_cache_archive <- pkgcache::cran_archive_list(package = remote_ref$package)
  pv <- unique(c(x_pkg_cache_archive$version, x_pkg_cache$version))
  pv <- stats::setNames(package_version(pv), pv)
  pv <- filter_valid_version(pv, op, op_ver)
  min_ver <- Filter(function(x) x == min(pv), pv)

  new_ref <- sprintf("%s@%s", remote_ref$ref, names(min_ver)) # @TODO deparse, add ver, parse again
  tryCatch(
    pkgdepends::parse_pkg_ref(new_ref),
    error = function(err) {
      cli::cli_alert_danger(
        paste(
          sep = " ",
          "Possible problem finding release for:",
          "`{remote_ref$package} ({op} {op_ver})`.",
          "The version might be invalid."
        )
      )
      stop(err)
    }
  )
}

#' @rdname get_ref_min
#' @exportS3Method get_ref_min remote_ref_standard
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' verdepcheck:::get_ref_min(pkgdepends::parse_pkg_ref("dplyr"))
get_ref_min.remote_ref_standard <- function(remote_ref, op = "", op_ver = "") {
  get_ref_min.remote_ref_cran(remote_ref, op, op_ver)
}

#' * for GitHub type of remote - this would use [`gh::gh_gql()`] to get list of all releases or tags
#' and then [`gh::gh()`] to download `DESCRIPTION` file and then read package version.
#'
#' @rdname get_ref_min
#' @exportS3Method get_ref_min remote_ref_github
#' @importFrom pkgdepends parse_pkg_ref
#'
#' @examplesIf gh::gh_token() != ""
#' verdepcheck:::get_ref_min(pkgdepends::parse_pkg_ref("cran/dplyr"))
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
  vapply(res, `[[`, character(1), "tagName")
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
  vapply(res, `[[`, character(1), "name")
}

#' Get DESCRIPTION from GitHub Repository
#'
#' @importFrom desc desc
#' @importFrom gh gh
#' @keywords internal
#'
#' @examples
#' verdepcheck:::get_desc_from_gh("tidyverse", "tibble")
#' verdepcheck:::get_desc_from_gh("insightsengineering", "formatters", "v0.5.0")
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
get_ref_max <- function(remote_ref) {
  remote_ref
}

#' Get reference to the release version of the package.
#'
#' @inheritParams get_ref_min
#' @inherit get_ref_min return
#'
#' @importFrom pkgdepends parse_pkg_ref
#' @export
get_ref_release <- function(remote_ref) {
  if (check_if_on_cran(remote_ref)) {
    return(pkgdepends::parse_pkg_ref(remote_ref$package))
  }
  if (inherits(remote_ref, "remote_ref_github")) {
    if (!is.null(remote_ref$commitish) && remote_ref$commitish != "") {
      return(remote_ref)
    }
    if (!is.null(remote_ref$pull) && remote_ref$pull != "") {
      return(remote_ref)
    }
    if (!is.null(remote_ref$release) && remote_ref$release != "") {
      return(cond_parse_pkg_ref_release(remote_ref))
    }
    return(cond_parse_pkg_ref_release(remote_ref))
  }
  return(remote_ref)
}

#' @importFrom pkgdepends parse_pkg_ref
#' @importFrom remotes github_remote
#' @keywords internal
cond_parse_pkg_ref_release <- function(remote_ref) {
  has_release <- function(remote_ref) {
    isFALSE(inherits(
      try(remotes::github_remote(sprintf("%s/%s@*release", remote_ref$username, remote_ref$repo))),
      "try-error"
    ))
  }
  parse_pkg_ref_remote <- function(remote_ref) {
    # temporary fix for https://github.com/r-lib/pkgdepends/issues/275#issuecomment-1461787363
    # @TODO: replace it with below one-liner if fixed
    # parse_pkg_ref(sprintf("%s/%s@*release", remote_ref$username, remote_ref$repo)) # nolint
    pkgdepends::parse_pkg_ref(
      sprintf(
        "%s=%s/%s@%s",
        remote_ref$package,
        remote_ref$username,
        remote_ref$repo,
        remotes::github_remote(sprintf("%s/%s@*release", remote_ref$username, remote_ref$repo))$ref
      )
    )
  }

  if (has_release(remote_ref)) {
    parse_pkg_ref_remote(remote_ref)
  } else {
    NULL
  }
}

#' Get release date.
#'
#' @inheritParams get_ref_min
#' @inherit get_ref_min return
#'
#' @export
get_release_date <- function(remote_ref) {
  UseMethod("get_release_date", remote_ref)
}

#' Get release date from GitHub references
#'
#' @inheritParams get_release_date
#'
#' @importFrom gh gh_gql
#' @export
#' @examplesIf gh::gh_token() != ""
#' remote_ref <- pkgdepends::parse_pkg_ref("insightsengineering/teal@v0.10.0")
#' verdepcheck:::get_release_date.remote_ref_github(remote_ref)
get_release_date.remote_ref_github <- function(remote_ref) {
  gql_query <- sprintf("{
    repository(owner: \"%s\", name: \"%s\") {
      refs(refPrefix: \"refs/tags/\", query: \"%s\", first: 100) {
        edges {
        	node {
          	name
            target {
              ... on Commit {
                committedDate
              }
            }
          }
        }
      }
    }
  }", remote_ref$username, remote_ref$repo, remote_ref$commitish)

  resp <- try(gh::gh_gql(gql_query), silent = TRUE)
  if (inherits(resp, "try-error")) {
    return(character(0))
  }

  result <- vapply(
    resp$data$repository$refs$edges,
    function(x) {
      if (x$node$name != remote_ref$commitish) {
        return(NA_character_)
      }
      x$node$target$committedDate
    },
    character(1)
  )

  if (length(result) <= 1) {
    return(result %||% NA_character_)
  }

  max(result, na.rm = TRUE)
}

#' Get release date from GitHub references
#'
#' @inheritParams get_release_date
#'
#' @export
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' remote_ref <- pkgdepends::parse_pkg_ref("rlang@1.0.0")
#' get_release_date.remote_ref_cran(remote_ref)
get_release_date.remote_ref_cran <- function(remote_ref) {
  subset(
    get_cran_data(remote_ref$package),
    package_version(version, strict = FALSE) == package_version(remote_ref$version, strict = FALSE),
    select = "mtime"
  )[[1]][1]
}

#' @export
get_release_date.remote_ref_standard <- function(remote_ref) {
  get_release_date.remote_ref_cran(remote_ref)
}

#' @export
get_release_date.remote_ref <- function(remote_ref) {
  NA
}

#' @importFrom pkgcache cran_archive_list meta_cache_list
#' @keywords internal
get_cran_data <- function(package) {
  cran_archive <- pkgcache::cran_archive_list(packages = package)[, c("package", "version", "mtime")]
  cran_current <- pkgcache::meta_cache_list(packages = package)[, c("package", "version", "published")]

  cran_current <- setNames(cran_current, names(cran_archive))
  rbind(cran_archive, cran_current)
}
