#' Get reference to the minimal version of the package including also check in CRAN repository.
#'
#' @param remote_ref (`remote_ref`) object created with [`pkgdepends::parse_pkg_ref()`]
#' @param op (`character(1)`) optional, version condition comparison operator (e.g. `">"`, `">="`)
#' @param op_ver (`character(1)`) optional, version number against which `op` argument is applied
#'
#' @returns (`remote_ref`) object with the package reference
#'
#' @keywords internal
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
  if (check_if_on_cran(remote_ref)) {
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
check_if_on_cran <- function(remote_ref) {
  nrow(pkgcache::meta_cache_list(remote_ref$package)) > 0
}

#' Get reference to the minimal version of the package.
#'
#' @inherit get_ref_min_incl_cran
#' @keywords internal
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
  pkgdepends::parse_pkg_ref(new_ref)
}

#' @rdname get_ref_min
#' @exportS3Method get_ref_min remote_ref_standard
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' verdepcheck:::get_ref_min(pkgdepends::parse_pkg_ref("dplyr"))
get_ref_min.remote_ref_standard <- function(remote_ref, op = "", op_ver = "") {
  get_ref_min.remote_ref_cran(remote_ref, op, op_ver)
}

#' * for GitHub type of remote - this would use [`gh::gh_gql()`] to get list of all tags
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

  tags <- get_gh_tags(remote_ref$username, remote_ref$repo)

  if (length(tags) == 0) {
    return(remote_ref)
  }

  ref_suffix <- ""
  if (op == "") {
    # loop through the tags starting from the earliest until the first valid description file
    for (tag in tags) {
      tag_desc <- get_desc_from_gh(remote_ref$username, remote_ref$repo, tag)
      if ((length(tag_desc) == 1 && is.na(tag_desc)) || tag_desc$get_field("Package") != remote_ref$package) next
      ref_suffix <- sprintf("@%s", tag)
      break
    }
  } else {
    # loop through the tags starting from the earliest until the first version condition met
    for (tag in tags) {
      tag_desc <- get_desc_from_gh(remote_ref$username, remote_ref$repo, tag)
      if ((length(tag_desc) == 1 && is.na(tag_desc)) || tag_desc$get_field("Package") != remote_ref$package) next
      tag_ver <- tag_desc$get_version()
      op_res <- do.call(op, list(tag_ver, package_version(op_ver)))
      if (op_res) {
        ref_suffix <- sprintf("@%s", tag)
        break
      }
    }
  }

  new_ref <- sprintf("%s=%s/%s%s", remote_ref$package, remote_ref$username, remote_ref$repo, ref_suffix) # @TODO
  pkgdepends::parse_pkg_ref(new_ref)
}


#' @importFrom gh gh_gql
#' @keywords internal
get_gh_tags <- function(org, repo) {
  gql_query <- sprintf("{
    repository(owner: \"%s\", name: \"%s\") {
      refs(refPrefix: \"refs/tags/\", last: 100, orderBy: {field: TAG_COMMIT_DATE, direction: ASC}) {
        edges {
          node {
            name
          }
        }
      }
    }
  }", org, repo)
  resp <- try(gh::gh_gql(gql_query), silent = TRUE)
  if (inherits(resp, "try-error")) {
    return(character(0))
  }
  vapply(resp$data$repository$refs$edges, function(x) x$node$name, character(1))
}
#' @importFrom desc desc
#' @importFrom gh gh
#' @keywords internal
get_desc_from_gh <- function(org, repo, ref = "") {
  if (ref == "") ref <- "HEAD"
  url_str <- sprintf("/repos/%s/%s/contents/DESCRIPTION?ref=%s", org, repo, ref)
  resp <- try(gh::gh(url_str, .accept = "application/vnd.github.v3.raw+json"), silent = TRUE)
  if (inherits(resp, "try-error")) {
    return(NA)
  }
  desc::desc(text = resp$message)
}

#' @keywords internal
filter_valid_version <- function(x, op, op_ver) {
  res <- x
  res <- Filter(Negate(is.na), res)
  if (op != "" && op_ver != "") {
    res <- Filter(function(x) do.call(op, list(x, package_version(op_ver))), res)
  }
  return(res)
}

#' Get reference to the maximal version of the package.
#'
#' @inheritParams get_ref_min
#' @inherit get_ref_min return
#'
#' @importFrom pkgdepends parse_pkg_ref
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
      return(cond_parse_pkg_ref_remote(remote_ref))
    }
    return(cond_parse_pkg_ref_remote(remote_ref))
  }
  return(remote_ref)
}

#' @importFrom pkgdepends parse_pkg_ref
#' @importFrom remotes github_remote
#' @keywords internal
cond_parse_pkg_ref_remote <- function(remote_ref) {
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
