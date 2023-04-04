#' Get minimal version including also check in CRAN repository.
#'
#' @param remote_ref (`remote_ref`) object created with [`pkgdepends::parse_pkg_ref()`]
#' @param op (`character(1)`) optional, version condition comparison operator (e.g. `">"`, `">="`)
#' @param op_ver (`character(1)`) optional, version number against which `op` argument is applied
#'
#' @returns (`remote_ref`) object with reference to the package of minimal version.
#'
#' @seealso [get_min_ver_incl_cran()]
#'
#' @export
get_min_ver_incl_cran <- function(remote_ref, op = "", op_ver = "") {
  UseMethod("get_min_ver_incl_cran", remote_ref)
}

#' @rdname get_min_ver_incl_cran
#' @export
#' @examples
#' get_min_ver_incl_cran(pkgdepends::parse_pkg_ref("bioc::MultiAssayExperiment"))
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' get_min_ver_incl_cran(pkgdepends::parse_pkg_ref("cran::dplyr"))
get_min_ver_incl_cran.remote_ref <- function(remote_ref, op = "", op_ver = "") {
  get_min_ver(remote_ref, op, op_ver)
}

#' @rdname get_min_ver_incl_cran
#' @importFrom pkgdepends parse_pkg_ref
#' @export
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' get_min_ver_incl_cran(pkgdepends::parse_pkg_ref("cran/dplyr"))
get_min_ver_incl_cran.remote_ref_github <- function(remote_ref, op = "", op_ver = "") {
  if (check_if_on_cran(remote_ref)) {
    gh_res <- get_min_ver(remote_ref, op, op_ver)
    gh_desc <- get_desc_from_gh(gh_res$username, gh_res$repo, gh_res$commitish)
    gh_ver <- gh_desc$get_version()

    cran_remote_ref <- pkgdepends::parse_pkg_ref(sprintf("cran::%s", remote_ref$package))
    cran_res <- get_min_ver(cran_remote_ref, op, op_ver)
    cran_ver <- cran_res$version

    if (package_version(cran_ver) < package_version(gh_ver)) {
      return(cran_res)
    } else {
      return(gh_res)
    }
  } else {
    get_min_ver(remote_ref, op, op_ver)
  }
}

#' @importFrom pkgcache meta_cache_list
check_if_on_cran <- function(remote_ref) {
  nrow(pkgcache::meta_cache_list(remote_ref$package)) > 0
}

#' Get minimal version.
#'
#' @inherit get_min_ver_incl_cran
#'
#' @seealso [get_min_ver_incl_cran()]
#'
#' @export
get_min_ver <- function(remote_ref, op = "", op_ver = "") {
  UseMethod("get_min_ver", remote_ref)
}

#' @rdname get_min_ver
#' @export
#' @examples
#' get_min_ver(pkgdepends::parse_pkg_ref("bioc::MultiAssayExperiment"))
get_min_ver.remote_ref <- function(remote_ref, op = "", op_ver = "") {
  remote_ref
}

#' * for CRAN-type of remote - this would use [`pkgcache::cran_archive_list()`]
#' to obtain historical data.
#'
#' @rdname get_min_ver
#' @export
#' @importFrom pkgcache cran_archive_list meta_cache_list
#' @importFrom pkgdepends parse_pkg_ref
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' get_min_ver(pkgdepends::parse_pkg_ref("cran::dplyr"))
get_min_ver.remote_ref_cran <- function(remote_ref, op = "", op_ver = "") {
  if (remote_ref$atleast == "" && remote_ref$version != "") {
    return(remote_ref)
  }

  x_pkg_cache <- pkgcache::meta_cache_list(remote_ref$package)
  x_pkg_cache_archive <- pkgcache::cran_archive_list(package = remote_ref$package)
  pv <- unique(c(x_pkg_cache_archive$version, x_pkg_cache$version))
  pv <- setNames(package_version(pv), pv)
  pv <- filter_valid_version(pv, op, op_ver)
  min_ver <- Filter(function(x) x == min(pv), pv)

  new_ref <- sprintf("%s@%s", remote_ref$ref, names(min_ver)) # @TODO deparse, add ver, parse again
  pkgdepends::parse_pkg_ref(new_ref)
}

#' @rdname get_min_ver
#' @export
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' get_min_ver(pkgdepends::parse_pkg_ref("dplyr"))
get_min_ver.remote_ref_standard <- function(remote_ref, op = "", op_ver = "") {
  get_min_ver.remote_ref_cran(remote_ref, op, op_ver)
}

#' * for GitHub type of remote - this would use [`gh::gh_gql()`] to get list of all tags
#' and then [`gh::gh()`] to download `DESCRIPTION` file and then read package version.
#'
#' @rdname get_min_ver
#' @export
#' @importFrom pkgdepends parse_pkg_ref
#'
#' @examplesIf gh::gh_token() != ""
#' get_min_ver(pkgdepends::parse_pkg_ref("cran/dplyr"))
get_min_ver.remote_ref_github <- function(remote_ref, op = "", op_ver = "") {
  if (remote_ref$commitish != "") {
    return(remote_ref)
  }

  tags <- get_gh_tags(remote_ref$username, remote_ref$repo)

  if (length(tags) == 0) {
    return(remote_ref)
  }

  if (op == "") {
    # loop from the earliest tag and check if a valid description file
    for (tag in tags) {
      tag_desc <- get_desc_from_gh(remote_ref$username, remote_ref$repo, tag)
      if ((length(tag_desc) == 1 && is.na(tag_desc)) || tag_desc$get_field("Package") != remote_ref$package) next
      ref_suffix <- sprintf("@%s", tag)
      break
    }
  } else {
    # loop from the earliest tag, check if valid description file and check against version prespecified
    ref_suffix <- ""
    for (tag in tags) {
      tag_desc <- get_desc_from_gh(remote_ref$username, remote_ref$repo, tag)
      if ((length(tag_desc) == 1 && is.na(tag_desc)) || tag_desc$get_field("Package") != remote_ref$package) next
      tag_ver <- tag_desc$get_version()
      op_res <- do.call(op, list(tag_ver, package_version(op_ver)))
      if (isFALSE(op_res)) break
      ref_suffix <- sprintf("@%s", tag)
    }
  }

  new_ref <- sprintf("%s/%s%s", remote_ref$username, remote_ref$repo, ref_suffix) # @TODO
  pkgdepends::parse_pkg_ref(new_ref)
}


#' @importFrom gh gh_gql
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
get_desc_from_gh <- function(org, repo, ref = "") {
  if (ref == "") ref <- "HEAD"
  url_str <- sprintf("/repos/%s/%s/contents/DESCRIPTION?ref=%s", org, repo, ref)
  resp <- try(gh::gh(url_str, .accept = "application/vnd.github.v3.raw+json"), silent = TRUE)
  if (inherits(resp, "try-error")) {
    return(NA)
  }
  desc::desc(text = resp$message)
}

filter_valid_version <- function(x, op, op_ver) {
  res <- x
  res <- Filter(Negate(is.na), res)
  if (op != "" && op_ver != "") {
    res <- Filter(function(x) do.call(op, list(x, package_version(op_ver))), res)
  }
  return(res)
}
