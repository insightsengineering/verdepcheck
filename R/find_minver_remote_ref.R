#' Identify minimal version.
#'
#' @param remote_ref (`remote_ref`) object created with [`pkgdepends::parse_pkg_ref()`]
#' @param pacakge (`character(1)`) optional, name of the package
#' @param op (`character(1)`) optional, version condition comparison operator (e.g. `">"`, `">="`)
#' @param op_ver (`character(1)`) optional, version number against which `op` argument is applied
#'
#' @returns (`remote_ref`) object with reference to the package of minimal version.
#'
#' @note only `cran`, `standard` (treated as `cran`) and `github` remote type are supported.
#' For the rest, it would return `remote_ref` argument.
#'
#' @export
find_minver_remote_ref <- function(remote_ref, op = "", op_ver = "") {
  UseMethod("find_minver_remote_ref", remote_ref)
}

#' @rdname find_minver_remote_ref
#' @export
#' @examples
#' find_minver_remote_ref(pkgdepends::parse_pkg_ref("bioc::MultiAssayExperiment"))
find_minver_remote_ref.remote_ref <- function(remote_ref, op = "", op_ver = "") {
  remote_ref
}

#' Identify minimal version for CRAN-type of remote. This would use [`pkgcache::cran_archive_list()`]
#' to obtain historical data.
#'
#' @rdname find_minver_remote_ref
#' @export
#' @importFrom pkgcache cran_archive_list meta_cache_list
#' @importFrom pkgdepends parse_pkg_ref
#' @examples
#' find_minver_remote_ref(pkgdepends::parse_pkg_ref("cran::dplyr"))
find_minver_remote_ref.remote_ref_cran <- function(remote_ref, op = "", op_ver = "") {
  if (remote_ref$atleast == "" && remote_ref$version != "") {
    return(remote_ref)
  }

  x_pkg_cache <- pkgcache::meta_cache_list(package)
  x_pkg_cache_archive <- pkgcache::cran_archive_list(package = package)
  pv <- package_version(unique(c(x_pkg_cache$version, x_pkg_cache_archive$version)))
  pv <- filter_valid_version(pv, op, op_ver)
  pv <- filter_valid_version(pv, op, op_ver)
  min_ver <- min(pv)

  new_ref <- sprintf("%s@%s", remote_ref$ref, min_ver) # @TODO deparse, add ver, parse again
  pkgdepends::parse_pkg_ref(new_ref)
}

#' @rdname find_minver_remote_ref
#' @export
#' @examples
#' find_minver_remote_ref(pkgdepends::parse_pkg_ref("dplyr"))
find_minver_remote_ref.remote_ref_standard <- function(remote_ref, op = "", op_ver = "") {
  find_minver_remote_ref.remote_ref_cran(remote_ref, package, op, op_ver)
}

#' Identify minimal version of GitHub type of remote.
#'
#' @rdname find_minver_remote_ref
#' @export
#' @importFrom pkgdepends parse_pkg_ref
#'
#' @seealso `gh::gh()` for other configuration possibilities
#'
#' @examples
#' find_minver_remote_ref(pkgdepends::parse_pkg_ref("cran/dplyr"))
find_minver_remote_ref.remote_ref_github <- function(remote_ref, op = "", op_ver = "") {
  if (remote_ref$commitish != "") {
    return(remote_ref)
  }

  tags <- get_gh_tags(remote_ref$username, remote_ref$repo)

  if (length(tags) == 0) {
    return(remote_ref)
  }

  if (op == "") {
    min_tag <- tags[1]
  } else {
    # it's needed to start from the end (i.e. latest tags) as there are many unexpected things in the history
    # e.g. r-lib/styler decreased package version in the past
    for (tag in rev(tags)) {
      tag_ver <- get_ver_from_gh(remote_ref$username, remote_ref$repo, tag)
      op_res <- do.call(op, list(tag_ver, package_version(op_ver)))
      if (isFALSE(op_res)) break
      min_tag <- tag
    }
  }

  new_ref <- sprintf("%s/%s@%s", remote_ref$username, remote_ref$repo, min_tag) # @TODO
  pkgdepends::parse_pkg_ref(new_ref)
}


#' @importFrom gh gh
get_gh_tags <- function(org, repo) {
  url_str <- sprintf("/repos/%s/%s/git/refs/tags", org, repo)
  resp <- try(gh::gh(url_str, .limit = Inf), silent = TRUE)
  if (inherits(resp, "try-error")) {
    return(character(0))
  }
  gsub("^refs/tags/", "", vapply(resp, `[[`, character(1), "ref"))
}
#' @importFrom desc desc
#' @importFrom gh gh
get_ver_from_gh <- function(org, repo, ref = "HEAD") {
  url_str <- sprintf("/repos/%s/%s/contents/DESCRIPTION?ref=%s", org, repo, ref)
  resp <- try(gh::gh(url_str, .accept = "application/vnd.github.v3.raw+json"), silent = TRUE)
  if (inherits(resp, "try-error")) {
    return(NA)
  }
  desc::desc(text = resp$message)$get_version()
}
