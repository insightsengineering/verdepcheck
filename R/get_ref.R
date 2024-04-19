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
#' verdepcheck:::check_if_on_cran(list(package = "dplyr"))
#' verdepcheck:::check_if_on_cran(list(package = "dplyr"), op = ">=", op_ver = "1.1.0")
#' verdepcheck:::check_if_on_cran(list(package = "dplyr"), op = ">=", op_ver = "9999.9.99")
#' verdepcheck:::check_if_on_cran(list(package = "dplyr"), op = "<", op_ver = "0.0.0")
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
#' @exportS3Method get_ref_min remote_ref
#' @examples
#' get_ref_min(pkgdepends::parse_pkg_ref("bioc::MultiAssayExperiment"))
get_ref_min.remote_ref <- function(remote_ref, op = "", op_ver = "") {
  remote_ref
}

#' * for standard and CRAN-type of remote - this would use [`pkgcache::cran_archive_list()`]
#' to obtain historical data.
#'
#' @rdname get_ref_min
#' @exportS3Method get_ref_min remote_ref_cran
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
  x_pkg_cache_archive <- pkgcache::cran_archive_list(package = remote_ref$package)
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
#' @exportS3Method get_ref_min remote_ref_standard
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' get_ref_min(pkgdepends::parse_pkg_ref("dplyr"))
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
#' get_ref_min(pkgdepends::parse_pkg_ref("cran/dplyr"))
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
#' @examples
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
  # create list of ref candidates to check
  # return the one of the highest version
  # this is a named list of character with version values and refs names
  ref_candidates <- list()
  if (check_if_on_cran(remote_ref)) {
    cran_ref <- remote_ref$package
    cran_ver <- get_version(pkgdepends::parse_pkg_ref(cran_ref))
    ref_candidates <- append(ref_candidates, setNames(list(cran_ver), cran_ref))
  }
  if (inherits(remote_ref, "remote_ref_github")) {
    gh_release_ref <- cond_parse_pkg_ref_release(remote_ref)
    gh_release_ver <- get_version(gh_release_ref)
    ref_candidates <- c(ref_candidates, setNames(list(gh_release_ver), gh_release_ref$ref))

    if (!is.null(remote_ref$commitish) && remote_ref$commitish != "") {
      gh_commitish_ref <- remote_ref
      gh_commitish_ver <- get_version(gh_commitish_ref)
      ref_candidates <- c(ref_candidates, setNames(list(gh_commitish_ver), gh_commitish_ref$ref))
    }
    if (!is.null(remote_ref$pull) && remote_ref$pull != "") {
      gh_pull_ref <- remote_ref
      gh_pull_ver <- get_version(gh_pull_ref)
      ref_candidates <- c(ref_candidates, setNames(list(gh_pull_ver), gh_pull_ref$ref))
    }
  } else {
    input_ref <- remote_ref$ref
    input_ver <- get_version(remote_ref)
    ref_candidates <- c(ref_candidates, setNames(list(input_ver), input_ref))
  }

  if (length(ref_candidates) == 0 || all(is.na(ref_candidates))) {
    return(remote_ref)
  } else {
    max_ver <- ref_candidates[[1]]
    max_ref <- names(ref_candidates[1])
    for (i in 2:length(ref_candidates)) {
      i_ver <- ref_candidates[[i]]
      i_ref <- names(ref_candidates[i])
      if (!is.na(i_ver) && i_ver > max_ver) {
        max_ref <- i_ref
      }
    }
    return(pkgdepends::parse_pkg_ref(max_ref))
  }
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
#' @exportS3Method get_version remote_ref
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' get_version(pkgdepends::parse_pkg_ref("dplyr"))
#' get_version(pkgdepends::parse_pkg_ref("tidyverse/dplyr"))
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
#' @exportS3Method get_release_date remote_ref_github
#' @importFrom gh gh_gql
#'
#' @examplesIf gh::gh_token() != ""
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

#' Get release date from GitHub references
#'
#' @rdname get_release_date
#' @exportS3Method get_release_date remote_ref_cran
#'
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' remote_ref <- pkgdepends::parse_pkg_ref("dplyr@1.1.0")
#' get_release_date.remote_ref_cran(remote_ref)
get_release_date.remote_ref_cran <- function(remote_ref) {
  result <- subset(
    get_cran_data(remote_ref$package),
    package_version(version, strict = FALSE) == package_version(remote_ref$version, strict = FALSE),
    select = "mtime"
  )[[1]][1]
  as.Date(result)
}

#' @export
get_release_date.remote_ref_standard <- function(remote_ref) {
  get_release_date.remote_ref_cran(remote_ref)
}

#' @export
get_release_date.remote_ref <- function(remote_ref) {
  as.Date(NA_real_)
}

#' Get CRAN/Bioconductor metadata information on packages
#'
#' @importFrom pkgcache cran_archive_list meta_cache_list
#' @keywords internal
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' verdepcheck:::get_cran_data("dplyr")
#' verdepcheck:::get_cran_data("SummarizedExperiment")
get_cran_data <- function(package) {
  cran_archive <- pkgcache::cran_archive_list(packages = package)[, c(
    "package", "version", "mtime"
  )]
  cran_current <- pkgcache::meta_cache_list(packages = package)[, c(
    "type", "package", "version", "published"
  )]
  if (all(is.na(cran_current$published))) {
    # workaround of https://github.com/r-lib/pkgcache/issues/109
    if (is.null(pkgenv$cache_db)) {
      pkgenv$cache_db <- tools::CRAN_package_db()
    }
    db <- subset(pkgenv$cache_db, pkgenv$cache_db$Package == package)
    cran_current <- data.frame(
      type = "cran",
      package = package,
      version = db$Version,
      published = as.POSIXct(db$`Date/Publication`)
    )
  }

  # Bioc custom logic as packages in Bioconductor do not return a published date
  #  this will be immediately obsolete if {pkgcache} starts to return a non-NA value
  #  note: a date is required for the `min_cohort` strategy
  bioc_na_mtime_ix <- is.na(cran_current$published) & cran_current$type == "bioc"
  if (NROW(cran_current[bioc_na_mtime_ix, ]) > 0) {
    cran_current[bioc_na_mtime_ix, "published"] <- Sys.Date()
  }

  # Remove extra columns
  cran_current <- cran_current[, setdiff(names(cran_current), c("type"))]

  cran_current <- setNames(cran_current, names(cran_archive))
  rbind(cran_archive, cran_current)
}
