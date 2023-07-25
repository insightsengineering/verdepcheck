#' Read DESCRIPTION file and return list of references.
#'
#' Returned list is an union between references specified in `"Config/Needs/verdepcheck"` field and
#' standard references for all other not covered dependencies.
#' @importFrom pkgdepends pkg_dep_types parse_pkg_ref
#' @keywords internal
#' @examplesIf Sys.getenv("R_USER_CACHE_DIR", "") != ""
#' d <- desc::desc("!new")
#' d$set_dep("foo", "Import")
#' d$set_dep("bar", "Suggest")
#' d$set_list("Config/Needs/verdepcheck", "foo/bar")
#' d$set_list("Config/Needs/verdepcheck", "foo/baz") # not in pacakge deps - will be skipped
#' get_refs_from_desc(d)
get_refs_from_desc <- function(d) {
  if (.desc_field %nin% d$fields()) {
    refs <- list()
  } else {
    refs <- lapply(
      trimws(strsplit(d$get_field(.desc_field), ",")[[1]]),
      pkgdepends::parse_pkg_ref
    )
  }
  all_deps <- subset(d$get_deps(), type %in% pkgdepends::pkg_dep_types(), "package")[[1]]
  missing_refs <- setdiff(setdiff(all_deps, base_pkgs()), vapply(refs, `[[`, character(1), "package"))
  res <- c(
    refs,
    lapply(missing_refs, pkgdepends::parse_pkg_ref)
  )
  res_idx <- match(all_deps, vapply(res, `[[`, character(1), "package"))
  res_idx <- res_idx[!is.na(res_idx)]
  res[res_idx]
}

#' Replace Remotes in the `desc` that have been resolved to a GitHub tag or are
#' in CRAN
#'
#' Replaces any existing Remotes entry with the resolved GitHub tag from the
#' `new_refs`.
#' It keeps all the existing Remotes that have not been resolved in `new_refs`.
#'
#' @param d (`desc`) DESCRIPTION object
#' @param new_refs (`list`) remote references that have been resolved and are
#' being updated in `Config/Needs/verdepcheck`
#' @keywords internal
desc_remotes_cleanup <- function(d, new_refs) {
  # Parse the remotes to retrieve the package names
  remotes <- pkgdepends::parse_pkg_refs(d$get_remotes())

  # Get the packages defined in remotes
  #  (making sure that only packages that are already defined here are modified)
  remotes_pkg <- vapply(remotes, `[[`, character(1), "package")

  # Find which packages of the new_refs are defined in Remotes
  new_refs_remotes <- Filter(
    function(.x) {
      isTRUE(.x$package %in% remotes_pkg) && inherits(.x, "remote_ref_github")
    },
    new_refs
  )

  # New remotes ref to use when replacing
  new_ref_remote <- vapply(new_refs_remotes, `[[`, character(1), "ref")

  new_ref_pkg <- vapply(new_refs, `[[`, character(1), "package")

  # Remove from `Remotes` all package that have been resolved to
  #  * CRAN package
  #  * GitHub tag
  new_remotes <- c(
    # Keep remotes (if the DESCRIPTION file is correct, this should have no elements)
    d$get_remotes()[!(remotes_pkg %in% new_ref_pkg)],
    # Modified remotes
    new_ref_remote
  )

  # Remotes that are not in new_refs are kept, as well as the ones that were
  #  resolved to be a github repo
  d$clear_remotes()

  # Return clause without Remotes section
  if (is.null(new_remotes) || length(new_remotes) == 0) return(d)
  d$set_remotes(new_remotes)
  d
}

#' Set `"Config/Needs/verdepcheck"` section into the `desc` object if not empty else clear this section.
#' @keywords internal
desc_cond_set_refs <- function(d, refs) {
  if (length(refs)) {
    d$set_list(.desc_field, refs)
  } else {
    d$del(.desc_field)
  }
  return(invisible(d))
}

#' Create `installation_plan` object from `desc` object
#' @importFrom pkgdepends new_pkg_deps new_pkg_installation_proposal
#' @keywords internal
desc_to_ip <- function(d, config) {
  temp_desc <- tempfile()
  d$write(temp_desc)

  pkgdepends::new_pkg_installation_proposal(
    refs = paste0("deps::", temp_desc),
    config = config
  )
}

#' Get package version from description
#'
#' @keywords internal
#'
#' @examples
#' d <- desc::desc(cmd = "!new")
#'
#' d$set_dep("magrittr", type = "Imports", version = "*")
#' verdepcheck:::version_from_desc("magrittr", d)
#'
#' d$set_dep("magrittr", type = "Imports", version = ">= 1.5")
#' verdepcheck:::version_from_desc("magrittr", d)
version_from_desc <- function(pkg_name, desc) {
  version <- subset(desc$get_deps(), package == pkg_name, version)[[1]]
  result <- list(
    package = pkg_name,
    version_str = version,
    op = "",
    op_ver = ""
  )
  if (version == "*" || trimws(version) == "") {
    return(result)
  }
  split_vec <- strsplit(version, " ")[[1]]
  result$version_str <- version
  result$op <- split_vec[1]
  result$op_ver <- split_vec[2]
  result
}

#' Filter for package versions that comply with an operator and version
#'
#' @param x `vector` of valid package versions.
#' @param op `character(1)` relational operator (`>=`, `==`, ...)
#' @param op_ver `character(1)` or `package_version(1)` with version to compare
#' with using a relational operator.
#'
#' @keywords internal
#'
#' @examples
#' versions <- paste(1:10, rep("0", 10), sep = ".")
#' verdepcheck:::filter_valid_version(versions, ">=", "3.1")
filter_valid_version <- function(x, op, op_ver) {
  res <- package_version(x)
  res <- Filter(Negate(is.na), res)
  if (op == "" || op_ver == "") return(res)

  Filter(function(x) check_valid_version(x, op, op_ver), res)
}

#' Check for package versions that comply with an operator and version
#'
#' @param x `vector` of valid package versions.
#' @param op `character(1)` relational operator (`>=`, `==`, ...)
#' @param op_ver `character(1)` or `package_version(1)` with version to compare
#' with using a relational operator.
#'
#' @keywords internal
#'
#' @examples
#' versions <- paste(1:10, rep("0", 10), sep = ".")
#' verdepcheck:::check_valid_version(versions, ">=", "3.1")
check_valid_version <- function(x, op, op_ver) {
  res <- package_version(x, strict = FALSE)
  res <- Filter(Negate(is.na), res)
  if (op == "" || op_ver == "") return(res)

  do.call(op, list(res, package_version(op_ver)))
}
