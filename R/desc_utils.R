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
#' d$set_list("Config/Needs/verdepcheck", "foo/baz") # not in package deps - will be skipped
#' get_refs_from_desc(d)
get_refs_from_desc <- function(d) {
  if (.desc_field %nin% d$fields()) {
    refs <- list()
  } else {
    refs <- lapply(get_desc_field_pkgs(d), pkgdepends::parse_pkg_ref)
  }
  all_deps <- d$get_deps()
  all_deps <- all_deps$package[all_deps$type %in% pkgdepends::pkg_dep_types()]
  missing_refs <- setdiff(
    setdiff(all_deps, base_pkgs()),
    map_key_character(refs, "package")
  )
  res <- c(
    refs,
    lapply(missing_refs, pkgdepends::parse_pkg_ref)
  )
  res_idx <- match(all_deps, map_key_character(res, "package"))
  res_idx <- res_idx[!is.na(res_idx)]
  res[res_idx]
}

#' Get the packages from the custom configuration field
#' @param d (`desc`) DESCRIPTION object from [desc::description]
#' @return character string
#' @keywords internal
get_desc_field_pkgs <- function(d) {
  if (!d$has_fields(.desc_field)) {
    return(character(0))
  }
  trimws(strsplit(d$get_field(.desc_field), ",")[[1]])
}

#' Replace Remotes in the `desc` that have been resolved to a GitHub tag or are
#' in CRAN.
#'
#' Replaces any existing Remotes entry with the resolved GitHub tag from
#' `Config/Needs/verdepcheck`.
#'
#' @param d (`desc`) DESCRIPTION object
#'
#' @importFrom pkgdepends parse_pkg_refs
#' @keywords internal
#' @examples
#' # Example that should replace dplyr & tibble on Remotes but not pkgdepends
#'
#' d <- desc::desc(
#'   file = verdepcheck:::local_description(
#'     list(
#'       dplyr = "Import",
#'       tibble = "Import",
#'       pkgdepends = "Import"
#'     ),
#'     remotes = c(
#'       "tidyverse/dplyr@*release",
#'       "tidyverse/tibble@*release",
#'       "r-lib/pkgdepends@*release"
#'     ),
#'     need_verdepcheck = c(
#'       "dplyr",
#'       "tibble=tidyverse/tibble@v3.2.1"
#'     )
#'   )
#' )
#' verdepcheck:::desc_remotes_cleanup(d)
desc_remotes_cleanup <- function(d) {
  if (length(get_desc_field_pkgs(d)) == 0) {
    return(d)
  }
  # Parse the `Config/Needs/verdepcheck` to retrieve references and extract package names
  desc_field_refs <- pkgdepends::parse_pkg_refs(get_desc_field_pkgs(d))
  desc_field_names <- map_key_character(desc_field_refs, "package")

  # Parse the remotes to retrieve the package names
  remotes_refs <- pkgdepends::parse_pkg_refs(d$get_remotes())
  remotes_names <- map_key_character(remotes_refs, "package")

  # Add to remotes `Config/Needs/verdepcheck` that resolve to a remote_ref_github
  desc_field_include_ix <- vapply(desc_field_refs, inherits, logical(1), "remote_ref_github")

  # Only keep previous remotes that are not defined in `Config/Needs/verdepcheck`
  remotes_include_ix <- remotes_names %in% setdiff(remotes_names, desc_field_names)

  # Create new list of references that will be used as "Remotes"
  new_remotes <- c(
    map_key_character(desc_field_refs[desc_field_include_ix], "ref"),
    map_key_character(remotes_refs[remotes_include_ix], "ref")
  )

  new_d <- d$clone()
  # Remove all remotes and override it
  new_d$clear_remotes()

  # Return clause without Remotes section if none should be kept
  if (is.null(new_remotes) || length(new_remotes) == 0) {
    return(new_d)
  }
  new_d$set_remotes(new_remotes)
  new_d
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
#' @importFrom pkgdepends new_pkg_installation_proposal
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
#' @param d (`desc`) DESCRIPTION object from [desc::description]
#' @param pkg_name (`character`) Package name
#' @keywords internal
#'
#' @examples
#' d <- desc::desc(cmd = "!new")
#'
#' d$set_dep("magrittr", type = "Imports", version = "*")
#' verdepcheck:::version_from_desc(d, "magrittr")
#'
#' d$set_dep("magrittr", type = "Imports", version = ">= 1.5")
#' verdepcheck:::version_from_desc(d, "magrittr")
version_from_desc <- function(d, pkg_name) {
  all_deps <- d$get_deps()

  version <- (all_deps$version[all_deps$package == pkg_name])[[1]]
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
#' versions <- paste(1:10, 0, sep = ".")
#' verdepcheck:::filter_valid_version(versions, ">=", "3.1")
filter_valid_version <- function(x, op, op_ver) {
  res <- Filter(Negate(is.na), numeric_version(x, strict = FALSE))
  if (op == "" || op_ver == "") {
    return(res)
  }
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
#' versions <- paste(1:10, 0, sep = ".")
#' verdepcheck:::check_valid_version(versions, ">=", "3.1")
check_valid_version <- function(x, op, op_ver) {
  res <- numeric_version(x, strict = FALSE)
  res <- Filter(Negate(is.na), res)
  if (op == "" || op_ver == "") {
    return(rep(TRUE, NROW(res)))
  }

  do.call(op, list(res, numeric_version(op_ver)))
}
