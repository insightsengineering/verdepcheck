
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
      get_desc_field_pkgs(d),
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

#' Get the packages from the custom config field
#' @param d (`desc`) DESCRIPTION object from [desc::description]
#' @return character string
#' @keywords internal
get_desc_field_pkgs <- function(d) {
  trimws(strsplit(d$get_field(.desc_field), ",")[[1]])
}

#' Replace Remotes in the `desc` that have been resolved to a GitHub tag or are
#' in CRAN
#'
#' Replaces any existing Remotes entry with the resolved GitHub tag from
#' `Config/Needs/verdepcheck`.
#'
#' @param d (`desc`) DESCRIPTION object
#' @keywords internal
#' @examples
#' # Example that should replace rtables & formatters on Remotes
#' #  but not pkgdepends
#'
#' d <- desc::desc(
#'   file = verdepcheck:::local_description(
#'     list(
#'       rtables = "Import", formatters = "Import", pkgdepends = "Import",
#'       dplyr = "Import"
#'     ),
#'     remotes = c(
#'       "insightsengineering/rtables@*release",
#'       "insightsengineering/formatters@*release",
#'       "r-lib/pkgdepends@*release"
#'     ),
#'     need_verdepcheck = c(
#'       "dplyr",
#'       "rtables=insightsengineering/rtables@0.6.2",
#'       "formatters=insightsengineering/formatters@0.5.1"
#'     )
#'   )
#' )
#' verdepcheck:::desc_remotes_cleanup(d)
desc_remotes_cleanup <- function(d) {
  # Parse the `Config/Needs/verdepcheck` to retrieve references and extract package names
  desc_field_pkgs <- pkgdepends::parse_pkg_refs(get_desc_field_pkgs(d))
  desc_field_pkg_names <- map_key_character(desc_field_pkgs, "package")

  # Parse the remotes to retrieve the package names
  remotes <- pkgdepends::parse_pkg_refs(d$get_remotes())
  remotes_pkg <- map_key_character(remotes, "package")

  # Add to remotes `Config/Needs/verdepcheck` that resolve to a remote_ref_github
  desc_field_include_ix <- vapply(desc_field_pkgs, inherits, logical(1), "remote_ref_github")

  # Only keep previous remotes that are not defined in `Config/Needs/verdepcheck`
  remotes_include_ix <- remotes_pkg %in% setdiff(remotes_pkg, desc_field_pkg_names)

  # Create new list of references that will be used as "Remotes"
  new_remotes <- c(
    map_key_character(desc_field_pkgs[desc_field_include_ix], "ref"),
    map_key_character(remotes[remotes_include_ix], "ref")
  )

  # Remove all remotes and override it
  d$clear_remotes()

  # Return clause without Remotes section if none should be kept
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
  subset_versions <- function(.x) (.x$version[.x$package == pkg_name])[[1]]
  version <- subset_versions(desc$get_deps())
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
  res <- Filter(Negate(is.na), numeric_version(x, strict = FALSE))
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
  res <- numeric_version(x, strict = FALSE)
  res <- Filter(Negate(is.na), res)
  if (op == "" || op_ver == "") return(rep(TRUE, NROW(res)))

  do.call(op, list(res, numeric_version(op_ver)))
}
