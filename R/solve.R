#' Try to solve using standard method. If error - use [resolve_ignoring_release_remote].
#'
#' @inheritParams check_ip
#'
#' @returns `pkg_installation_plan` object invisibly
#'
#' @export
solve_ip <- function(ip) {
  UseMethod("solve_ip", ip)
}

#' @exportS3Method solve_ip deps_installation_proposal
solve_ip.deps_installation_proposal <- function(ip) {
  ip$solve()
  resolve_ignoring_release_remote(ip)
}

#' Try to solve using min_isolated method. If Error - use [resolve_ignoring_release_remote]
#'
#' For each direct dependency, resolve that package using PPM snapshot as of release date + 1.
#' Finally, combine resolutions and run solve.
#'
#' @keywords internal
#'
#' @exportS3Method solve_ip min_isolated_deps_installation_proposal
solve_ip.min_isolated_deps_installation_proposal <- function(ip) { # nolint
  ip$resolve()
  res <- ip$get_resolution()

  deps <- res[1, "deps"][[1]]
  ## copy op and version to Config\Needs\verdepcheck rows
  deps <- split(deps, as.factor(deps$package))
  deps <- lapply(deps, function(x) {
    x$op <- x$op[1]
    x$version <- x$version[1]
    x
  })

  deps <- do.call(rbind, deps)
  deps <- deps[tolower(deps$type) %in% tolower(res[1, "dep_types"][[1]]), ]

  # Avoid repeating calls to resolve_ppm_snapshot
  deps <- deps[!duplicated(deps[, c("ref", "op", "version")]), ]

  cli_pb_init("min_isolated", total = nrow(deps))

  deps_res <- lapply(seq_len(nrow(deps)), function(i) {
    i_pkg <- deps[i, "package"]

    cli_pb_update(package = i_pkg, n = 4L)

    if (i_pkg %in% base_pkgs()) {
      return(NULL)
    }

    tryCatch(
      resolve_ppm_snapshot(deps[i, "ref"], deps[i, "op"], deps[i, "version"]),
      error = function(err) NULL
    )
  })

  new_res <- do.call(rbind, deps_res)

  # Keep only top versions in calculated resolution (new_res).
  #  Very large resolution tables can become problematic and take a long to
  #  converge to a solution.
  new_res <- new_res[order(new_res$ref, package_version(new_res$version), decreasing = TRUE), ]
  new_res <- new_res[!duplicated(new_res[, c("ref")]), ]

  # Keep res at top
  new_res <- rbind(res[1:2, ], new_res)
  ip$.__enclos_env__$private$plan$.__enclos_env__$private$resolution$result <- new_res
  ip$solve()

  resolve_ignoring_release_remote(ip)

  return(invisible(ip))
}

#' If solution errors finishes with "dependency conflict" error then
#' re-try again ignoring "@*release" remote refs for detected conflicts.
#'
#' @inheritParams check_ip
#'
#' @inherit solve_ip return
#'
#' @keywords internal
resolve_ignoring_release_remote <- function(ip) { # nolint
  tryCatch(
    ip$stop_for_solution_error(),
    error = function(e) {
      if (!grepl("*.dependency conflict$", e$message)) stop(e)
      cat("Solve using alternative method ignoring `@*release` for conflicting refs.\n")
      solve_ip_ignore_remotes_release(ip)
      ip$stop_for_solution_error()
    }
  )
  return(invisible(ip))
}

#' Solve installation plan ignoring entries with "@*release" remote refs for detected conflicts.
#'
#' @inheritParams check_ip
#'
#' @inherit solve_ip return
#'
#' @keywords internal
solve_ip_ignore_remotes_release <- function(ip) { # nolint
  # replace "@*release" GH refs to the "@<ref for min ver>" for all direct dependent pkgs to avoid conflicts
  # use case:
  # foo -imports-> bar (>= 1.2.3) & baz (>= 1.2.3) (and has bar@*release and baz@*release in its Remotes)
  # bar -imports-> baz (and has baz@*release in its Remotes)
  # when doing min_deps we identify min version of baz to be 1.2.3
  # there is a conflict between baz@1.2.3 and baz@*release

  if (is.null(ip$.__enclos_env__$private$plan$.__enclos_env__$private$resolution$result)) ip$resolve()

  conflicting_pkgs <- resolution <- ip$get_resolution()

  conflicting_pkgs <- split(resolution, as.factor(conflicting_pkgs$package))
  conflicting_pkgs <- Filter(function(x) any(grepl("\\@\\*release", x$ref)), conflicting_pkgs)
  conflicting_pkgs <- Filter(function(x) length(unique(x$ref)) > 1, conflicting_pkgs)

  conflicting_pkgs_refs <- lapply(
    conflicting_pkgs,
    function(x) {
      c(
        package = x$package[1],
        old_ref = grep("\\@\\*release", x$ref, value = TRUE)[1],
        new_ref = grep("\\@\\*release", x$ref, value = TRUE, invert = TRUE)[1]
      )
    }
  )
  conflicting_pkgs_refs <- data.frame(do.call(rbind, conflicting_pkgs_refs), row.names = NULL)

  replace_using_df <- function(x, df) {
    for (i in seq_len(nrow(df))) {
      x <- replace(x, x == df[i, 1], df[i, 2])
    }
    x
  }
  for (i in seq_len(nrow(resolution))) {
    i_deps <- resolution[i, "deps"][[1]]
    if (any(i_deps$package %in% conflicting_pkgs_refs$package)) {
      i_deps$ref <- replace_using_df(i_deps$ref, conflicting_pkgs_refs[, c("old_ref", "new_ref")])
    }
    resolution[i, "deps"] <- list(list(i_deps))
  }

  ip$.__enclos_env__$private$plan$.__enclos_env__$private$resolution$result <- resolution

  ip$solve()

  return(invisible(ip))
}
