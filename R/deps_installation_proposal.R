#' Create installation proposal using various dependency strategies
#'#'
#' These functionalities would read local package `DESCRIPTION` file, retrireve dependencies and
#' create an installation proposal using various strategies for package versions as described below.
#'
#' @rdname deps_installation_proposal
#'
#' @section strategies:
#' Currently implemented strategies:
#' * `max` - use the greatest version of dependencies.
#' * `release` - read `Remotes` field and for each GitHub type of the package source replace any futher
#' reference (i.e. `@<commitish>` `#<pr>` or `@*release`) with a tag associated with the latest release.
#' Please note that `@*release` endpoint in `Remotes` is not supported by `pkgdepends`.
#' * `min` - use the lowest version of dependencies. If no version is specified then the minimal available
#' version is assumed. See [find_minver_remote_ref] for details how the minimal version is determined.
#'
#' Any modification is done for _direct_ dependencies. Indirect ones are installed as usual.
#'
#' @note Some functions are supported only for package remotes from GitHub.
#'
#' @param path (`string`) path to the package sources
#' @param config (`list`) configuration options. See [`pkgdepends::pkg_config`] for details. If it does not include
#' `library` then temporary dir is used which simulates clean environment without using any pre-installed packages.
#' If it does not include `dependencies` then `TRUE` value is used which means all hard dependencies plus `Suggests`.
#'
#' @returns `pkg_installation_plan` object
#'
#' @seealso [`pkgdepends::pkg_installation_proposal`]
#'
#' @export
#' @importFrom pkgdepends new_pkg_installation_proposal
#'
#' @examples
#' x <- new_max_deps_installation_proposal(".")
#' x$solve()
#' x$get_solution()
new_max_deps_installation_proposal <- function(path, config = list()) { # nolint
    path <- normalizePath(path)

    if (!("dependencies" %in% names(config))) config$dependencies <- TRUE
    if (!("library" %in% names(config))) {
        config$library <- tempfile()
        on.exit(unlink(config$library), add = TRUE, after = FALSE)
    }

    pkgdepends::new_pkg_installation_proposal(
        refs = paste0("deps::", path),
        config = config,
        policy = "upgrade"
    )
}

#' @rdname deps_installation_proposal
#' @export
#' @importFrom desc desc
#' @importFrom pkgdepends new_pkg_installation_proposal parse_pkg_ref
#' @importFrom remotes github_remote
#' @examples
#' x <- new_release_deps_installation_proposal(".")
#' x$solve()
#' x$get_solution()
new_release_deps_installation_proposal <- function(path, config = list()) { # nolint
    path <- normalizePath(path)

    if (!("dependencies" %in% names(config))) config$dependencies <- TRUE
    if (!("library" %in% names(config))) {
        config$library <- tempfile()
        on.exit(unlink(config$library), add = TRUE, after = FALSE)
    }

    d <- desc::desc(path)
    new_remotes <- vapply(
        d$get_remotes(),
        function(x) {
            if (is(x_parsed <- pkgdepends::parse_pkg_ref(x), "remote_ref_github")) {
                release_ref <- remotes::github_remote(sprintf("%s/%s@*release", x_parsed$username, x_parsed$repo))$ref
                sprintf("%s/%s@%s", x_parsed$username, x_parsed$repo, release_ref)
            } else {
                x
            }
        },
        character(1),
        USE.NAMES = FALSE
    )
    if (length(new_remotes)) {
        d$set_remotes(new_remotes)
    } else {
        d$clear_remotes()
    }

    temp_desc <- tempfile()
    on.exit(unlink(temp_desc), add = TRUE, after = FALSE)
    d$write(temp_desc)

    pkgdepends::new_pkg_installation_proposal(
        refs = paste0("deps::", temp_desc),
        config = config
    )
}

#' @rdname deps_installation_proposal
#' @export
#' @importFrom desc desc
#' @importFrom pkgdepends new_pkg_deps new_pkg_installation_proposal pkg_dep_types parse_pkg_ref
#' @examples
#' x <- new_min_deps_installation_proposal(".")
#' x$solve()
#' x$get_solution()
new_min_deps_installation_proposal <- function(path, config = list()) { # nolint
    path <- normalizePath(path)

    if (!("dependencies" %in% names(config))) config$dependencies <- TRUE
    if (!("library" %in% names(config))) {
        config$library <- tempfile()
        on.exit(unlink(config$library), add = TRUE, after = FALSE)
    }

    x <- pkgdepends::new_pkg_deps(
        refs = path,
        config = config,
        policy = "lazy"
    )
    x$solve()
    x$stop_for_solution_error()
    deps <- x$get_solution()$data$deps[[1]]

    deps <- subset(deps, !(package %in% c("R", rownames(installed.packages(priority = "base")))))

    deps$ref_parsed <- lapply(deps$ref, pkgdepends::parse_pkg_ref)

    deps$ref_minver <- mapply( # @TODO: add cli progress bar
        find_minver_remote_ref,
        remote_ref = deps$ref_parsed,
        op = deps$op,
        op_ver = deps$version,
        SIMPLIFY = FALSE
    )

    # @TODO: wait for https://github.com/r-lib/pak/issues/122
    # as a suggested workaround - use GH mirror of CRAN
    deps$ref_minver <- lapply(
        deps$ref_minver,
        function(x) {
            if (is(x, "remote_ref_standard") || is(x, "remote_ref_cran")) {
                new_ref <- sprintf("cran/%s", gsub("cran::|standard::", "", x$ref))
                pkgdepends::parse_pkg_ref(new_ref)
            } else {
                x
            }
        }
    )

    refs <- vapply(deps$ref_minver, `[[`, character(1), "ref")

    d <- desc::desc(path)
    if (length(refs)) {
        d$set_remotes(refs)
    } else {
        d$clear_remotes()
    }

    temp_desc <- tempfile()
    d$write(temp_desc)

    pkgdepends::new_pkg_installation_proposal(
        refs = paste0("deps::", temp_desc),
        config = config
    )
}
