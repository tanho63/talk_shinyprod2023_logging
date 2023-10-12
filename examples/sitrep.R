#' Get a Situation Report ("sitrep") on System, Internal Package Versions and Dependencies
#'
#' This function gives a quick overview of the versions of R and the operating
#' system as well as the versions of internal packages, options, and their dependencies.
#' It's primarily designed to help triage issues and identify whether the problem
#' is outdated versions or missing options/environment variables.
#'
#' Adapted from past work:
#' <https://github.com/nflverse/nflreadr/blob/main/R/utils_sitrep.R>
#'
#' @param pkg a character vector naming packages to check, defaulting to `.internal_packages()`
#'
#' @param dev_repos URLs of cran-like private repositories for the packages in
#' question, e.g. r-universe, drat type repos. This is used to identify whether
#' the installed version of internal packages are outdated.
#'
#' @return "sitrep" list object containing information about packages, that can be
#' pretty-printed via print() or converted to json via `jsonlite::toJSON()` for
#' other uses
#'
#' @examples
#' \donttest{
#'   sitrep()
#' }
#' @export
sitrep <- function(pkg = .internal_packages(), dev_repos = .dev_repos()) {
  rlang::check_installed(c("data.table","cli","glue","curl"))
  #' optional: force pkg to come from .internal_packages()
  pkg <- rlang::arg_match(pkg, values = .internal_packages(), multiple = TRUE)

  .sitrep(pkg)
}

#' list of internal packages
.internal_packages <- function() {
  pkg <- c(
    "nflreadr",
    "nflfastR"
  )
}

.dev_repos <- function(){
  c("https://nflverse.r-universe.dev")
}

.sitrep <- function(pkg, dev_repos = .dev_repos()) {
  out <- structure(
    list(
      system_info = list(),
      env_vars = list(),
      installed = list(),
      package_options = list(),
      not_installed = list(),
      dependencies = list(),
      packages = pkg,
      timestamp = Sys.time()
    ),
    class = c("sitrep", "list")
  )
  # OPERATING SYSTEM INFO
  out$system_info <- list(
    r_version = R.version$version.string,
    os_version = utils::osVersion
  )

  out$env_vars <- .sitrep_env_vars()

  packages <- pkg
  inst_pkgs <- data.table::as.data.table(utils::installed.packages())

  out$not_installed <- packages[!packages %in% inst_pkgs$Package]
  packages <- packages[packages %in% inst_pkgs$Package]

  if (length(packages) == 0) return(out)

  out$installed <- .sitrep_pkg_status(packages = packages,
                                      inst_pkgs = inst_pkgs,
                                      dev_repos = dev_repos,
                                      check_latest = TRUE)

  # Relies on options for a given package being prefixed by said package name
  out$package_options <- .sitrep_pkg_opts(packages)

  out$dependencies <- .sitrep_pkg_deps(packages = packages,
                                       inst_pkgs = inst_pkgs,
                                       recursive = TRUE)

  return(out)
}

.sitrep_pkg_status <- function(packages,
                               inst_pkgs = utils::installed.packages(),
                               dev_repos = .dev_repos(),
                               check_latest = curl::has_internet()){

  inst_pkgs <- data.table::as.data.table(inst_pkgs)
  Package <- Version <- NULL
  inst <- inst_pkgs[Package %in% packages][, list(package = Package, installed = Version, cran = NA, dev = NA, behind = NA)]
  if (!curl::has_internet() || !check_latest) return(as.data.frame(inst))

  cran_repos <- c(
    "https://packagemanager.posit.co/cran/latest",
    "https://cloud.r-project.org",
    "https://cran.rstudio.com"
  )
  cran_pkgs <- data.table::as.data.table(utils::available.packages(repos = cran_repos))[Package %in% packages, list(package = Package, cran = Version)]
  dev_pkgs <- data.table::as.data.table(utils::available.packages(repos = dev_repos))[Package %in% packages, list(package = Package, dev = Version)]

  package <- installed <- cran <- dev <- behind_cran <- behind_dev <- behind <- NULL

  inst <- merge(
    merge(
      inst[, list(package, installed)],
      cran_pkgs,
      by = "package",
      all = TRUE
    ),
    dev_pkgs,
    all = TRUE,
    by = "package"
  )[
    , list(
      package, installed, cran, dev,
      behind_cran = ifelse(!is.na(cran), package_version(installed, strict = FALSE) < package_version(cran, strict = FALSE), FALSE),
      behind_dev = ifelse(!is.na(dev), package_version(installed, strict = FALSE) < package_version(dev, strict = FALSE), FALSE)
    )
  ][
    , list(
      package, installed, cran, dev,
      behind = paste0(ifelse(behind_cran, "cran;", ""), ifelse(behind_dev, "dev", "")),
      behind_cran = NULL,
      behind_dev = NULL
    )
  ]

  return(as.data.frame(inst))
}

.sitrep_env_vars <- function(pattern = "^ZELUS|SHINYPROXY"){
  env_vars <- Sys.getenv()
  # Search for common prefix patterns
  env_vars <- env_vars[grepl(pattern, names(env_vars), ignore.case = TRUE)]
  env_vars <- env_vars[names(env_vars) != "ZELUS_ENV"]
  # redact confidential tokens
  token_vars <- grepl("TOKEN", names(env_vars), ignore.case = TRUE)
  env_vars[token_vars] <- sapply(env_vars[token_vars], .redact_string)
  return(env_vars)
}

.redact_string <- \(x) {
  l <- nchar(x)
  if (l <= 8) {
    return("<redacted>")
  }
  paste0(substr(x, 1, 3), "..<redacted>..", substr(x, l - 2, l))
}

#' Show options for specified packages
#' @keywords internal
.sitrep_pkg_opts <- function(packages, redact = TRUE){
  opts <- options()
  pkg_search_string <- paste(packages, collapse = "|")
  package_options <- opts[grepl(pkg_search_string, x = names(opts))]
  return(package_options)
}

#' Show dependency versions of specified packages
#' @keywords internal
.sitrep_pkg_deps <- function(packages, inst_pkgs = utils::installed.packages(), recursive = TRUE){

  inst_pkgs <- data.table::as.data.table(inst_pkgs)
  .flatten <- function(x) sort(unique(unlist(x, use.names = FALSE)))
  deps <- .flatten(tools::package_dependencies(packages, db = inst_pkgs, recursive = recursive))

  deps <- deps[!deps %in% packages]
  missing_pkgs <- setdiff(deps, inst_pkgs$Package)

  dep_status <- inst_pkgs[
    inst_pkgs$Package %in% deps
  ][
    , c("Package", "Version")
  ][
    , priority := seq_len(.N)
    , by = Package
  ][
    priority == 1
  ][
    order(Package)
  ][
    , priority := NULL
  ]
  dep_status <- unique(dep_status)
  data.table::setnames(dep_status, c("package","version"))

  if(length(missing_pkgs) > 0) {
    dep_status <- data.table::rbindlist(
      list(
        dep_status,
        data.table::data.table(package = missing_pkgs, version = "missing")
      )
    )
  }

  return(as.data.frame(dep_status))
}

#' sitrep data class
#'
#' `sitrep()` returns an S3 class that was created to allow for custom
#' printing. It will otherwise dispatch to the `list` class.
#'
#' Re-exporting with `methods::setOldClass()` allows these dataframes to be used
#' in S4 generics like those from DBI and jsonlite.
#'
#' @name sitrep-data-class
#'
#' @exportClass sitrep
methods::setOldClass("sitrep", "list")

#' Print method for sitrep
#' @export
print.sitrep <- function(x, ...) {
  cli::cat_rule(cli::style_bold("System Info"), col = cli::make_ansi_style("cyan"), line = 1)
  cli::cat_bullet(glue::glue("{x$system_info$r_version} {cli::symbol$bullet} Running under: {x$system_info$os_version}")) # nolint

  cli::cat_rule(cli::style_bold("Environment Variables"), col = cli::make_ansi_style("cyan"), line = 1)
  .cat_options(names(x$env_vars), unname(x$env_vars))

  cli::cat_rule(cli::style_bold("Package Status"), col = cli::make_ansi_style("cyan"), line = 1)
  if(nrow(x$installed) > 0) {
    print(x$installed)
  }

  cli::cat_rule(cli::style_bold("Package Options"), col = cli::make_ansi_style("cyan"), line = 1)
  if (length(x$package_options) == 0) cli::cat_bullet("No options set for above packages")
  if (length(x$package_options) > 0) .cat_options(names(x$package_options), unname(x$package_options))

  if (length(x$dependencies) >= 1) {
    cli::cat_rule(cli::style_bold("Package Dependencies"), col = cli::make_ansi_style("cyan"), line = 1)
    .cat_pkg(x$dependencies$package, x$dependencies$version)
  }

  if (length(x$not_installed) >= 1) {
    cli::cat_rule(cli::style_bold("Not Installed"), col = cli::make_ansi_style("cyan"), line = 1)
    .cat_pkg(x$not_installed, rep_len("", length(x$not_installed)))
  }

  cli::cat_rule(col = cli::make_ansi_style("cyan"), line = 1)

  return(invisible(x))
}

.cat_pkg <- function(packages, versions) {
  stopifnot(length(packages) == length(versions))

  if (length(packages) <= 2) {
    cli::cat_bullet(glue::glue("{format(packages)} ({format(versions)})"))
    return(invisible())
  }

  l <- length(packages)

  breaks <- cut(
    x = seq_along(packages),
    breaks = c(0, ceiling(l / 3), 2 * ceiling(l / 3), l + 1)
  )

  p <- split(packages, breaks)
  v <- split(versions, breaks)

  r <- length(p[[1]]) - length(p[[3]])

  if (r != 0) {
    p[[3]] <- c(p[[3]], rep("", r))
    v[[3]] <- c(v[[3]], rep("", r))
  }

  p <- lapply(p, function(x) ifelse(x != "", format(paste(cli::symbol$bullet, x)), ""))
  v <- lapply(v, function(x) ifelse(x != "", format(paste0(" (", x, ")")), ""))

  cli::cat_line(
    paste0(
      p[[1]], v[[1]], "  ",
      p[[2]], v[[2]], "  ",
      p[[3]], v[[3]], "  "
    )
  )
}

.cat_options <- function(option_names, option_values) {
  stopifnot(length(option_names) == length(option_values))
  cli::cat_bullet(glue::glue("{format(option_names)}: {format(option_values)}"))
}
