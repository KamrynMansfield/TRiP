# zzz.R -------------------------------------------------------------------
# Package load hooks. Nothing here is exported or called directly by the user;
# R runs `.onLoad()` automatically when the TRiP namespace is loaded.

#' Package load hook
#'
#' Registers the static asset directory so that the Shiny UI can reference
#' images and other files with `src = "www/..."` (for example the
#' `www/dataexample.png` screenshot shown on the Ridership Data Upload tab).
#'
#' @param libname The library directory where the package is installed.
#'   Supplied automatically by R; not used here.
#' @param pkgname The name of the package being loaded ("TRiP"). Supplied
#'   automatically by R.
#'
#' @returns Called for its side effect. Returns the resource path invisibly.
#'
#' @keywords internal
#' @noRd
.onLoad <- function(libname, pkgname) {
  # Maps the installed inst/app/www folder to the "/www" URL prefix used in app_ui().
  shiny::addResourcePath("www", system.file("app/www", package = pkgname))
}
