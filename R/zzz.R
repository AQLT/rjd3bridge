#' @noRd
#' @keywords internal
#' @importFrom rJava .jpackage .jaddClassPath
#' @importFrom rjd3jars reload_dictionaries reload_tsproviders
#' @importFrom rjd3jars reload_safactories check_java_version
.onLoad <- function(libname, pkgname) {
  # if (!requireNamespace("rjd3filters", quietly = TRUE)) stop("Loading rjd3 libraries failed")
  # if (!requireNamespace("rjd3x13", quietly = TRUE)) stop("Loading rjd3 libraries failed")

  # result <- rJava::.jpackage(pkgname, lib.loc=libname)

  path_jar <- system.file("java", package = "rjd3x13")
  if (file.exists(path_jar)) {
    .jaddClassPath(path_jar)
  } else {
    stop("Install rjd3x13 package")
  }
  path_jar <- system.file("java", package = "rjd3tramoseats")
  if (file.exists(path_jar)) {
    .jaddClassPath(path_jar)
  } else {
    stop("Install rjd3x13 package")
  }
  has_java <- rjd3jars::check_java_version(silent = TRUE)
  if (has_java) {
    rjd3jars::reload_dictionaries()
    rjd3jars::reload_tsproviders()
    rjd3jars::reload_safactories()
  }
  # reload extractors
  # rjd3toolkit::reload_dictionaries()
}
