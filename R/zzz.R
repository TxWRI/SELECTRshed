.onLoad <- function(libname, pkgname) {
  # check_whitebox_binary() is called "loudly" only on package load either:
  #   1. interactively, or
  #   2. environment var R_WHITEBOX_VERBOSE=TRUE or package option whitebox.verbose=TRUE
  whitebox::check_whitebox_binary(silent = !whitebox::wbt_verbose())

}
