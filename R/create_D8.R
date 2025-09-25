#' Use D8 algorithm to create flow direction grid.
#'
#' Creates a flow direction or pointer grid using the d8 algorithm. This is a
#' wrapper for the whitebox tools `d8_pointer` that accepts terra SpatRaster
#' objects or a file path to common raster files.
#'
#' @param dem A character file path to a raster or a SpatRaster object.
#' @param output  A character file path specifying where the raster file should be stored. Defaults to a temporary file.
#' @param whitebox_wd valid working directory for whitebox to store temporary rasters. Defaults to NULL which stores in a temporary space that is deleted when the user session is over. You can use this to store rasters before they are processed into terra::rast objects. Otherwise it is suggested to leave this NULL and store the rasters using the output argument.
#' @param esri_pntr logical, D8 pointer uses the ESRI style scheme.
#'
#' @return a SpatRaster object.
#' @export
#' @examplesIf whitebox::check_whitebox_binary()
#' # example code
#' dem <- system.file("extdata", "thompsoncreek.tif", package = "SELECTR")
#' dem <- terra::rast(dem)
#' create_D8_pointer(dem)
#'

create_d8_pointer <- function(dem,
                              output = tempfile(fileext = ".tif"),
                              whitebox_wd = NULL,
                              esri_pntr = FALSE) {

  ## need to check whitebox tools is installed
  if(!whitebox::check_whitebox_binary()) {
    rlang::abort()
  }

  ## check args
  # dem should be terra obj, does not have to be file backed object since {whitebox} takes care of that when using `wbt()`
  # output should be valid file path
  # whitebox_wd should be NULL or directory
  check_spat_ras(dem)


  ## set whitebox working directory
  if(is.null(whitebox_wd)) {
    ## this prevents temp {whitebox} files being written to the project directory by default
    whitebox::wbt_wd(wd = tempdir())
  }
  if(!is.null(whitebox_wd)) {
    ## else we can point to whatever directory the user wants {whitebox} generated files to be written
    whitebox::wbt_wd(wd = whitebox_wd)
  }

  ## run d8 pointer
  fa <- whitebox::wbt("d8_pointer",
                      dem = dem,
                      output = output,
                      esri_pntr = esri_pntr)

  fa <- whitebox::wbt_result(fa, i = 1, attribute = "output")

  ## reset whitebox wd
  if(is.null(whitebox_wd)) {
    whitebox::wbt_wd("")
  }

  ## return terra rast object
  return(fa)

}


#' Create a D8 flow accumulation raster
#'
#' Creates a flow accumulation grid from a D8 flow direction raster or imput DEM.
#' This is a wrapper for `whitebox::wbt_d8_flow_accumulation()` that accepts
#' terra SpatRaster objects or a file path to common raster files.
#'
#' @param D8pointer A character file path to a raster or a SpatRaster object.
#' @param output A character file path specifying where the raster file should be stored. Defaults to a temporary file.
#' @param whitebox_wd valid working directory for whitebox to store temporary rasters. Defaults to NULL which stores in a temporary space that is deleted when the user session is over. You can use this to store rasters before they are processed into terra::rast objects. Otherwise it is suggested to leave this NULL and store the rasters using the output argument.
#' @param out_type A character value indicating the values calculated for each cell. One of `cells` (default), `catchment area`, or `specific contributing area`.
#' @param log Optional flag to request the output be log-transformed.
#' @param clip Optional flag to request clipping the display max by 1%.
#' @param pntr  Is the input raster a D8 flow pointer rather than a DEM?
#' @param esri_pntr Input D8 pointer uses the ESRI style scheme.
#' @param ... additional arguments used by `whitebox::wbt()`
#'
#' @return A SpatRaster object.
#' @export
#' @examplesIf whitebox::check_whitebox_binary()
#' # example code
#' dem <- system.file("extdata", "thompsoncreek.tif", package = "SELECTR")
#' dem <- terra::rast(dem)
#' D8pointer <- create_D8_pointer(dem)
#' create_D8_fa(D8pointer)
#'

create_D8_fa <- function(D8pointer,
                         output = tempfile(fileext = ".tif"),
                         whitebox_wd = NULL,
                         out_type = "cells",
                         log = FALSE,
                         clip = FALSE,
                         pntr = TRUE,
                         esri_pntr = FALSE,
                         ...) {

  ## need to check whitebox tools is installed
  if(!whitebox::check_whitebox_binary()) {
    rlang::abort()
  }

  ## check the output type
  check_out_type(out_type, arg = "out_type")

  ## set whitebox working directory
  if(is.null(whitebox_wd)) {
    ## this prevents temp {whitebox} files being written to the project directory by default
    whitebox::wbt_wd(wd = tempdir())
  }
  if(!is.null(whitebox_wd)) {
    ## else we can point to whatever directory the user wants {whitebox} generated files to be written
    whitebox::wbt_wd(wd = whitebox_wd)
  }

  fa <- whitebox::wbt("D8FlowAccumulation",
                      input = D8pointer,
                      output = output,
                      out_type = out_type,
                      log = log,
                      clip = clip,
                      pntr = pntr)

  fa <- whitebox::wbt_result(fa, i = 1, attribute = "output")

  ## reset whitebox wd
  if(is.null(whitebox_wd)) {
    whitebox::wbt_wd("")
  }

  ## return terra rast object
  return(fa)

}




check_out_type <- function(x,
                           arg = rlang::caller_arg(x),
                           call = rlang::caller_env()) {
  if (!x %in% c("cells", "catchment area", "specific contributing area")) {
    cli::cli_abort("{.arg {arg}} must be one of `c('cells', 'catchment area', 'specific contributing area')`.",
                   call = call)
  }
}
