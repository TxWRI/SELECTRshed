#' Convert stream raster to vector
#'
#' Creates a stream vector (terra SpatVector object) from a D8 flow direction
#' raster and stream network raster.
#' This is a wrapper for `whitebox::wbt_raster_streams_to_vector()` that accepts
#' terra SpatRaster objects or a file path to common raster files.
#'
#' @param streams SpatRaster object, input raster streams.
#' @param d8_pointer SpatRaster object, input raster D8 pointer.
#' @param output A character file path specifying where the raster file should be stored. Defaults to a temporary file.
#' @param whitebox_wd valid working directory for whitebox to store temporary rasters. Defaults to NULL which stores in a temporary space that is deleted when the user session is over. You can use this to store rasters before they are processed into terra::rast objects. Otherwise it is suggested to leave this NULL and store the rasters using the output argument.
#' @param esri_pntr logical, D8 pointer uses the ESRI style scheme.
#' @param all_vertices logical, Do you want to preserve all vertices in output (i.e. no straight-line generalization).
#' @param ... optional arguments passed to `whitebox::wbt()`
#'
#' @return A terra SpatVector object
#' @export
#' @examplesIf whitebox::check_whitebox_binary()
#' # example code
#' dem <- system.file("extdata", "thompsoncreek.tif", package = "SELECTR")
#' dem <- terra::rast(dem)
#' D8pointer <- create_d8_pointer(dem)
#' D8fa <- create_d8_fa(D8pointer)
#' streams <- create_streams(D8fa)
#' create_streams_vector(streams, D8pointer)
#'
create_streams_vector <- function(streams,
                                  d8_pointer,
                                  output = tempfile(fileext = ".shp"),
                                  whitebox_wd = NULL,
                                  esri_pntr = FALSE,
                                  all_vertices = FALSE,
                                  ...) {

  ## need to check whitebox tools is installed
  if(!whitebox::check_whitebox_binary()) {
    rlang::abort()
  }

  ## can write a function for this check since we use it everytime....
  ## set whitebox working directory
  if(is.null(whitebox_wd)) {
    ## this prevents temp {whitebox} files being written to the project directory by default
    whitebox::wbt_wd(wd = tempdir())
  }
  if(!is.null(whitebox_wd)) {
    ## else we can point to whatever directory the user wants {whitebox} generated files to be written
    whitebox::wbt_wd(wd = whitebox_wd)
  }

  opt_args <- rlang::list2(...)

  wbt_args <- rlang::list2("RasterStreamsToVector",
                           streams = streams,
                           d8_pntr = d8_pointer,
                           output = output,
                           esri_pntr = esri_pntr,
                           all_vertices = all_vertices,
                           !!! opt_args)

  x <- rlang::exec(whitebox::wbt, !!!wbt_args)

  x <- whitebox::wbt_result(x, i = 1, attribute = "output")

  ## reset whitebox wd
  if(is.null(whitebox_wd)) {
    whitebox::wbt_wd("")
  }

  ## return terra object
  return(x)

}
