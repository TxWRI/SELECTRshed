#' Subbasins
#'
#' Identifies the catchments, or sub-basin, draining to each link in a stream network.
#'
#' @param d8_pntr Input D8 pointer raster file.
#' @param streams Input raster streams file.
#' @param output A character file path specifying where the shapefile file should be stored. Defaults to a temporary file.
#' @param esri_pntr D8 pointer uses the ESRI style scheme.
#' @param whitebox_wd valid working directory for whitebox to store temporary rasters. Defaults to NULL which stores in a temporary space that is deleted when the user session is over. You can use this to store rasters before they are processed into terra::rast objects. Otherwise it is suggested to leave this NULL and store the rasters using the output argument.
#' @param type character, one of `"terra"` or `"wbt"`. If `type = "terra"`, returns an object of SpatVector. If `type = "wbt"`, a wbt_result object is returned.
#' @param ... optional arguments passed to `whitebox::wbt()`.
#' @return SpatRaster
#' @export
#' @examplesIf whitebox::check_whitebox_binary()
#' dem <- system.file("extdata", "thompsoncreek.tif", package = "SELECTR")
#' dem <- terra::rast(dem)
#' D8pointer <- create_d8_pointer(dem)
#' D8fa <- create_d8_fa(D8pointer)
#' streams <- create_streams(D8fa)
#' create_subbasins(D8pointer, streams)

create_subbasins <- function(d8_pntr,
                             streams,
                             output = tempfile(fileext = ".tif"),
                             esri_pntr = FALSE,
                             whitebox_wd = NULL,
                             type = "terra",
                             ...) {
  ## need to check whitebox tools is installed
  if(!whitebox::check_whitebox_binary()) {
    rlang::abort()
  }
  ## check args
  # dem should be terra obj, does not have to be file backed object since {whitebox} takes care of that when using `wbt()`
  # output should be valid file path
  check_spat_ras(d8_pntr)
  check_spat_ras(streams)
  check_logical(esri_pntr)

  ## check CRS
  if(isFALSE(terra::same.crs(d8_pntr, streams))) {
    cli::cli_abort("coordinate reference systems don't match!")
  }

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

  wbt_args <- rlang::list2("Subbasins",
                           d8_pntr = d8_pntr,
                           streams = streams,
                           output = output,
                           esri_pntr = esri_pntr,
                           !!! opt_args)

  x <- rlang::exec(whitebox::wbt, !!!wbt_args)

  x <- whitebox::wbt_result(x, i = 1, attribute = "output")

  ## reset whitebox wd
  if(is.null(whitebox_wd)) {
    whitebox::wbt_wd("")
  }

  ## return terra rast object
  return(x)
}
