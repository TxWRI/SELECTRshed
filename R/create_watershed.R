#' Watershed
#'
#' Identifies the watershed, or drainage basin, draining to a set of target cells.
#'
#' @param d8_pntr Input D8 pointer.
#' @param pour_pts Input pour points (outlet).
#' @param output A character file path specifying where the shapefile file should be stored. Defaults to a temporary file.
#' @param esri_pntr D8 pointer uses the ESRI style scheme.
#' @param whitebox_wd valid working directory for whitebox to store temporary rasters. Defaults to NULL which stores in a temporary space that is deleted when the user session is over. You can use this to store rasters before they are processed into terra::rast objects. Otherwise it is suggested to leave this NULL and store the rasters using the output argument.
#' @param type character, one of `"terra"` or `"wbt"`. If `type = "terra"`, returns an object of SpatVector. If `type = "wbt"`, a wbt_result object is returned.
#' @param ... optional arguments passed to `whitebox::wbt()`.
#'
#' @return A SpatRaster or wbt_result object.
#' @export
#' @examplesIf whitebox::check_whitebox_binary()
#' dem <- system.file("extdata", "thompsoncreek.tif", package = "SELECTR")
#' dem <- terra::rast(dem)
#' gpkg <- system.file("extdata", "thompsoncreek.gpkg", package = "SELECTRdata")
#' pourpoint <- terra::vect(gpkg, layer = "pourpoint", crs = terra::crs(dem))
#' D8pointer <- create_d8_pointer(dem)
#' D8fa <- create_d8_fa(D8pointer)
#' streams_ras <- create_streams(D8fa)
#' ## write pourpoints to temp folder
#' temp_pour_point_file <- tempfile("snapped", fileext = ".shp")
#' snapped_pour_point <- snap_pour_point(pour_pts = pourpoint,
#'                                       streams = streams_ras,
#'                                       output = temp_pour_point_file)
#' snapped_pour_point <- terra::vect(temp_pour_point_file)
#' pour_point_rast <-  terra::rasterize(snapped_pour_point, streams_ras)
#' create_watershed(d8_pntr = D8pointer, pour_pts = pour_point_rast)
#' ## cleanup temp files
#' unlink(tempdir(), recursive = FALSE)

create_watershed <- function(d8_pntr,
                             pour_pts,
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
  check_spat_ras(pour_pts)
  check_logical(esri_pntr)
  check_whitebox_wd(whitebox_wd)

  ## check CRS
  if(isFALSE(terra::same.crs(d8_pntr, pour_pts))) {
    cli::cli_abort("coordinate reference systems don't match!")
  }

  opt_args <- rlang::list2(...)

  wbt_args <- rlang::list2("Watershed",
                           d8_pntr = d8_pntr,
                           pour_pts = pour_pts,
                           output = output,
                           esri_pntr = esri_pntr,
                           !!! opt_args)

  x <- rlang::exec(whitebox::wbt, !!!wbt_args)

  x <- whitebox::wbt_result(x, i = 1, attribute = "output")

  ## reset whitebox wd
  reset_whitebox_wd(whitebox_wd)

  ## return terra vector object
  return(x)

}
