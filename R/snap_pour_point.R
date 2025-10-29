#' Jenson snap pour points
#'
#' Moves outlet points used to specify points of interest in a watershedding operation to the nearest stream cell.
#' @param pour_pts Input vector pour points (outlet) file.
#' @param streams Input raster streams file.
#' @param output A character file path specifying where the shapefile file should be stored. Defaults to a temporary file.
#' @param whitebox_wd valid working directory for whitebox to store temporary rasters. Defaults to NULL which stores in a temporary space that is deleted when the user session is over. You can use this to store rasters before they are processed into terra::rast objects. Otherwise it is suggested to leave this NULL and store the rasters using the output argument.
#' @param snap_dist Maximum snap distance in map units.
#' @param type character, one of `"terra"` or `"wbt"`. If `type = "terra"`, returns an object of SpatVector. If `type = "wbt"`, a wbt_result object is returned.
#' @param ... optional arguments passed to `whitebox::wbt()`.
#'
#' @return terra SpatVector object.
#' @export
#' @examplesIf whitebox::check_whitebox_binary()
#' ## example data
#' dem <- system.file("extdata", "thompsoncreek.tif", package = "SELECTRdata")
#' dem <- terra::rast(dem)
#' gpkg <- system.file("extdata", "thompsoncreek.gpkg", package = "SELECTRdata")
#' pourpoint <- terra::vect(gpkg, layer = "pourpoint", crs = terra::crs(dem))
#'
#' ## create flow direction and flow accumulation rasters
#' D8pointer <- create_d8_pointer(dem)
#' D8fa <- create_d8_fa(D8pointer)
#'
#' ## create streams network raster
#' streams_ras <- create_streams(D8fa)
#'
#' ## write pourpoints to temp folder
#' temp_pour_point_file <- tempfile("snapped", fileext = ".shp")
#'
#' ## snap pour points to stream raster network
#' snapped_pour_point <- snap_pour_point(pour_pts = pourpoint,
#'                                       streams = streams_ras,
#'                                       output = temp_pour_point_file)
#'
#' ## you probably need a raster of the pour point, you need to call
#' ## terra::vect since it is a SpatVectorProxy
#' pour_point_rast <-  terra::rasterize(terra::vect(temp_pour_point_file), streams_ras)
#'
#' ## cleanup temp files
#' unlink(tempdir(), recursive = FALSE)
snap_pour_point <- function(pour_pts,
                            streams,
                            output = tempfile(fileext = ".shp"),
                            whitebox_wd = NULL,
                            snap_dist = 90,
                            type = "terra",
                            ...) {
  ## need to check whitebox tools is installed
  if(!whitebox::check_whitebox_binary()) {
    rlang::abort()
  }
  ## check args
  # dem should be terra obj, does not have to be file backed object since {whitebox} takes care of that when using `wbt()`
  # output should be valid file path
  check_spat_vect(pour_pts)
  check_spat_ras(streams)
  check_numeric(snap_dist)
  check_whitebox_wd(whitebox_wd)
  check_wbt_type(type)

    ## check CRS and project the vector if needed.
  if(isFALSE(terra::same.crs(pour_pts, streams))) {
    cli::cli_abort("coordinate reference systems don't match!")
  }

  opt_args <- rlang::list2(...)

  wbt_args <- rlang::list2("JensonSnapPourPoints",
                           pour_pts = whitebox::wbt_source(pour_pts, force = TRUE),
                           streams = streams,
                           output = output,
                           snap_dist = snap_dist,
                           !!! opt_args)

  x <- rlang::exec(whitebox::wbt, !!!wbt_args)

  x <- whitebox::wbt_result(x, i = 1, attribute = "output")

  ## reset whitebox wd
  reset_whitebox_wd(whitebox_wd)

  ## return terra vector object
  return(x)

}
