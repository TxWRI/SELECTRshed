#' Jenson snap pour points
#'
#' Moves outlet points used to specify points of interest in a watershedding operation to the nearest stream cell.
#' @param pour_pts Input vector pour points (outlet) file.
#' @param streams Input raster streams file.
#' @param output A character file path specifying where the shapefile file should be stored. Defaults to a temporary file.
#' @param whitebox_wd valid working directory for whitebox to store temporary rasters. Defaults to NULL which stores in a temporary space that is deleted when the user session is over. You can use this to store rasters before they are processed into terra::rast objects. Otherwise it is suggested to leave this NULL and store the rasters using the output argument.
#' @param snap_dist Maximum snap distance in map units.
#' @param type character, one of `"terra"` or `"wbt"`. If `type = "terra"`, returns an object of SpatVector. If `type = "wbt"`, a wbt_result object is returned.
#'
#' @return terra SpatVector object.
#' @export
#'
snap_pour_point <- function(pour_pts,
                            streams,
                            output = tempfile(fileext = ".shp"),
                            whitebox_wd = NULL,
                            snap_dist = 90,
                            type = "terra") {
  ## need to check whitebox tools is installed
  if(!whitebox::check_whitebox_binary()) {
    rlang::abort()
  }
  ## check args
  # dem should be terra obj, does not have to be file backed object since {whitebox} takes care of that when using `wbt()`
  # output should be valid file path
  check_spat_ras(streams)
  ## need to check SpatVect pourpts

  ## check CRS and project the vector if needed.
  if(isFALSE(terra::same.crs(pour_pts, streams))) {
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

  x <- whitebox::wbt("JensonSnapPourPoints",
                     pour_pts = whitebox::wbt_source(pour_pts, force = TRUE),
                     streams = streams,
                     output = output,
                     snap_dist = snap_dist)

  x <- whitebox::wbt_result(x, i = 1, attribute = "output")

  ## reset whitebox wd
  if(is.null(whitebox_wd)) {
    whitebox::wbt_wd("")
  }

  ## return terra vector object
  return(x)

}
