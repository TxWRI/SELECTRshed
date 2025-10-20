#' Fill single cell pits
#'
#' Wrapper for whitebox `FillSingleCellPits`. Raises pit cells to the elevation of their lowest neighbor.
#'
#' @param dem A character file path to a raster or a SpatRaster object.
#' @param output A character file path specifying where the raster file should be stored. Defaults to a temporary file.
#' @param whitebox_wd  A valid working directory for whitebox to store temporary rasters. Defaults to NULL which stores in a temporary space that is deleted when the user session is over. You can use this to store rasters before they are processed into terra::rast objects. Otherwise it is suggested to leave this NULL and store the rasters using the output argument.
#' @param ... optional arguments passed to `whitebox::wbt()`
#'
#' @return A SpatRaster object
#' @export
#'
create_single_pit_fill <- function(dem,
                                   output = tempfile(fileext = ".tif"),
                                   whitebox_wd = NULL,
                                   ...) {
  ## need to check whitebox tools is installed
  if(!whitebox::check_whitebox_binary()) {
    rlang::abort()
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

  wbt_args <- rlang::list2("fill_single_cell_pits",
                           dem = dem,
                           output = output,
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
