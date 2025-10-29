#' Use Whitebox Tools fill depression algorithm.
#'
#' Creates a DEM with all of the depressions filled and flat areas removed. This is a
#' wrapper for `whitebox::wbt_fill_depressions()` that accepts terra SpatRaster
#' objects or a file path to common raster files.
#'
#' @param dem A character file path to a raster or a SpatRaster object.
#' @param output A character file path specifying where the raster file should be stored. Defaults to a temporary file.
#' @param whitebox_wd valid working directory for whitebox to store temporary rasters. Defaults to NULL which stores in a temporary space that is deleted when the user session is over. You can use this to store rasters before they are processed into terra::rast objects. Otherwise it is suggested to leave this NULL and store the rasters using the output argument.
#' @param fix_flats logical, indicating whether flat areas should have a small gradient applied. Defaults to `TRUE`.
#' @param flat_increment Optional elevation increment applied to flat areas. Defaults to `NULL`.
#' @param max_depth Optional maximum depression depth to fill. Defaults to `NULL`.
#' @param ... optional arguments passed to `whitebox::wbt()`
#'
#' @return A SpatRaster object.
#' @export
#'
#' @examplesIf whitebox::check_whitebox_binary()
#' # example code
#' dem <- system.file("extdata", "thompsoncreek.tif", package = "SELECTR")
#' dem <- terra::rast(dem)
#' create_fill_depressions(dem)
#'
create_fill_depressions <- function(dem,
                                    output = tempfile(fileext = ".tif"),
                                    whitebox_wd = NULL,
                                    fix_flats = TRUE,
                                    flat_increment = NULL,
                                    max_depth = NULL,
                                    ...) {
  ## need to check whitebox tools is installed
  if(!whitebox::check_whitebox_binary()) {
    rlang::abort()
  }

  check_spat_ras(dem)
  check_logical(fix_flats)
  check_whitebox_wd(whitebox_wd)



  opt_args <- rlang::list2(...)
  if(!is.null(flat_increment)) {
    check_numeric(flat_increment)
    c(opt_args, flat_increment = flat_increment)
  }
  if(!is.null(max_depth)) {
    check_numeric(max_depth)
    c(opt_args, max_depth = max_depth)
  }

  wbt_args <- rlang::list2("FillDepressions",
                           dem = dem,
                           output = output,
                           fix_flats = fix_flats,
                           !!! opt_args)

  x <- rlang::exec(whitebox::wbt, !!!wbt_args)

  x <- whitebox::wbt_result(x, i = 1, attribute = "output")

  ## reset whitebox wd
  reset_whitebox_wd(whitebox_wd)

  return(x)

}
