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
                                    max_depth = NULL) {
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

  ## run wbt
  x <- whitebox::wbt("fill_depressions",
                dem = dem,
                output = output,
                fix_flats = fix_flats,
                flat_increment = flat_increment,
                max_depth = max_depth)

  x <- whitebox::wbt_result(x, i = 1, attribute = "output")

  ## reset whitebox wd
  if(is.null(whitebox_wd)) {
    whitebox::wbt_wd("")
  }

  return(x)

}
