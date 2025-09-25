#' Use the least cost pathway method to breach depressions
#'
#' Creates a DEM with breached depressions using the least cost pathway. This is a
#' wrapper for `whitebox::wbt_breach_depressions_least_cost()` that accepts terra SpatRaster
#' objects or a file path to common raster files.
#'
#' @param dem A character file path to a raster or a SpatRaster object.
#' @param output A character file path specifying where the raster file should be stored. Defaults to a temporary file.
#' @param dist Maximum search distance for breach paths in cells. Defaults to 100.
#' @param whitebox_wd valid working directory for whitebox to store temporary rasters. Defaults to NULL which stores in a temporary space that is deleted when the user session is over. You can use this to store rasters before they are processed into terra::rast objects. Otherwise it is suggested to leave this NULL and store the rasters using the output argument.
#' @param max_cost numeric, Optional maximum breach cost (default is `Inf`).
#' @param min_dist logical, Optional flag indicating whether to minimize breach distances. Default is `FALSE`.
#' @param flat_increment Optional elevation increment applied to flat areas. Default is `NULL`.
#' @param fill logical, Optional flag indicating whether to fill any remaining unbreached depressions. Default is `FALSE`
#'
#'
#' @return A SpatRaster object.
#' @export
#' @examples
#' dem <- system.file("extdata", "thompsoncreek.tif", package = "SELECTR")
#' dem <- terra::rast(dem)
#' create_breach_depression_lc(dem, dist = 10)
#'
create_breach_depression_lc <- function(dem,
                                        output = tempfile(fileext = ".tif"),
                                        dist = 100,
                                        whitebox_wd = NULL,
                                        max_cost = NULL,
                                        min_dist = FALSE,
                                        flat_increment = NULL,
                                        fill = FALSE) {
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

  x <- whitebox::wbt("BreachDepressionsLeastCost",
                     dem = dem,
                     output = output,
                     dist = dist,
                     max_cost = max_cost,
                     min_dist = min_dist,
                     flat_increment = flat_increment)

  x <- whitebox::wbt_result(x, i = 1, attribute = "output")
  ## reset whitebox wd
  if(is.null(whitebox_wd)) {
    whitebox::wbt_wd("")
  }

  ## return terra rast object
  return(x)
}



#' Breach depressions
#'
#' Breaches all of the depressions in a DEM using Lindsay's (2016) algorithm. This should be preferred over depression filling in most cases.
#'
#' @param dem A character file path to a raster or a SpatRaster object.
#' @param output A character file path specifying where the raster file should be stored. Defaults to a temporary file.
#' @param whitebox_wd valid working directory for whitebox to store temporary rasters. Defaults to NULL which stores in a temporary space that is deleted when the user session is over. You can use this to store rasters before they are processed into terra::rast objects. Otherwise it is suggested to leave this NULL and store the rasters using the output argument.
#' @param max_depth numeric, Optional maximum breach depth (default is `Inf`).
#' @param max_length numeric, Optional maximum breach channel length (in grid cells; default is `Inf`).
#' @param flat_increment numeric, Optional elevation increment applied to flat areas. Default is `NULL`.
#' @param fill_pits logical, Optional flag indicating whether to fill single-cell pits. Default is `FALSE`
#'
#'
#' @return A SpatRaster object.
#' @export
#' @examples
#' dem <- system.file("extdata", "thompsoncreek.tif", package = "SELECTR")
#' dem <- terra::rast(dem)
#' create_breach_depression(dem)
#'
create_breach_depression <- function(dem,
                                     output = tempfile(fileext = ".tif"),
                                     whitebox_wd = NULL,
                                     max_depth = NULL,
                                     max_length = NULL,
                                     flat_increment = NULL,
                                     fill_pits = FALSE) {
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

  x <- whitebox::wbt("BreachDepressions",
                     dem = dem,
                     output = output,
                     max_depth = max_depth,
                     max_length = max_length,
                     flat_increment = flat_increment,
                     fill_pits = fill_pits)

  x <- whitebox::wbt_result(x, i = 1, attribute = "output")
  ## reset whitebox wd
  if(is.null(whitebox_wd)) {
    whitebox::wbt_wd("")
  }

  ## return terra rast object
  return(x)

}
