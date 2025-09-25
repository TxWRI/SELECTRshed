#' Extract stream network
#'
#' Creates a raster of likely stream cells from a flow-accumulation raster using
#' the Whitebox Tools algorithm. This is a
#' wrapper for `whitebox::wbt_extract_streams()` that accepts terra SpatRaster
#' objects or a file path to common raster files.
#'
#' @param flow_accumulation SpatRaster object, Input raster D8 flow accumulation file.
#' @param output A character file path specifying where the raster file should be stored. Defaults to a temporary file.
#' @param whitebox_wd valid working directory for whitebox to store temporary rasters. Defaults to NULL which stores in a temporary space that is deleted when the user session is over. You can use this to store rasters before they are processed into terra::rast objects. Otherwise it is suggested to leave this NULL and store the rasters using the output argument.
#' @param threshold Numeric value indicating the threshold in flow accumulation values (number of cells) for channelization.
#' @param zero_background logical, Flag indicating whether a background value of zero should be used.
#'
#' @return A SpatRaster object.
#' @export
#' @examplesIf whitebox::check_whitebox_binary()
#' # example code
#' dem <- system.file("extdata", "thompsoncreek.tif", package = "SELECTR")
#' dem <- terra::rast(dem)
#' D8pointer <- create_D8_pointer(dem)
#' D8fa <- create_D8_fa(D8pointer)
#' create_streams(D8fa)
#'
create_streams <- function(flow_accumulation,
                           output = tempfile(fileext = ".tif"),
                           whitebox_wd = NULL,
                           threshold = 1000,
                           zero_background = FALSE) {
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

  x <- whitebox::wbt("ExtractStreams",
                flow_accum = flow_accumulation,
                output = output,
                threshold = threshold,
                zero_background = zero_background)

  x <- whitebox::wbt_result(x, i = 1, attribute = "output")

  ## reset whitebox wd
  if(is.null(whitebox_wd)) {
    whitebox::wbt_wd("")
  }

  ## return terra rast object
  return(x)
}



#' Extract last coordinate geom from stream vector
#'
#' Internal function to retrieve outlet coordinates for stream network vector.
#' @param stream_vect A SpatVector object of stream network
#' @param output A character file path specifying where the raster file should be stored. Defaults to a temporary file.
#'
#' @return A SpatVector points object for outlet stream points
#' @export
#' @importFrom terra geom vect writeVector vect
#' @keywords internal
extract_stream_outlets <- function(stream_vect,
                                   output = tempfile(fileext = ".shp")) {
  ## TODO: Add a check for output

  ## grab the geoms for each stream line
  stream_geoms <- terra::geom(stream_vect)
  n <- length(unique(stream_geoms[,"geom"]))
  outlet_geom <- data.frame(x = as.numeric(), y = as.numeric())
  ## return a data frame of the last coordinate in each line
  for (i in 1:n) {
    stream_geom <- stream_geoms[stream_geoms[,"geom"] == i, c("x","y")]
    n_coords <- nrow(stream_geom)
    outlet_geom <- rbind(outlet_geom,
                         as.data.frame(as.list(stream_geom[n_coords,])))
  }
  ## data frame to SpatVector
  outlet_geom <- terra::vect(outlet_geom, geom = c("x", "y"))
  ## Write the file and return a terra object with file source
  ## for use in downstream functions with whitebox tools
  terra::writeVector(outlet_geom, filename = output)
  pts <- terra::vect(output)
  return(pts)
}
