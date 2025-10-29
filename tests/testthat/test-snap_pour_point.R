test_that("snap_pour_point_ returns expected types", {
  testthat::skip_on_cran()
  testthat::skip_if_not(whitebox::check_whitebox_binary(), "whitebox binary not installed")
  dem <- system.file("extdata", "thompsoncreek.tif", package = "SELECTRdata")
  dem <- terra::rast(dem)
  gpkg <- system.file("extdata", "thompsoncreek.gpkg", package = "SELECTRdata")
  pourpoint <- terra::vect(gpkg, layer = "pourpoint", crs = terra::crs(dem))
  D8pointer <- create_d8_pointer(dem)
  D8fa <- create_d8_fa(D8pointer)
  streams_ras <- create_streams(D8fa)
  ## write pourpoints to temp folder
  temp_pour_point_file <- tempfile("snapped", fileext = ".shp")
  x <- snap_pour_point(pour_pts = pourpoint,
                       streams = streams_ras,
                       output = temp_pour_point_file)

  testthat::expect_type(x, "S4")
  testthat::expect_s4_class(x, "SpatVectorProxy")
  testthat::expect_error(snap_pour_point(pour_pts = dem,
                                         streams = streams_ras,
                                         output = temp_pour_point_file))
  testthat::expect_error(snap_pour_point(pour_pts = pourpoint,
                                         streams = pourpoint,
                                         output = temp_pour_point_file))
  testthat::expect_error(snap_pour_point(pour_pts = pourpoint,
                                         streams = streams_ras,
                                         output = temp_pour_point_file,
                                         snap_dist = "invalid"))
  testthat::expect_error(snap_pour_point(pour_pts = pourpoint,
                                         streams = streams_ras,
                                         output = temp_pour_point_file,
                                         type = "invalid"))



  ## cleanup temp files
  unlink(tempdir(), recursive = FALSE)
})
