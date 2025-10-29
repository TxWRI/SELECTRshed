test_that("create_watershed_ returns expected types", {
  testthat::skip_on_cran()
  dem <- system.file("extdata", "thompsoncreek.tif", package = "SELECTRdata")
  dem <- terra::rast(dem)
  gpkg <- system.file("extdata", "thompsoncreek.gpkg", package = "SELECTRdata")
  pourpoint <- terra::vect(gpkg, layer = "pourpoint", crs = terra::crs(dem))
  D8pointer <- create_d8_pointer(dem)
  D8fa <- create_d8_fa(D8pointer)
  streams_ras <- create_streams(D8fa)
  ## write pourpoints to temp folder
  temp_pour_point_file <- tempfile("snapped", fileext = ".shp")
  snapped_pour_point <- snap_pour_point(pour_pts = pourpoint,
                                        streams = streams_ras,
                                        output = temp_pour_point_file)
  snapped_pour_point <- terra::vect(temp_pour_point_file)
  pour_point_rast <-  terra::rasterize(snapped_pour_point, streams_ras)
  x <- create_watershed(d8_pntr = D8pointer, pour_pts = pour_point_rast)

  testthat::expect_type(x, "S4")
  testthat::expect_s4_class(x, "SpatRaster")
  testthat::expect_error(create_watershed(d8_pntr = terra::vect(system.file("ex/lux.shp", package="terra")),
                                          pour_pts = pour_point_rast))
  testthat::expect_error(create_watershed(d8_pntr = D8pointer,
                                          pour_pts = snapped_pour_point))
  testthat::expect_error(create_watershed(d8_pntr = D8pointer,
                                          pour_pts = pour_point_rast,
                                          esri_pntr = "invalid"))
  testthat::expect_error(create_watershed(d8_pntr = D8pointer,
                                          pour_pts = pour_point_rast,
                                          type = "invalid"))



  ## cleanup temp files
  unlink(tempdir(), recursive = FALSE)
})
