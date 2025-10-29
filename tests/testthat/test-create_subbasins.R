test_that("create_subbasins_ returns expected types", {
  testthat::skip_on_cran()
  dem <- system.file("extdata", "thompsoncreek.tif", package = "SELECTRdata")
  dem <- terra::rast(dem)
  D8pointer <- create_d8_pointer(dem)
  D8fa <- create_d8_fa(D8pointer)
  streams <- create_streams(D8fa)
  x <- create_subbasins(D8pointer, streams)

  testthat::expect_type(x, "S4")
  testthat::expect_s4_class(x, "SpatRaster")
  testthat::expect_error(create_subbasins(terra::vect(system.file("ex/lux.shp", package="terra")),
                                          streams))
  testthat::expect_error(create_subbasins(D8pointer,
                                          terra::vect(system.file("ex/lux.shp", package="terra"))))
  testthat::expect_error(create_streams(D8fa, threshold = TRUE))
  testthat::expect_error(create_streams(D8fa, zero_background = 0))

  y <- create_streams_vector(x, D8pointer)
  testthat::expect_type(y, "S4")
  testthat::expect_s4_class(y, "SpatVectorProxy")
  testthat::expect_error(create_streams_vector(terra::vect(system.file("ex/lux.shp", package="terra")),
                                               d8_pointer = D8pointer))
  testthat::expect_error(create_streams_vector(x,
                                               d8_pointer = terra::vect(system.file("ex/lux.shp", package="terra"))))
  testthat::expect_error(create_subbasins(D8pointer, streams,
                                          esri_pntr = "yes"))
  testthat::expect_error(create_subbasins(D8pointer, streams,
                                          type = "invalid"))
})
