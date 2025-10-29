test_that("create_single_pit_fill returns expected types", {
  testthat::skip_on_cran()
  dem <- system.file("extdata", "thompsoncreek.tif", package = "SELECTRdata")
  dem <- terra::rast(dem)
  x <- create_single_pit_fill(dem)
  testthat::expect_type(x, "S4")
  testthat::expect_s4_class(x, "SpatRaster")
  testthat::expect_error(create_single_pit_fill(terra::vect(system.file("ex/lux.shp", package="terra"))))
})
