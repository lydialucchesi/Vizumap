library(Vizumap)
context("data formatting prior to visualisation construction")

test_that("both estimates and errors are formatted properly as numerical columns",
          {
            expect_true(is.numeric(read.uv(
              us_data, estimate = "pov_rate", error = "pov_moe"
            )[, 1]))
            expect_true(is.numeric(read.uv(
              us_data, estimate = "pov_rate", error = "pov_moe"
            )[, 2]))

          })

test_that("pixelate works with projection different than that of the maps included in package",
          {
            pol <- "POLYGON((0 0, 0 .01, .01 .01, .01 0, 0 0))"
            testpol <- readWKT(pol)
            proj4string(testpol) <-
              "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"

            expect_true(nrow(Vizumap::pixelate(testpol, pixelSize = 3)) > 0)
          })

test_that("pixelate check works for input type", {
  expect_error(pixelate(geoData = data.frame(
    long = c(-100), lat = c(45)
  )))

})





