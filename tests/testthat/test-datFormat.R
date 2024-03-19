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







