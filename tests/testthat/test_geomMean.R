
context("Geometric mean")

test_that("a few geometric means", {
  expect_equal(geomMean(c(2, 2, 2)), 2)
  expect_equal(geomMean(c(1, 2, 3, 4, 5, 6)), (1*2*3*4*5*6)^(1/6))
})
