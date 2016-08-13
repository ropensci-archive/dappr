context("od_list")

test_that("basic usage works", {
  skip_on_cran()

  aa <- od_list("MERRA_MONTHLY")
  bb <- od_list("MERRA_MONTHLY", "MATMNXLND.5.2.0")
  cc <- od_list("MERRA_MONTHLY", "MSTMNXMLD.5.2.0", "2000")

  expect_is(aa, "data.frame")
  expect_is(bb, "data.frame")
  expect_is(cc, "data.frame")
  expect_is(aa$name, "character")

  expect_gt(NROW(bb), NROW(aa))
  expect_gt(NROW(cc), NROW(aa))
})

test_that("od_list fails well with no input", {
  expect_error(od_list(), "Please pass in some values")
})
