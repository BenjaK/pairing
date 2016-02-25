context("Cantor pairing functions")


#-------------------------------------------------------------------------------
# Function: cantor_pairing

test_that("Error when non-integer.", {
  expect_error(cantor_pairing(1.5, 1), "x and y must be integers.")
  expect_error(cantor_pairing(1, 1.5), "x and y must be integers.")
  expect_error(cantor_pairing(1.5, 0.4), "x and y must be integers.")
})

test_that("Error when negative.", {
  expect_error(cantor_pairing(-1, 1), "x and y must be non-negative.")
  expect_error(cantor_pairing(1, -1), "x and y must be non-negative.")
  expect_error(cantor_pairing(-1, -1), "x and y must be non-negative.")
})

test_that("Calculates correctly.", {
  expect_equal(cantor_pairing(0, 0), 0)
  expect_equal(cantor_pairing(1, 0), 1)
  expect_equal(cantor_pairing(0, 1), 2)
  expect_equal(cantor_pairing(47, 32), 3192)
})


#-------------------------------------------------------------------------------
# Function: inverse_cantor_pairing

test_that("Error when non-integer.", {
  expect_error(inverse_cantor_pairing(1.5), "z must be an integer.")
})

test_that("Error when negative.", {
  expect_error(inverse_cantor_pairing(-1), "z must be non-negative.")
})

test_that("Calculates correctly.", {
  expect_equal(inverse_cantor_pairing(0), c(0, 0))
  expect_equal(inverse_cantor_pairing(1), c(1, 0))
  expect_equal(inverse_cantor_pairing(2), c(0, 1))
  expect_equal(inverse_cantor_pairing(3192), c(47, 32))
})
