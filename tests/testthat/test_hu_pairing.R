context("Hopcroft-Ullman pairing functions")


#-------------------------------------------------------------------------------
# Function: hu_pairing

test_that("Error when non-integer.", {
  expect_error(hu_pairing(1.5, 1), "x and y must be integers.")
  expect_error(hu_pairing(1, 1.5), "x and y must be integers.")
  expect_error(hu_pairing(1.5, 0.4), "x and y must be integers.")
})

test_that("Error when negative.", {
  expect_error(hu_pairing(-1, 1), "x and y must be positive.")
  expect_error(hu_pairing(1, -1), "x and y must be positive.")
  expect_error(hu_pairing(-1, -1), "x and y must be positive.")
})

test_that("Calculates correctly.", {
  expect_equal(hu_pairing(1, 1), 1)
  expect_equal(hu_pairing(2, 1), 2)
  expect_equal(hu_pairing(1, 2), 3)
})


#-------------------------------------------------------------------------------
# Function: inverse_hu_pairing

test_that("Error when non-integer.", {
  expect_error(inverse_hu_pairing(1.5), "z must be an integer.")
})

test_that("Error when negative.", {
  expect_error(inverse_hu_pairing(-1), "z must be positive.")
})

test_that("Calculates correctly.", {
  expect_equal(inverse_hu_pairing(1), c(1, 1))
  expect_equal(inverse_hu_pairing(2), c(2, 1))
  expect_equal(inverse_hu_pairing(3), c(1, 2))
})
