
test_that("positive", {
  expect_equal(1+1, 2)
})
test_that("positive", {
  # Ensure test__init.R is called first
  expect_equal(getOption('ans'), 42)
})
