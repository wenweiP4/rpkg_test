
assert_default_length = function() {
  expect_identical(myrpkg::get_length(), 3L)
}

test_that("default_env", {
  myrpkg::connect(user = "alice")
  e = myrpkg::default_env
  expect_identical(get('host', e), 'localhost')
  expect_identical(get('user', e), 'alice')
})

test_that("call pkg function", {
  myrpkg::connect()
  e = myrpkg::default_env
  assert_default_length()
  expect_identical(get_length(list(a=3, b=42)), 2L)
})

test_that("set global side effect", {
  myrpkg::connect()
  e = myrpkg::default_env
  e$extra = 'extra'
  expect_identical(get_length(), 4L)
})

test_that("global side effect result", {
  expect_identical(get_length(), 4L)
})

test_that("cancel global side effect result", {
  e = myrpkg::default_env
  rm(list = ls(e), envir = e)
  expect_identical(get_length(), 0L)

  myrpkg::connect()
  assert_default_length()
})
