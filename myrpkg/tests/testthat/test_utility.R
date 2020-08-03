context("test make_env")

assert_all_vars = function(e, var_names, include_hidden = FALSE) {
  nameList = ls(envir = e, all.names = include_hidden)
  expect_identical(length(nameList), length(var_names))
  expect_true(all(var_names %in% nameList))
}

#
# root_utility is a pacakge-level singleton
#
test_that("Simulate a package-level singleton object with an R env", {
  expect_true(is.environment(root_utility))
  assert_all_vars(root_utility, c(
    'greet', 'ls', 'change_parent', 'name', 'self', 'change_self'
  ))
  expect_identical(root_utility$greet(), "self: Alice, dad: Bob")
  assert_all_vars(parent.env(root_utility), 'dad')
})

test_that("Change variable bindings and variable lookup scopes", {
  root_utility$change_self("Tom")
  assert_all_vars(root_utility, c(
    'greet', 'ls', 'change_parent', 'name', 'dad', 'self', 'change_self'
  ))
  expect_identical(root_utility$greet(), "self: Alice, dad: Tom")

  parentEnv = parent.env(root_utility)
  expect_identical(parentEnv$dad, "Bob")

  rm('dad', envir = root_utility)
  expect_identical(root_utility$greet(), "self: Alice, dad: Bob")
})


# Create a new env instance

test_that("create a new env without parent reference", {
  e = make_env(a = 3, b = "BB", private = list(pa = "PA", pb = "PB"))
  expect_identical(e[['self']], e)
  assert_all_vars(e, c('a', 'b', 'self'))

  pe = parent.env(e)
  expect_null(e[['.private']])
  assert_all_vars(pe, c('pa', 'pb'))
})

test_that("create a new env without parent reference", {
  e = make_env(a = 3, b = "BB", private = list(pa = "PA", pb = "PB"), add_parent_ref = T)
  expect_identical(e[['self']], e)
  assert_all_vars(e, c('a', 'b', 'self'))
  assert_all_vars(e, c('a', 'b', 'self', '.private'), include_hidden = T)

  pe = parent.env(e)
  expect_identical(e[['.private']], pe)
  assert_all_vars(pe, c('pa', 'pb'))
})

test_that("create a new env, lock_env but not lock_binding", {
  e = make_env(a = 3, b = 4, lock_env = T, lock_binding = F)
  expect_error({e$extra = "extra"}, "cannot add bindings")
  e$a = 'A'
  expect_identical(e$a, 'A')
})

test_that("create a new locked env, lock_binding but not lock_env", {
  e = make_env(a = 3, b = 4, lock_env = F, lock_binding = T)
  e$extra = "extra"
  expect_identical(e$extra, 'extra')
  expect_error({e$a = "A"}, "cannot change value")
})

test_that("create a new locked env, lock_binding and lock_env", {
  e = make_env(a = 3, b = 4, lock_env = T, lock_binding = T)
  expect_error({e$extra = "extra"}, "cannot add bindings")
  expect_error({e$a = "A"}, "cannot change value")
})

