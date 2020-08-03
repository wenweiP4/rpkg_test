
test_that("substitute: variables are substituted if found in current env (not parent envs)", {
  func <- function(argX) {
    a = 42
    substitute(argX + a + y)
  }

  a = 3L
  b = 4L
  x = a + b
  y = 'y'

  expect_identical(func(x), quote(x + 42 + y))
  expect_identical(func(argX = x), quote(x + 42 + y))
  expect_identical(func(x + a), quote(x + a + 42 + y))
})


test_that("quote vs. substitute", {
  quote_substitute_raw <- function(argX) {
    x = 10

    list(
      argX + x,
      quote(argX + x),
      substitute(argX + x),
      substitute(argX + x, env = parent.frame()) # do it in the calling env
    )
  }

  a = 3
  b = 4
  x = a + b

  result = quote_substitute_raw(x)
  expect_equal(result[[1]], 17)
  expect_identical(result[[2]], quote(argX + x))
  expect_identical(result[[3]], quote(x + 10))
  expect_identical(result[[4]], quote(argX + 7))
})


test_that("substitute: nested", {
  sub_quoted = function(quoted_expr, env = parent.frame()) {
    eval(
      substitute(substitute(z), list(z=quoted_expr)),
      envir = env
    )
  }

  a = 3
  b = 4
  q = quote(a + b + m)
  expect_identical(sub_quoted(q), quote(3 + 4 + m))
})

test_that("substitute function args", {

  func = function(argX){
    a = 3
    b = 4
    m = 5
    eval(substitute(substitute(argX)))
  }

  func_q = function(q){
    a = 3
    b = 4
    m = 5
    unquote = q
    eval(substitute(substitute(unquote)))
  }

  a = x = 10
  b = y = 20
  expect_identical(func(a + b), quote(3 + 4))
  expect_identical(func(a + m), quote(3 + 5))
  expect_identical(func(x + m), quote(x + 5))
  expect_identical(func(x + y), quote(x + y))

  quoted = quote(a + b)
  expect_identical(func(quoted), quote(quoted))

  # Force valuate `quoted`, equivalent to `func(a + b)`
  expect_identical(func(!!quoted), quote(3 + 4))
  # Notice we don't need to force evaluate 'quoted'
  expect_identical(func_q(quoted), quote(3 + 4))
})


# Function args ----

test_that("function args are lazy-evaluated", {


  fa = function(args = ls(pattern = 'var')){
    var_fa_a = 1
    var_fa_b = 2
    args
  }
  var_a = 'var_a'

  # default function args are evaluated in the execution env, ie. function body scope when function is running
  expect_identical(fa(), c('var_fa_a', 'var_fa_b'))

  # Non-default args are evaluated in the calling env (where the function is called)
  # Below line appears the same as the `fa` default args, but has a different scope when evaluated
  expect_identical(fa(args = ls(pattern = 'var')), c('var_a'))
})

test_that("function args are lazy-evaluated in the calling env", {

  fa = function(args = ls(pattern = 'var')){
    var_fa = 1

    fb = function(arg_i = args){
      var_fb = 'var_fb'
      args = 'args'
      arg_i
    }

    list(
      fb(),
      fb(arg_i = args),
      fb(ls(pattern = 'var')),
      fb(arg_i = ls(pattern = 'var'))
    )
  }

  var_ext = 'var_ext'

  expect_identical(fa(), list(
    'args', 'var_fa', 'var_fa', 'var_fa'
  ))
  expect_identical(fa(ls(pattern = 'var')), list(
    'args', 'var_ext', 'var_fa', 'var_fa'
  ))
})
