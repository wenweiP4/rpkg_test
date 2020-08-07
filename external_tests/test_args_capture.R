
assertf = function(cond, error_fmt = "'{.symbol}' is not true", .nframe = 0, .symbol = deparse(substitute(cond))) {
  if(!cond) {
    template = glue("{error_fmt}\n{{trace_call}\n.")
    # cond = deparse(substitute(cond))
    trace_call = deparse(sys.call(-.nframe - 1))
    # stop(glue(template), call. = F)
    warning(glue(template), call. = FALSE)
  }
}

assert_pos_num = function(num, error_fmt = "'{.symbol}' must be > 0, but got: {.value}", .nframe = 0, .symbol = deparse(substitute(num))) {
  .value = num
  assertf(num > 0, error_fmt = glue(error_fmt), .nframe = .nframe + 1, .symbol = .symbol)
}

assert_null = function(value, error_fmt = "'{.symbol}' must be NULL", .nframe = 0, .symbol = deparse(substitute(value))) {
  .value = value
  assertf(is.null(value), error_fmt = glue(error_fmt), .nframe = .nframe + 1, .symbol = .symbol)
}


dummy_api_function = function(a, b, m, n) {
  nested = function() {
    offset = 4
    # without param 'n', assert function shows the immediate calling function
    assert_pos_num(m - offset)
  }

  nested2 = function() {
    offset2 = 5
    # without param 'n', assert function shows the immediate calling function
    assert_pos_num(m - offset2)
  }

  a_num = function(x) x - 10

  assertf(a > 0, glue("param 'a' should be greater than 0, but a = {a}"))
  assert_pos_num(b-3)
  assert_pos_num(a_num(-3))

  # Test how nested functions can report errors
  nested()
  nested2()
}


main = function() {
  .main.name = T
  obj = list(method = dummy_api_function)

  assert_null(obj)
  assert_null(.main.name, "param {.symbol} <> NULL, {.value}")

  cat("================simmulate api function==============\n")
  dummy_api_function(a = -1, b = 2, m = 3)
  cat("================simmulate api class method==============\n")
  obj$method(a = -1, b = 2, m = 3)
}

main()
