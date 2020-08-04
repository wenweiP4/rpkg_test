
same_level = function() {
  .counter = 41

  get_value = function() {
    .counter <<- .counter + 1
    print("get_value running ....")
    .counter
  }

  delayedAssign('x', get_value())

  expect_equal(x, 42)
  expect_equal(x, 42)
  expect_equal(x, 42)

  # Even re-assign to the same value will invalidate the cached value
  delayedAssign('x', get_value())
  expect_equal(x, 43)
  expect_equal(x, 43)
  expect_equal(x, 43)
}

nested_level = function() {
  .counter = 41

  get_value = function() {
    .counter <<- .counter + 1
    print("get_value running ....")
    .counter
  }

  calling_func = function() {
    # assign x to the calling env
    delayedAssign('x', get_value(), assign.env = parent.frame())
    expect_true(exists('x'))
    expect_equal(x, 42)
    expect_equal(x, 42)
    # x only exists in the calling env, not the current function execution env
    expect_false(exists('x', inherits = F))
  }
  expect_false(exists('x'))
  expect_error({x == 42}, "'x' not found")

  calling_func()

  expect_true(exists('x'))
  expect_equal(x, 42)
  expect_equal(x, 42)
}

simulate_cache = function(){
  cache = myrpkg::make_env(
    get = function(key, force_update = FALSE) {
      v = cached[[key]]
      if(force_update || is.null(v)){
        v = calculate_value(key)
        cached[[key]] = v
      }
      v
    }
    ,
    private = list(
      counter = 0,
      cached = list(),
      calculate_value = function(key) {
        counter = counter + 1
        sprintf("%s%s", toupper(key), counter)
      }
    )
  )

  expect_identical(cache$get('a'), 'A1')
  expect_identical(cache$get('a'), 'A1')
  expect_identical(cache$get('a', force_update = T), 'A2')
  expect_identical(cache$get('a'), 'A2')
  expect_identical(cache$get('a'), 'A2')
}



main = function() {
  same_level()
  nested_level()
  simulate_cache()
}

main()
