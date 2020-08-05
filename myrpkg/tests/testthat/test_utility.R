context("test make_env")

# Create a new env instance

test_that("create a new env without parent reference", {
  e = make_env(public = list(
    a = 3L, b = "BB",
    get_private = function(key) {
      private = parent.env(parent.env(environment()))
      private[[key]]
    },
    get_public = function(key) {
      public = parent.env(environment())
      public[[key]]
    }),
    private = list(pa = "PA", pb = "PB"),
    add_self_ref = F,
    add_private_ref = F
  )

  expect_null(e$.self)
  expect_null(e$.private)

  expect_identical(e$a, 3L)
  expect_identical(e$get_public('a'), 3L)
  expect_identical(e$b, 'BB')
  expect_identical(e$get_public('b'), 'BB')

  # private fields are hidden in the public env
  expect_null(e$pa)
  expect_identical(e$get_private('pa'), 'PA')
  expect_identical(e$get_private('pb'), 'PB')
})

test_that("create a new env without parent reference", {
  e = make_env(public = list(a=3), private = list(b='B'))
  expect_identical(is.null(e[['.self']]), F)
  expect_identical(is.null(e[['.private']]), F)

  e = make_env(public = list(a=3), private = list(b='B'), add_self_ref = F, add_private_ref = F)
  expect_identical(is.null(e[['.self']]), T)
  expect_identical(is.null(e[['.private']]), T)

  e = make_env(public = list(a=3), private = list(b='B'), add_self_ref = T, add_private_ref = F)
  expect_identical(is.null(e[['.self']]), F)
  expect_identical(is.null(e[['.private']]), T)
})

test_that("create a new env, lock_env but not lock_binding", {
  e = make_env(public = list(a=3), private = list(b='B'), lock_env = T, lock_binding = F)
  expect_error({e$extra = "extra"}, "cannot add bindings")
  e$a = 'A'
  expect_identical(e$a, 'A')
})

test_that("create a new locked env, lock_binding but not lock_env", {
  # e = make_env(public = list(a = 3, b = 4), private = list(), lock_env = F, lock_binding = T)
  e = make_env(public = list(a = 3, b = 4), lock_env = F, lock_binding = T)
  e$extra = "extra"
  expect_identical(e$extra, 'extra')
  expect_error({e$a = "A"}, "cannot change value")
})

test_that("create a new locked env, lock_binding and lock_env", {
  e = make_env(public = list(a = 3, b = 4), lock_env = T, lock_binding = T)
  expect_error({e$extra = "extra"}, "cannot add bindings")
  expect_error({e$a = "A"}, "cannot change value")
})


test_that("instances do not share initial fields", {
  counter = 1

  get_env = function() {
    result = new.env(parent =  emptyenv())
    result[['value']] = letters[[counter]]
    counter <<- counter +1
    result
  }


  e1 = make_env(public = list(field = get_env(), get_field_value = function() {field[['value']]} ) )
  e2 = make_env(public = list(field = get_env(), get_field_value = function() {field[['value']]} ) )
  # 'field' are different in e1 and e2
  expect_true(e1$get_field_value() != e2$get_field_value())

  # shared public list
  public = list(
    field = get_env(),
    get_field_value = function() field[['value']],
    change_value = function(v) field[['value']] <- v
    )
  e3 = make_env(public = public)
  e4 = make_env(public = public)

  expect_true(e3$get_field_value() == e4$get_field_value())
  e3$change_value('changed')
  expect_true(e3$get_field_value() == e4$get_field_value())
  expect_true(e4$get_field_value() == 'changed')


})

test_that("variables are first accessed in public space", {
  e = make_env(
    public = list(
      a = 3L, b = 4L,
      call = function(private_func_name) {
        f = .private[[private_func_name]]
        f()
      }
    ),
    private = list(
      a = 10L, b = 20L,
      m = 5L, n = 6L
      , f1 = function() a * b
      , f2 = function() .self$a * .self$b
      , f3 = function() .private$a * b
      , f4 = function() m * n
      , change_values_local = function(){
        a = 2L
      }
      , change_values_self = function(){
        .self$a = 2L
        .private$a = 3L
      }
    )
  )

  expect_identical(e$call('f1'), 12L)
  expect_identical(e$call('f2'), 12L)
  expect_identical(e$call('f3'), 40L)
  expect_identical(e$call('f4'), 30L)
  expect_null(e$m)
  expect_null(e$.self$m)

  e$call('change_values_local')
  expect_identical(e$a, 3L)
  expect_identical(e$.private$a, 10L)

  e$call('change_values_self')
  expect_identical(e$a, 2L)
  expect_identical(e$.private$a, 3L)
})
