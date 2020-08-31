list_to_named_char_vec = function(..., .allow_null = TRUE, .allow_duplicate = FALSE, .coerce_char = FALSE, .dot = NULL) {
  aList = if(is.null(.dot)) list(...) else .dot
  if(!.coerce_char){
    if(!all(sapply(aList, function(x) is.null(x) || is.character(x)))) {
      stop("Erorr: not a named list. Every element must be a string (length-1 R character)")
    }
  }
  aListNames = names(aList) # either NULL or a char vector with empty strings
  if(!.allow_null && any(sapply(aList, is.null)))
    stop("NULL value is not allowed")
  values = ifelse(sapply(aList, is.null), NA, as.character(aList))
  if(is.null(aListNames)) aListNames = values
  if(any(is.na(aListNames))) stop("NULL element must have a name")
  if(!.allow_duplicate && anyDuplicated(aListNames))
    stop(sprintf("Duplicated name [%s] is not allowed", aListNames[[anyDuplicated(aListNames)]]))

  result = structure(values, names = ifelse(aListNames == "", values, aListNames))
  result
}


test_that("param list to named character vector", {
  expect_equal(
    list_to_named_char_vec(a = "A", b = "B"),
    c(a="A", b="B")
  )
  # Unamed elements assigned a name as its value, e.g. A="A"
  expect_equal(
    list_to_named_char_vec("A", b = "B"),
    c(A="A", b="B")
  )
  expect_equal(
    list_to_named_char_vec(.dot = list("A", b = "B")),
    c(A="A", b="B")
  )
  # If none of the element has a name, then the whole 'names' is assigned with the values vector
  expect_equal(
    list_to_named_char_vec("A", "B"),
    c(A="A", B="B")
  )

  # elements other than chars can be coerced to chars, if .coerce_char = TRUE
  expect_equal(list_to_named_char_vec(a = 1, .coerce_char = T), c(a="1"))
  # By default, error is thrown if non-string element detected.
  expect_error(list_to_named_char_vec(a = 1), "not a named list")

})

test_that("null values", {
  expect_equal(
    list_to_named_char_vec(a = NULL, b = "B"), c(a = NA, b = "B")
  )
  expect_error(list_to_named_char_vec(a = NULL, b = "B", .allow_null = FALSE), "NULL value is not allowed")
})

test_that("duplicates", {
  # by default, no duplicated names are allowed
  expect_error(list_to_named_char_vec(a = "A", a = "a2"), "Duplicated name")
  # unless specified otherwise
  expect_equal(list_to_named_char_vec(a = "A", a = "a2", .allow_duplicate = T), c(a="A", a="a2"))
})
