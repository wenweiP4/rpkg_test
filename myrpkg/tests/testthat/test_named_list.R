list_to_unique_named_char_vec = function(..., .dot = NULL, .coerce_char = FALSE) {
  aList = if(is.null(.dot)) list(...) else .dot
  if(!.coerce_char){
    if(!all(sapply(aList, function(x) is.null(x) || is.character(x)))) {
      stop("Erorr: not a named list. Every element must be a string (length-1 R character)")
    }
  }
  aListNames = names(aList)
  values = ifelse(sapply(aList, is.null), NA, as.character(aList))
  if(is.null(aListNames)) aListNames = values
  if(any(is.na(aListNames))) stop("NULL element must have a name")

  result = structure(values, names = ifelse(aListNames == "", values, aListNames))
  result
}


test_that("param list to named character vector", {
  expect_equal(
    list_to_unique_named_char_vec(a = "A", b = "B"),
    c(a="A", b="B")
  )
  # Unamed elements assigned a name as its value, e.g. A="A"
  expect_equal(
    list_to_unique_named_char_vec("A", b = "B"),
    c(A="A", b="B")
  )
  # If none of the element has a name, then the whole 'names' is assigned with the values vector
  expect_equal(
    list_to_unique_named_char_vec("A", "B"),
    c(A="A", B="B")
  )

  # elements other than chars can be coerced to chars, if .coerce_char = TRUE
  expect_equal(list_to_unique_named_char_vec(a = 1, .coerce_char = T), c(a="1"))
  # By default, error is thrown if non-string element detected.
  expect_error(list_to_unique_named_char_vec(a = 1), "not a named list")
})
