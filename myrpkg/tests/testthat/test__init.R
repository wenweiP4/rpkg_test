context("Test initialization")

# # Only set the option with the 'name' if not set
try_set_option = function(name, func, overwrite = FALSE) {
  if(is.null(options(name)) || overwrite){
    cat(glue::glue("\nSet the option '{name}'\n"))
    args = as.list(setNames(func(), name))
    do.call(options, args)
  }
}

try_set_option('ans', function() 42)

