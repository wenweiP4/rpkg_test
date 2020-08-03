#' Make an env with function/objs and a hidden parent env.
#'
#' Use the returned env to simulate a singleton with static methods
#' @export
make_env = function(..., private = baseenv(), add_parent_ref = FALSE, lock_env = FALSE, lock_binding = FALSE){
  return_list = list(...)

  parentEnv = if(is.list(private)) {
    list2env(private, parent = baseenv())
  } else if(is.environment(private)) {
    private
  } else {
    stop("ERROR: make_env: param 'private' must be an R env or named list, but got: [%s]", paste(class(private), collapse = ","))
  }
  result = list2env(return_list, parent = parentEnv)

  for (name in ls(result)) {
    if(is.function(result[[name]])){
      environment(result[[name]]) <- result
    }
  }
  result[["self"]] <- result
  if(add_parent_ref){
    result[[".private"]] <- parentEnv
  }
  if(lock_env || lock_binding){
    # we cannot add new variables to a locked env
    # we cannot change variable bindings in a lockBinding env
    # variable bindings can be individually locked, but we treated them consistently
    if(lock_env) {
      lockEnvironment(result, lock_binding)
    } else {
      lockBinding(ls(result), env = result)
    }
  }
  result
}

#' @export
root_utility = make_env(
  greet = function() {
    sprintf("self: %s, dad: %s", name, dad)
  },
  ls = function() {
    # Without the base:: prefix, ls will be first found in the root env,
    # which will cause an error of 'unused arguments'; otherwise infinite recursion
    base::ls(envir = self)
  },
  change_parent = function(value) {
    parent.env(self)$dad = value
  },
  change_self = function(value) {
    self$dad = value
  },
  name = "Alice",
  private = list(
    dad = "Bob"
  )
)
