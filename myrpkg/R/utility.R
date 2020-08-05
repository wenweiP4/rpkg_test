#' Make an env with function/objs and a hidden parent env.
#'
#' Use the returned env to simulate a singleton with static methods
#' @export
make_env = function(
  public = list(),
  private = list(),
  root_env = parent.frame(),
  add_self_ref = TRUE,
  add_private_ref = TRUE,
  lock_env = FALSE,
  lock_binding = FALSE
){
  return_list = public

  move_funcs_to_env = function(e, target_env = e) {
    is_name_func = function(name) is.function(e[[name]])
    funcNames = Filter(is_name_func, ls(e, all.names = TRUE))
    sapply(funcNames, function(x) {
      environment(e[[x]]) <- target_env
    })
  }

  parentEnv = if(is.list(private)) {
    list2env(private, parent = baseenv())
  } else {
    stop("ERROR: make_env: param 'private' must be a named list, but got: [%s]", paste(class(private), collapse = ","))
  }
  result = if(is.list(public))  {
    list2env(public, parent = parentEnv)
  } else if(is.environment(public)) {
    parent.env(public) <- parentEnv
    public
  } else { stop("ERROR: make_env: param 'public' must be a named list or R env") }

  move_funcs_to_env(result)
  move_funcs_to_env(parentEnv, result)

  if(add_self_ref){
    result[[".self"]] <- result
    parentEnv[['.self']] <- result
  }
  if(add_private_ref){
    result[[".private"]] <- parentEnv
    parentEnv[[".private"]] <- parentEnv
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
  public = list(
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
    name = "Alice"
  ),
  private = list(
    dad = "Bob"
  )
)
