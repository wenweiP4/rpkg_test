#' @export
default_env = new.env()

##' @importFrom magrittr %>%
#NULL

#' @importFrom dplyr filter arrange
NULL

# This function can change the variables inside `default_env`
# also can be called multiple tiems
#' @export
connect = function(host = "localhost", user = "me", token = "my_token") {
  names = c("host", "user", "token")
  sapply(names, function(x) assign(x, get(x), envir = default_env))
  invisible(NULL)
}

#' @export
get_default_env = function() default_env

#' @export
Person = R6::R6Class(
  "Person",
  cloneable = F, portable = F,
  public = list(
    pub_attr = NULL,
    initialize = function(pub_attr = "unset", secrete = "unset"){
       self$pub_attr = pub_attr
       private$secrete = secrete
    },
    pub_show_info = function() {
      sprintf("pub_attr: %s; secrete: %s", self$pub_attr, secrete)
    },
    api_change_secrete = function(value) {
      self$pub_attr = sprintf("%s (from api)", value)
      private$secrete = sprintf("%s (from api)", value)
    }
  )
  ,
  active = list(
    active_field = function(value) {
      if(missing(value)){
        sprintf("active_field: %s", private$secrete)
      } else {
        private$secrete = sprintf("%s (set from outside)", value)
      }
    }
  )
  ,
  private = list(
    secrete = NULL,
    change_secrete = function(value) {
      private$secrete = value
    }
  )
)


#' @export
PersonSingleton = Person$new("Alice", "alice_secrete")

default_env[['bob']] = Person$new("Bob", "bobbby")

#' @export
get_length = function(e = default_env) {
  length(e)
}

#' @export
get_head_of = function(df, n = 5) {
  df %>% head(n)
}

#' @export
call_arrange = function(df, ...) {
  df %>% arrange(...)
}

#' @export
call_filter = function(df, ...) {
  df %>% filter(...)
}


collect_func_args = function(x = 1, y = 2, m, n, ..., z = NULL) {
  m = match.call()
  m
}

collect_func_args2 = function(x = 1, y = 2, m, n, ..., z = NULL) {
  m = match.call(expand.dots = T)
  m
}
