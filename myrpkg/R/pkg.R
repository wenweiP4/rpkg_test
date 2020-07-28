default_env = new.env()

connect = function(host = "localhost", user = "me", token = "my_token") {
  names = c("host", "user", "token")
  sapply(names, function(x) assign(x, get(x), envir = default_env))
  invisible(NULL)
}

get_length = function(e = default_env) {
  length(e)
}
