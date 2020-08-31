test_all_regex_functions = function(pattern, x) {
  m_gregexpr = gregexpr(pattern, x) # return a list even if length(x) == 1, also return all sub-string matches
  m_regexpr = regexpr(pattern, x) # return a vector
  m_regexec = regexec(pattern, x) # always return a list, each element is a vector consist of all match substrings

  # browser()
  # regmatches(x[[1]], m_regexec[[1]])

  list(
    regexpr = regmatches(x, m_regexpr),
    grepexpr = regmatches(x, m_gregexpr),
    regexec = regmatches(x, m_regexec)
  ) %>% lapply(deparse)
  # regmatches(x, m_regexpr)
  # regmatches(x, m_regexec) %>% lapply(as.list)

}

# Return all matched substrings in 'x'
# groups in pattern are ignored. The full pattern is looked up
get_sub_recursive = function(pattern, x) {
  x = x[[1]]
  m = gregexpr(pattern, x)
  regmatches(x, m)[[1]]
}

# Return all matched groups explicit defined by parentheses.
# If there are multiple matches, only the first is returned
get_sub_groups = function(pattern, x) {
  x = x[[1]]
  m = regexec(pattern, x)
  regmatches(x, m)[[1]][-1]
}

parse_schema = function(schema_str) {
  parts = get_sub_groups(array_schema_pattern, schema_str)
  arrayName = parts[[1]]
  attrStr = parts[[2]]
  dimStr = parts[[3]]

  attrVec = get_sub_recursive(single_attr_pattern, attrStr)
  dimVec = get_sub_recursive(single_dim_pattern, dimStr)


  list(array_name = arrayName,
    attrs = structure(gsub(single_attr_pattern, "\\2", attrVec), names = gsub(single_attr_pattern, "\\1", attrVec)),
    dims = structure(gsub(single_dim_pattern, "\\3", dimVec), names = gsub(single_dim_pattern, "\\1", dimVec))
    , attrVec = attrVec
    , dimVec = dimVec
  )
}

array_schema_pattern = "^\\s*(\\w+|\\w+\\.\\w+)?\\s*(<[^<>]+>)\\s*(\\[[^]]+\\])?\\s*$"
single_attr_pattern = "(\\w+)\\s*:\\s*([^;,:<>]+)"
single_dim_pattern = "(\\w+)(\\s*\\=\\s*([^];, \t]+))?"

parse_schema(" ab.c_d <ba:int32, bb : string compression 'zlib' , bc:bool not null> [i=0:*:0:*; j=*; k] ")

