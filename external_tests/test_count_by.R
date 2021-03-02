# Test table like function

library(dplyr)

table(CO2 %>% select(Treatment, Plant, Type))

count_by = function(df, count_by_field, ...) {
  values = unique(df %>% select(!!enquo(count_by_field)))
  values %>% print
  df %>% group_by(...) %>% add_count(!!enquo(count_by_field))
}

count_by(CO2, Plant, Type, Treatment)

unloadNamespace("dplyr")
