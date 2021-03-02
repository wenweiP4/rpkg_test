# Try to get unique records according to a timestamp field

df = data.table::fread(
"timestamp,name,path
2020-01-01 01:02:03,abc.tsv,path1/abc.tsv
2020-02-03 01:02:03,abc.tsv,path2/abc.tsv
2020-01-01 01:02:03,def.tsv,path1/def.tsv
2020-01-01 01:02:03,ghi.tsv,path1/ghi.tsv
2020-03-01 01:02:03,def.tsv,path2/def.tsv
"
)

find_most_recent = function(dataFrame, unique_field, timestamp_field) {
  semiJoinFields = c(as.character(unique_field), maxts = as.character(timestamp_field))
  dataFrame %>% group_by(!!unique_field) %>% summarize(maxts = max(!!timestamp_field)) %>%
    semi_join(dataFrame, ., by = semiJoinFields)
}

find_most_recent(df, as.name("name"), as.name("timestamp"))


df %>% group_by(name) %>% summarize(cnt = n(), timestamp = max(timestamp)) %>%
  semi_join(df, ., by = c(name, timestamp))

