library(data.table)
score_map <- data.table(value = 1:14, score = 1:14)
score_map[value %in% c(10,11,12), score := 10]
score_map[value == 13, score := 0]
score_map[value == 14, score := -5]

usethis::use_data(score_map, overwrite = T)
