calc_score <- function(information, default = 5.37) {
  score <- matrix(score_map$score[match(information, score_map$value)], nrow = 3, ncol = 3)
  score[which(is.na(score))] <- default
  score[ , apply(information, 2, function(x) sum(x)/3 == x[1] & sum(is.na(x)) == 0)]  <- 0
  return(score)
}
