calc_score <- function(information, default = 5.37) {
  score <- matrix(score_map$score[match(information, score_map$value)], nrow = 3, ncol = 3)
  score[which(is.na(score))] <- default
  score[ , apply(information, 2, function(x) (x[1] == x[2]) & (x[2] == x[3]) & sum(is.na(x)) == 0)]  <- 0
  return(score)
}
