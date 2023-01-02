summarise_game <- function(game_table) {
  scores <- sapply(game_table$players, function(x) sum(calc_score(x$cards)))
  winner <- which.min(scores)
  return(list(winner = winner,
              score_winner = scores[winner],
              game_table = game_table))
}
