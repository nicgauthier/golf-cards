run_game <- function(game_table) {
  while(!game_table$last_turn) {
    game_table <- table_play_turn(game_table = game_table)
  }

  for (i in 1:(length(game_table$players) - 1)) {
    debugonce(apply_strategy.default_strat)
    game_table <- table_play_turn(game_table = game_table)
  }
  return(game_table)
}
