table_play_turn <- function(game_table) {

  if (game_table$turn == 1) {
    game_table$discard <- game_table$deck$value[1]
    game_table$deck <- game_table$deck[2:.N]
  }

  player_turn <- game_table$turn %% length(game_table$players)
  if (player_turn == 0) {
    player_turn <- length(game_table$players)
  }

  player_play_turn(player = game_table$players[[player_turn]], game_table = game_table)

}
