#' create a table
#'
#' @param players
#' @param deck
#' @param discard
#' @param turn
#' @import data.table
#' @return list
create_game_table <- function(players, deck = create_deck(n = 2), discard = NA, turn = 1) {
  game_table <- list(players = players, deck = deck, discard = discard, turn = turn)

  for (i in seq_along(game_table$players)) {
    game_table$players[[i]]$cards <- matrix(game_table$deck$value[1:9], nrow = 3, ncol = 3)
    game_table$deck <- game_table$deck[10:.N]
  }

  return(game_table)
}
