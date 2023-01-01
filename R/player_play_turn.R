player_play_turn <- function(player, game_table) {
  apply_strategy(player = player, game_table = game_table)
}


apply_strategy <- function(player, game_table) {
  UseMethod("apply_strategy")
}

apply_strategy.default_strat <- function(player, game_table) {

  if (game_table$turn == 1) {
    game_table$players[[player$id]]$information[1,1] <- game_table$players[[player$id]]$cards[1,1]
    if (game_table$players[[player$id]]$information[1,1] >= 13) {
      game_table$players[[player$id]]$information[1,2] <- game_table$players[[player$id]]$cards[1,2]
    } else {
      game_table$players[[player$id]]$information[2,1] <- game_table$players[[player$id]]$cards[2,1]
    }
  }

  # check for completion of column
  own_completion_ind <- check_own_col_completion(information = game_table$players[[player$id]]$information, discard = game_table$discard)

  # if completion possible
  if (any(own_completion_ind)) {
    card_to_switch <- which(is.na(game_table$players[[player$id]]$information[, which(own_completion_ind)[1]]))
    game_table$players[[player$id]]$information[card_to_switch, which(own_completion_ind)[1]] <- game_table$discard
    game_table$discard <- game_table$players[[player$id]]$cards[card_to_switch, which(own_completion_ind)[1]]
    game_table$players[[player$id]]$cards[card_to_switch, which(own_completion_ind)[1]] <- game_table$players[[player$id]]$information[card_to_switch, which(own_completion_ind)[1]]
    game_table$players[[player$id]]$completion[which(own_completion_ind)[1]] <- TRUE
    game_table$players[[player$id]]$score[, which(own_completion_ind)[1]] <- 0
  } else {
    if (game_table$discard > 5) {
      # calc switch payoff
      # check next player completion
      # choose max payoff that don't give completion to other player
    }
  }

  #TODO if no completion if card higher than 5 pick card from deck
  #TODO switch for max payoff without giving completion to next player
}

check_own_col_completion <- function(information, discard) {
  apply(information, 2, function(x) {
    if (sum(is.na(x)) == 1) {
      return((sum(x, discard, na.rm = T)/3) == discard)
    } else {
      return(FALSE)
    }
  })
}

calc_payoff <- function(player, game_table) {
  UseMethod("calc_payoff")
}

calc_payoff.default_strat <- function(player, game_table) {
  # calc switch payoff for completion
  for (completion in player$completion) {
    if (completion) {

    }
  }
  # assign default value to unknown cards
}
