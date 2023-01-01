player_play_turn <- function(player, game_table) {
  return(apply_strategy(player = player, game_table = game_table))
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

  payoff <- calc_payoff(player = game_table$players[[player$id]], game_table = game_table)

  if (payoff$action == "discard") {
    temp_card <- game_table$players[[player$id]]$cards[payoff$switch]
    game_table$players[[player$id]]$cards[payoff$switch] <- game_table$discard
    game_table$players[[player$id]]$information[payoff$switch] <- game_table$players[[player$id]]$cards[payoff$switch]
    game_table$discard <- temp_card
  } else if (payoff$action == "draw") {
    game_table$discard <-  game_table$players[[player$id]]$cards[payoff$switch]
    game_table$players[[player$id]]$cards[payoff$switch] <- game_table$deck[1, value]
    game_table$players[[player$id]]$information[payoff$switch] <- game_table$players[[player$id]]$cards[payoff$switch]
    game_table$deck <- game_table$deck[2:.N]
  } else {
    game_table$discard <- game_table$deck[1, value]
    game_table$deck <- game_table$deck[2:.N]
    card_to_flip <- which(is.na(game_table$players[[player$id]]$information))[1]
    game_table$players[[player$id]]$information[card_to_flip] <- game_table$players[[player$id]]$cards[card_to_flip]
  }

  game_table$players[[player$id]]$score <- calc_score(information = game_table$players[[player$id]]$information, default = 5.37)

  if (length(which(is.na(game_table$players[[player$id]]$information))) == 0) {
    game_table$last_turn <- TRUE
  }
  return(game_table)
}


calc_payoff <- function(player, game_table) {
  UseMethod("calc_payoff")
}

calc_payoff.default_strat <- function(player, game_table) {

  player_score_discard_list <- list()
  player_score_draw_list <- list()

  current_score <- sum(player$score)

  for (i in seq_along(player$information)) {
    temp_information <- player$information
    temp_information[i] <- game_table$discard
    player_score_discard_list[[i]] <- calc_score(information = temp_information, default = 5.37)
    temp_information[i] <- 5.37
    player_score_draw_list[[i]] <- calc_score(information = temp_information, default = 5.37)
  }

  best_discard <- which.min(sapply(player_score_discard_list, sum))
  best_draw <- which.min(sapply(player_score_draw_list, sum))

  if (current_score < best_discard & current_score < best_draw) {
    return(list(action = "pass",
           switch = NA))
  }

  if (best_discard <= best_draw) {
    return(list(action = "discard",
           switch = best_discard))
  } else {
    return(list(action = "draw",
           switch = best_draw))
  }
}
