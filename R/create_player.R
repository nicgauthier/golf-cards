#' create a player
#'
#' @param id
#' @param cards
#' @param information
#' @param strategy
#' @return list
create_player <- function(id, cards = NA, information = matrix(nrow = 3, ncol = 3), score = matrix(nrow = 3, ncol = 3), completion = rep(FALSE, 3), strategy = "default_strat") {
  player <- list(id = id, cards = cards, information = information, score = score, completion = completion)
  class(player) <- c(class(player), strategy)
  return(player)
}
