#' create a deck of cards
#'
#' @param n number of deck
#' @return data.table
#' @import data.table
create_deck <- function(n, shuffle = TRUE) {
  deck_dt <- data.table(value = c(rep(c(1:13), 4), 14, 14), suit = c(sapply(c("heart", "diamond", "spade", "club"), function(x) rep(x, 13)), "joker", "joker"))
  deck_dt <- deck_dt[rep(1:.N,n)]

  if (shuffle) {
    deck_dt <- deck_dt[order(runif(.N))]
  }

  return(deck_dt)
}
