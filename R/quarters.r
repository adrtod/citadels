
quarter <- function(color, value) {
  quarter <- list()
  quarter$color <- color
  quarter$value <- value
  return(quarter)
}

color <- function(quarter) {
  quarter$color
}

value <- function(quarter) {
  quarter$value
}

hand_quarters <- function(...) {
  hand <- list(...)
  return(hand)
}

table_quarters <- function(yellow = c(0,0,5,5,2,0),
                           blue = c(3,3,4,0,2,0),
                           green = c(5,8,3,3,2,0),
                           red = c(3,3,3,0,3,0),
                           purple = c(0,1,1,1,6,5)) {
  quarters_table <- list(yellow = yellow,
                         blue = blue,
                         green = green,
                         red = red,
                         purple = purple)
  color_names <- names(quarters_table)
  stopifnot(!is.null(color_names), all(!is.na(color_names)))
  len <- length(quarters_table[[1]])
  stopifnot(all(sapply(quarters_table, length) == len))
  return(quarters_table)
}

#' create a shuffled deck of quarters
#' @param quarters_table information on the quarters available in the game
#' @param n_cards integer. number of cards in the deck
deck_quarters <- function(quarters_table = table_quarters(), 
                          n_cards = sum(unlist(quarters_table))) {
  colors <- names(quarters_table)
  
  deck <- list()
  
  for (i in 1:n_cards) {
    sums <- sapply(quarters_table, sum)
    
    # draw a color
    ii <- sample.int(length(colors), 1, replace = FALSE, prob = sums/sum(sums))
    col <- colors[ii]
    # draw a quarter
    val <- sample.int(length(quarters_table[[col]]), 1, replace = FALSE, 
                      prob = quarters_table[[col]]/sum(quarters_table[[col]]))
    
    deck[[i]] <- quarter(col, val)
    
    # decrement counter
    quarters_table[[col]][val] <- quarters_table[[col]][val] - 1    
  }
  
  return(deck)
}

