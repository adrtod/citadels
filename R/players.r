
#' create a player
#' @param name string. name of the player
#' @param has_crown logical. \code{TRUE} if the player has the crown
#' @export
player <- function(name, has_crown = FALSE) {
  player <- list()
  player$name <- name
  player$has_crown <- has_crown
  player$n_coins <- 0
  player$hand <- list()
  player$city <- list()
  player$roles <- list()
  player$is_first <- FALSE
  player$score <- 0
  return(player)
}

#' create a list of players
#' @param n_players integer. number of players in the list
#' @param names character vector. names of the players
#' @export
list_players <- function(n_players = 2, names = strsplit("ABCDEFGH", "")[[1]]) {
  lapply(names[1:n_players], player)
}

has_crown <- function(player) {
  player$has_crown  
}

n_coins <- function(player) {
  player$n_coins  
}

is_first <- function(player) {
  player$is_first
}

hand_size <- function(player) {
  length(player$hand)  
}

hand_total_value <- function(player) {
  if (hand_size(player)==0)
    return(0)
  sum(sapply(player$hand, value))
}

city_size <- function(player) {
  length(player$city)  
}

city_min_value <- function(player) {
  if (city_size(player)==0)
    return(NA)
  min(sapply(player$city, value))
}

city_total_value <- function(player) {
  if (city_size(player)==0)
    return(0)
  sum(sapply(player$city, value))
}

city_n_colors <- function(player) {
  length(unique(sapply(player$city, color)))
}

take_crown <- function(ind_player, players, quiet = TRUE) {
  ind_crown <- which(sapply(players, has_crown))
  if (length(ind_crown)>0)
    players[[ind_crown]]$has_crown <- FALSE
  
  players[[ind_player]]$has_crown <- TRUE
  
  if (!quiet)
    cat(players[[ind_player]]$name, "takes the crown\n")
  
  return(players)
}

choose_random_crown <- function(players, quiet=TRUE) {
  ind_player <- sample.int(length(players), 1)
  players <- take_crown(ind_player, players, quiet=quiet)
  return(players)
}

compute_score <- function(player, max_city_size = 8, quiet=TRUE) {
  player$score <- city_total_value(player)
  if (player$is_first)
    player$score <- player$score+4
  else if (city_size(player)>=max_city_size)
    player$score <- player$score+2
  
  player$score <- player$score + 3*(city_n_colors(player) == 5)
  
  if (!quiet)
    cat(player$name, "scores", player$score, "points\n")
  
  return(player)
}

score <- function(player) {
  player$score
}

which_player_has_role <- function(role_name, players) {
  which(sapply(players, function(x) role_name %in% x$roles))
}

which_player_revealed_role <- function(role_name, players, roles) {
  which(sapply(players, function(x) x$name == roles[role_name, "player_revealed"]))
}

reset_roles <- function(player) {
  player$roles <- list()
  return(player)
}

# standard actions -------------------------------------

choose_random_role <- function(player, roles, n_discard = 1, quiet = TRUE) {
  i <- sample.int(sum(roles$is_available), 1)
  ind_chosen <- which(roles$is_available)[i]
  
  player$roles <- c(player$roles, roles$name[ind_chosen])
  roles$is_available[ind_chosen] <- FALSE
  
  if(!quiet)
    cat(player$name, "chooses", roles$name[ind_chosen], "\n")
  
  if (n_discard>0) {
    if(!quiet)
      cat(player$name, " ", sep="")
    
    roles <- discard_random_roles(roles, n_discard, quiet = quiet)
  }
  
  return(list(player = player, roles = roles))
}

choose_random_action <- function(player, actions = c("collect_coins", "draw_quarters"), 
                                 prob = 1/(.01+c(n_coins(player), hand_total_value(player)))) {
  
  ind_action <- sample.int(length(actions), 1, prob = prob/sum(prob))
  return(actions[ind_action])
}

collect_coins <- function(player, n = 2, quiet = TRUE) {
  player$n_coins <- player$n_coins + n
  if (!quiet)
    cat(player$name, "collects", n, "coins\n")
  return(player)
}

draw_quarters_random_discard <- function(player, quarters_deck, n_draw = 2, n_discard = 1, quiet = TRUE) {
  if (n_draw>length(quarters_deck))
    stop("the deck size is insufficient")    
    
  quarters <- quarters_deck[1:n_draw]
  quarters_deck <- quarters_deck[-(1:n_draw)]  
  
  if (n_discard>0) {
    ind_quarters <- sample.int(length(quarters), n_discard)    
    quarters_deck <- c(quarters_deck, quarters[ind_quarters])    
    quarters <- quarters[-ind_quarters]
  }
  
  player$hand <- c(player$hand, quarters)
  
  if (!quiet) {
    cat(player$name, "draws", n_draw, "quarters")
    if (n_discard>0) {
      cat(" and discards", n_discard, "quarters")
    }
    cat("\n")
  }
  
  return(list(player = player, quarters_deck = quarters_deck))
}

build_random_quarter <- function(player, quiet=TRUE) {
  # get quarters indices that can be built
  quarter_values <- sapply(player$hand, value)  
  ind_quarters <- which(player$n_coins>=quarter_values)
  
  if (length(ind_quarters)==0)
    return(player)
  
  # draw random quarter
  prob <- 1/quarter_values[ind_quarters]
  i <- sample.int(length(ind_quarters), 1, prob=prob/sum(prob))
  ind_quarter_built <- ind_quarters[i]
  quarter_built <- player$hand[[ind_quarter_built]]
  
  # pay value
  player$n_coins <- player$n_coins - quarter_built$value
  
  # build quarter
  player$hand <- player$hand[-ind_quarter_built]
  player$city <- c(player$city, list(quarter_built))
  
  if (!quiet)
    cat(player$name, "builds a", quarter_built$color, "quarter for", quarter_built$value, "coins\n")
  
  return(player)
}

steal <- function(ind_player_thief, ind_player_stealed, players, quiet=TRUE) {
  player_thief <- players[[ind_player_thief]]
  player_stealed <- players[[ind_player_stealed]]
  
  n_coins <- player_stealed$n_coins
  player_thief$n_coins <- player_thief$n_coins + n_coins
  player_stealed$n_coins <- 0
  
  players[[ind_player_thief]] <- player_thief
  players[[ind_player_stealed]] <- player_stealed
  
  if (!quiet)
    cat(player_stealed$name, "gives", n_coins, "coins to", player_thief$name, "\n")
  return(players)
}

# special actions ---------------------------------------------

murder_random_role <- function(player, roles, quiet = TRUE) {
  ind_murdered <- sample(which(!(roles$name %in% player$roles)), 1)
  roles$is_murdered[ind_murdered] <- TRUE
  if (!quiet)
    cat(player$name, "murders", roles$name[ind_murdered], "\n")
  return(roles)
}

steal_random_role <- function(player, roles, quiet = TRUE) {
  can_be_stealed <- !(roles$name %in% c("assassin", player$roles)) & !roles$is_murdered
  i <- sample.int(sum(can_be_stealed), 1)
  ind_stealed <- which(can_be_stealed)[i]
  roles$is_stealed[ind_stealed] <- TRUE
  
  if (!quiet)
    cat(player$name, "steals", roles$name[ind_stealed], "\n")
  return(roles)
}

exchange_hand_with_random_player <- function(ind_player, players, quiet = TRUE) {
  player <- players[[ind_player]]
  
  hand_sizes <- sapply(players, hand_size)
  ind_players <- setdiff(which(hand_sizes > 0), ind_player) # no self exchange
  
  if (length(ind_players)==0) 
    return(players)
  
  i <- sample.int(length(ind_players), 1)
  ind_player_target <- ind_players[i]
  player_target <- players[[ind_player_target]]
  
  n_cards_out <- length(player$hand)
  n_cards_in <- length(player_target$hand)
  
  cards <- player$hand
  player$hand <- player_target$hand
  player_target$hand <- cards
  
  players[[ind_player]] <- player
  players[[ind_player_target]] <- player_target
  
  if (!quiet)
    cat(player$name, "receives", n_cards_in, "quarters from", player_target$name, 
        "and gives", n_cards_out, "quarters\n")  
  
  return(players)
}

discard_random_cards <- function(player, quarters_deck, n_cards = 1, quiet = TRUE) {
  ind_cards <- sample.int(length(player$hand), n_cards)
  
  cards_out <- player$hand[ind_cards]
  player$hand <- player$hand[-ind_cards]
  quarters_deck <- c(quarters_deck, cards_out)
  
  if (!quiet && n_cards>0)
    cat(player$name, "discards", n_cards, "cards in his hand\n")
  
  return(list(player = player, quarters_deck = quarters_deck))
}

renew_random_hand <- function(player, quarters_deck, quiet = TRUE) {
  if (hand_size(player)==0)
    return(list(player = player, quarters_deck = quarters_deck))
  
  n_cards <- sample(0:length(player$hand), 1)
  
  if (n_cards==0)
    return(list(player = player, quarters_deck = quarters_deck))
  
  out <- discard_random_cards(player, quarters_deck, n_cards, quiet = quiet)
  out <- draw_quarters_random_discard(out$player, out$quarters_deck, n_draw = n_cards, 
                                      n_discard = 0, quiet = quiet)
  
  return(out)
}

collect_coins_from_city <- function(player, quarter_color, quiet = TRUE) {
  colors <- sapply(player$city, color)
  n_coins <- sum(colors == quarter_color)
  player <- collect_coins(player, n_coins, quiet = TRUE)
  
  if(!quiet && n_coins>0)
    cat(player$name, "collects", n_coins, "coins from his", 
        quarter_color, "city quarters\n")
  return(player)
}

destroy_random_quarter_to_random_player <- function(ind_player, players, roles, 
                                                    quarters_deck, max_city_size = 8,
                                                    prob_self = .05, quiet = TRUE) {
  player <- players[[ind_player]]
  
  # find player that revealed bishop
  ind_eveque <- which_player_revealed_role("bishop", players, roles)
  
  # self destroy?
  self_destroy <- FALSE
  if ((city_size(player)>0) && (city_size(player)<max_city_size)
      && ((player$n_coins+1)>city_min_value(player)) 
      && ((length(ind_eveque)!=0) && (ind_player != ind_eveque)))
    self_destroy <- runif(1)<prob_self
  
  # choose a target player
  if (self_destroy) {
    ind_player_target <- ind_player
  } else {    
    # get player indices with at least one quarter and less quarters than max
    city_sizes <- sapply(players, city_size)
    
    ind_players <- setdiff(which(city_sizes>0 & city_sizes<max_city_size), c(ind_player, ind_eveque))
    
    if (length(ind_players)==0)
      return(list(players = players, quarters_deck = quarters_deck))
    
    # get player indices with at least one quarter that can be destroyed
    city_min_values <- sapply(players[ind_players], city_min_value)
    
    ind_players <- ind_players[(player$n_coins+1)>=city_min_values]
    
    if (length(ind_players)==0)
      return(list(players = players, quarters_deck = quarters_deck))
    
    # draw random player
    i <- sample.int(length(ind_players), 1)
    ind_player_target <- ind_players[i]
  }
  
  # CAUTION: ind_player and ind_player_target can be equal!
  # use only for reading
  player_target <- players[[ind_player_target]]
  
  # get quarters indices that can be destroyed
  quarter_values <- sapply(player_target$city, value)  
  ind_quarters <- which((player$n_coins+1)>=quarter_values)
  
  # draw random quarter
  prob <- 1/quarter_values[ind_quarters]
  i <- sample.int(length(ind_quarters), 1, prob=prob/sum(prob))
  ind_quarter_target <- ind_quarters[i]
  quarter_target <- player_target$city[[ind_quarter_target]]
  
  # pay value minus 1
  players[[ind_player]]$n_coins <- players[[ind_player]]$n_coins-(quarter_target$value-1)
  
  # destroy
  players[[ind_player_target]]$city <- players[[ind_player_target]]$city[-ind_quarter_target]
  
  # push back in deck
  quarters_deck <- c(quarters_deck, list(quarter_target))
  
  if(!quiet)
    cat(player$name, "destroys a", quarter_target$color, "quarter in the city of", 
        player_target$name, "for", quarter_target$value, "coins\n")
  
  return(list(players = players, quarters_deck = quarters_deck))
}
