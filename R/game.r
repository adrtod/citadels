n_faceup <- function(n_players, n_roles = 8) {
  if (n_roles==8)
    n_faceup <- c(NA, 0, 0, 2, 1, 0, 0, 0)[n_players]
  else if (n_roles==9)
    n_faceup <- c(NA, 0, 0, 3, 2, 1, 0, 0)[n_players]
  return(n_faceup)
}


turn <- function(players, quarters_deck, roles, max_city_size = 8, quiet = TRUE) {
  n_players <- length(players)
  n_roles <- nrow(roles)
  
  # choose roles ---------------------------------------
  players <- lapply(players, reset_roles)
  
  if (!quiet)
    cat("------------- choice of roles -------------\n")
  
  n_faceup <- n_faceup(n_players, nrow(roles))
  roles <- faceup_random_roles(roles, n=n_faceup, quiet = quiet)
  
  n_facedown <- 1
  roles <- discard_random_roles(roles, n=n_facedown, quiet = quiet)
  ind_discarded <- which(!(roles$is_available | roles$is_faceup))
  
  ind_crown <- which(sapply(players, has_crown))
  i <- 0
  while (any(roles$is_available)) {
    i <- i+1
    
    ind_player <- ((ind_crown+i-2)%%n_players)+1
    
    if (sum(roles$is_available)<2) {
      if (!quiet)
        cat(players[[ind_player]]$name, " ", sep="")
      roles <- undiscard_role(ind_discarded, roles, quiet = quiet)
    }
    
    n_discard <- 0
    if ((n_players==2 && i>1) || i==n_roles-n_faceup-n_facedown-1)
      n_discard <- 1
    
    out <- choose_random_role(players[[ind_player]], roles, n_discard, quiet = quiet)
    players[[ind_player]] <- out$player
    roles <- out$roles
  }
  
  # call roles and play --------------------------------------
  if (!quiet)
    cat("------------- calling roles and play -------------\n")
  for (i_role in  1:nrow(roles)) {
    if (!quiet)
      cat(i_role, ") ", sep="")
    
    role_name <- roles$name[i_role]
    
    # skip if faceup
    if (roles$is_faceup[i_role]) {
      if (!quiet)
        cat("skipping", role_name, "(faceup)\n")
      next
    }
    
    # skip if murdered
    if (roles$is_murdered[i_role]) {
      if (!quiet)
        cat("skipping", role_name, "(murdered)\n")
      next
    }
    
    # call role
    ind_player <- which_player_has_role(role_name, players)
    
    # skip if discarded
    if (length(ind_player)==0) {
      if (!quiet)
        cat("skipping", role_name, "(discarded)\n")
      next
    }
    
    if (!quiet)
      cat("calling", role_name, "\n")
    
    player <- players[[ind_player]]
    roles <- reveal_player(roles, role_name, player$name, quiet = quiet)
    
    # give money to thief if stealed
    if (roles$is_stealed[i_role]) {
      ind_player_thief <- which_player_revealed_role("thief", players, roles)
      players <- steal(ind_player_thief, ind_player, players, quiet = quiet)
    }
    
    # actions
    out <- do.call(role_name, list(ind_player, players, roles, quarters_deck, max_city_size = max_city_size, quiet = quiet))
    players <- out$players
    roles <- out$roles
    quarters_deck <- out$quarters_deck
    
    # check if city is over
    player <- players[[ind_player]]
    
    are_first <- sapply(players, is_first)
    if (all(!are_first) && city_size(player) >= max_city_size)
      player$is_first <- TRUE
    
    players[[ind_player]] <- player
  }
  
  # reveal murdered role
  ind_role_murdered <- which(roles$is_murdered)
  if (length(ind_role_murdered)>0) {
    role_name <- roles$name[ind_role_murdered]
    ind_player <- which_player_has_role(role_name, players)
    if (length(ind_player)>0) {
      player <- players[[ind_player]]
      roles <- reveal_player(roles, role_name, player$name, quiet = quiet)
      
      # give crown to the king if murdered
      if (role_name == "king")
        players <- take_crown(ind_player, players, quiet = quiet)
    }
  }
  
  list(players = players, quarters_deck = quarters_deck)
}

#' play a random game of Citadels
#' @param players list of players
#' @param quarters_deck list of quarters in the deck
#' @param n_coins_init integer. number of coins for each player at the beginning
#' @param n_cards_init integer. number of cards for each player at the beginning
#' @param max_city_size integer. if any player builds \code{max_city_size} the 
#'   game stops at the end of the turn.
#' @param quiet logical. disable text output
#' @export
#' @example demo/play_game.r
game <- function(players = list_players(4), quarters_deck = deck_quarters(), 
                 n_coins_init = 2, n_cards_init = 4, max_city_size = 8, 
                 quiet = TRUE) {
  
  # initialize ----------------------------------------
  if (!quiet)
    cat("============= initialize =============\n")
  
  quarters_deck <- deck_quarters()
  
  players <- lapply(players, function(x) collect_coins(x, n = n_coins_init, quiet = quiet))
  for (i in 1:length(players)) {
    out <- draw_quarters_random_discard(players[[i]], quarters_deck, 
                                        n_draw = n_cards_init, n_discard = 0, quiet = quiet)
    players[[i]] <- out$player
    quarters_deck <- out$quarters_deck
  }
  
  if (!any(sapply(players, has_crown))) {
    players <- choose_random_crown(players, quiet = quiet)
  }
  
  # turns ------------------------------------
  i_turn <- 0
  while (TRUE) {
    i_turn <- i_turn+1
    if (!quiet)
      cat("============= turn", i_turn, "=============\n")
    
    # initialize roles
    roles <- table_roles()
    
    out <- turn(players, quarters_deck, roles, quiet = quiet)
    players <- out$players
    quarters_deck <- out$quarters_deck
    
    are_first <- sapply(players, is_first)
    if (any(are_first))
      break
  }
  
  # scores -----------------------------------
  if (!quiet)
    cat("============= end of game =============\n")
  players <- lapply(players, function(x) compute_score(x, max_city_size = max_city_size,
                                                       quiet = quiet))
  
  scores <- sapply(players, score)
  max_score <- max(scores)
  ind_player_winner <- which(scores == max_score)
  if (length(ind_player_winner)>1) {
    coins <- sapply(players[ind_player_winner], n_coins)
    max_coins <- max(coins)
    ind_player_winner <- ind_player_winner[coins == max_coins]
  }
  
  if (!quiet) {
    cat(players[[ind_player_winner[1]]]$name)
    if (length(ind_player_winner)>1) {
      for (i in 2:length(ind_player_winner)) {
        cat(" and", players[[ind_player_winner[1]]]$name)
      }
      cat(" win the game\n")
    } else {
      cat(" wins the game\n")
    }
  }
}
