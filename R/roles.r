
role_names <- c("assassin", "thief", "magician", "king", "bishop", 
                "merchant","architect", "warlord")
role_colors <- c(NA, NA, NA, "yellow", "blue",
                 "green", NA, "red")

table_roles <- function(names = role_names,
                        colors = role_colors,
                        is_available = rep(TRUE, length(names)),
                        is_faceup = rep(FALSE, length(names)),
                        is_murdered = rep(FALSE, length(names)),
                        is_stealed = rep(FALSE, length(names)),
                        player_revealed = rep(NA, length(names))) {
  roles <- data.frame(name = names, color = colors, is_available, is_faceup, 
                      is_murdered, is_stealed, player_revealed,
                      row.names = names, stringsAsFactors = FALSE)
  return(roles)
}

discard_random_roles <- function(roles, n = 1, quiet = TRUE) {
  i <- sample.int(sum(roles$is_available), n)
  ind_discarded <- which(roles$is_available)[i]
  roles$is_available[ind_discarded] <- FALSE
  
  if(!quiet)
    cat("discards", roles$name[ind_discarded], "\n")
  
  return(roles)
}

faceup_random_roles <- function(roles, n = 1, quiet = TRUE) {
  can_faceup <- roles$is_available & (roles$name != "king") # avoid faceup the king
  i <- sample.int(sum(can_faceup), n)
  ind_faceup <- which(can_faceup)[i]
  roles$is_available[ind_faceup] <- FALSE
  roles$is_faceup[ind_faceup] <- TRUE
  
  if(!quiet && n>0)
    cat("faceup", roles$name[ind_faceup], "\n")
  
  return(roles)
}

undiscard_role <- function(ind_role, roles, quiet = TRUE) {
  roles$is_available[ind_role] <- TRUE
  
  if(!quiet)
    cat("undiscards", roles$name[ind_role], "\n")
  
  return(roles)
}

reveal_player <- function(roles, role_name, player_name, quiet = TRUE) {
  roles[role_name, "player_revealed"] <- player_name
  if (!quiet)
    cat(player_name, "reveals", role_name, "\n")
  return(roles)
}

# role actions functions -------------------------------

assassin <- function(ind_player, players, roles, quarters_deck, quiet = TRUE, ...) {
  # murder
  player <- players[[ind_player]]
  roles <- murder_random_role(player, roles, quiet = quiet)
  
  # standard action
  switch(choose_random_action(player),
         collect_coins = {
           player <- collect_coins(player, quiet = quiet)
         },
         draw_quarters = {
           out <- draw_quarters_random_discard(player, quarters_deck, quiet = quiet)
           player <- out$player
           quarters_deck <- out$quarters_deck
         }
  )
  player <- build_random_quarter(player, quiet=quiet)
  players[[ind_player]] <- player
  
  return(list(players = players, roles = roles, quarters_deck = quarters_deck))
}

thief <- function(ind_player, players, roles, quarters_deck, quiet = TRUE, ...) {
  # steal
  player <- players[[ind_player]]
  roles <- steal_random_role(player, roles, quiet = quiet)
  
  # standard action
  switch(choose_random_action(player),
         collect_coins = {
           player <- collect_coins(player, quiet = quiet)
         },
         draw_quarters = {
           out <- draw_quarters_random_discard(player, quarters_deck, quiet = quiet)
           player <- out$player
           quarters_deck <- out$quarters_deck
         }
  )
  player <- build_random_quarter(player, quiet=quiet)
  players[[ind_player]] <- player
  
  return(list(players = players, roles = roles, quarters_deck = quarters_deck))
}

magician <- function(ind_player, players, roles, quarters_deck, quiet = TRUE, ...) {
  
  switch(choose_random_action(player, actions=c("exchange_hand", "renew_hand"), prob = c(.5, .5)),
         exchange_hand = {
           players <- exchange_hand_with_random_player(ind_player, players, quiet = quiet)
         },
         renew_hand = {
           out <- renew_random_hand(players[[ind_player]], quarters_deck, quiet = quiet)
           players[[ind_player]] <- out$player
           quarters_deck <- out$quarters_deck
         }
  )
  
  # standard action
  player <- players[[ind_player]]
  switch(choose_random_action(player),
         collect_coins = {
           player <- collect_coins(player, quiet = quiet)
         },
         draw_quarters = {
           out <- draw_quarters_random_discard(player, quarters_deck, quiet = quiet)
           player <- out$player
           quarters_deck <- out$quarters_deck
         }
  )
  player <- build_random_quarter(player, quiet=quiet)
  players[[ind_player]] <- player
  
  return(list(players = players, roles = roles, quarters_deck = quarters_deck))
}

king <- function(ind_player, players, roles, quarters_deck, quiet = TRUE, ...) {
  # take crown
  players <- take_crown(ind_player, players, quiet = quiet)
  
  # collect coins from yellow quarters
  player <- players[[ind_player]]
  player <- collect_coins_from_city(player, roles["king", "color"], quiet = quiet)
  
  # standard action
  player <- players[[ind_player]]
  switch(choose_random_action(player),
         collect_coins = {
           player <- collect_coins(player, quiet = quiet)
         },
         draw_quarters = {
           out <- draw_quarters_random_discard(player, quarters_deck, quiet = quiet)
           player <- out$player
           quarters_deck <- out$quarters_deck
         }
  )
  player <- build_random_quarter(player, quiet=quiet)
  players[[ind_player]] <- player
  
  return(list(players = players, roles = roles, quarters_deck = quarters_deck))
}

bishop <- function(ind_player, players, roles, quarters_deck, quiet = TRUE, ...) {
  # collect coins from blue quarters
  player <- players[[ind_player]]
  player <- collect_coins_from_city(player, roles["bishop", "color"], quiet = quiet)
  
  # standard action
  switch(choose_random_action(player),
         collect_coins = {
           player <- collect_coins(player, quiet = quiet)
         },
         draw_quarters = {
           out <- draw_quarters_random_discard(player, quarters_deck, quiet = quiet)
           player <- out$player
           quarters_deck <- out$quarters_deck
         }
  )
  player <- build_random_quarter(player, quiet=quiet)
  players[[ind_player]] <- player
  
  return(list(players = players, roles = roles, quarters_deck = quarters_deck))
}

merchant <- function(ind_player, players, roles, quarters_deck, quiet = TRUE, ...) {
  # collect one coin
  player <- players[[ind_player]]
  player <- collect_coins(player, n = 1, quiet = quiet)
  
  # collect coins from green quarters
  player <- collect_coins_from_city(player, roles["merchant", "color"], quiet = quiet)
  
  # standard action
  switch(choose_random_action(player),
         collect_coins = {
           player <- collect_coins(player, quiet = quiet)
         },
         draw_quarters = {
           out <- draw_quarters_random_discard(player, quarters_deck, quiet = quiet)
           player <- out$player
           quarters_deck <- out$quarters_deck
         }
  )
  player <- build_random_quarter(player, quiet=quiet)
  players[[ind_player]] <- player
  
  return(list(players = players, roles = roles, quarters_deck = quarters_deck))
}

architect <- function(ind_player, players, roles, quarters_deck, quiet = TRUE, ...) {
  # draw two quarters
  player <- players[[ind_player]]
  out <- draw_quarters_random_discard(player, quarters_deck, n_draw = 2, n_discard = 0, quiet = quiet)
  player <- out$player
  quarters_deck <- out$quarters_deck
  
  # standard action
  switch(choose_random_action(player),
         collect_coins = {
           player <- collect_coins(player, quiet = quiet)
         },
         draw_quarters = {
           out <- draw_quarters_random_discard(player, quarters_deck, quiet = quiet)
           player <- out$player
           quarters_deck <- out$quarters_deck
         }
  )
  player <- build_random_quarter(player, quiet=quiet)
  players[[ind_player]] <- player
  
  return(list(players = players, roles = roles, quarters_deck = quarters_deck))
}

warlord <- function(ind_player, players, roles, quarters_deck, max_city_size = 8, quiet = TRUE) {
  # collect coins from red quarters
  player <- players[[ind_player]]
  player <- collect_coins_from_city(player, roles["warlord", "color"], quiet = quiet)
  
  # destroy quarter
  players[[ind_player]] <- player
  out <- destroy_random_quarter_to_random_player(ind_player, players, roles, quarters_deck, max_city_size, quiet = quiet)
  players <- out$players
  quarters_deck <- out$quarters_deck
    
  # standard action
  player <- players[[ind_player]]
  switch(choose_random_action(player),
         collect_coins = {
           player <- collect_coins(player, quiet = quiet)
         },
         draw_quarters = {
           out <- draw_quarters_random_discard(player, quarters_deck, quiet = quiet)
           player <- out$player
           quarters_deck <- out$quarters_deck
         }
  )
  player <- build_random_quarter(player, quiet=quiet)
  players[[ind_player]] <- player
  
  return(list(players = players, roles = roles, quarters_deck = quarters_deck))
}
