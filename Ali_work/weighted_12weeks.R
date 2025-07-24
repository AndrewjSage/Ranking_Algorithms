library(tidyverse)
library(igraph) # For graph analysis
library(Matrix) # Use the Matrix package for sparse matrices
library(MASS)   # We still need ginv for the very first step

Games_12weeks <- read.csv("Ali_work/fide_games_12weeks.csv")

Final_Rankings <- function(Games_12weeks, tol = 1e-4, max_iter = 50) {
  
  ### Data Prep
  df_clean <- Games_12weeks %>%
    transmute(
      White,
      Black,
      Mar = case_when(
        Result == "1-0" ~ 1,
        Result == "0-1" ~ -1,
        Result %in% c("½-½", "1/2-1/2", "1/2–1/2") ~ 0,
        TRUE ~ NA_real_
      )
    ) %>%
    filter(!is.na(Mar), White != Black) # Remove games where a player plays themself
  
  ### Find the largest group of connected players
  # Create a graph where players are nodes and games are edges
  player_graph <- graph_from_data_frame(df_clean[, c("White", "Black")], directed = FALSE)
  
  # Find the separate "islands" or components
  components <- components(player_graph)
  
  # Find the largest component
  largest_component_id <- which.max(components$csize)
  
  # Get the names of all players in that largest group
  main_players <- names(components$membership[components$membership == largest_component_id])
  
  # Filter the original game data to only include games between these players
  df_filtered <- df_clean %>%
    filter(White %in% main_players, Black %in% main_players)
  
  # for debugging
  message(sprintf("Original games: %d. Analyzing the largest connected component with %d players and %d games.",
                  nrow(df_clean), length(main_players), nrow(df_filtered)))
  
  ### Proceed with the ranking algorithm on the filtered data
  df2 <- bind_rows(df_filtered, df_filtered)
  h   <- nrow(df2) / 2
  df2$White[(h + 1):nrow(df2)] <- df_filtered$Black
  df2$Black[(h + 1):nrow(df2)] <- df_filtered$White
  df2$Mar[(h + 1):nrow(df2)]   <- -df_filtered$Mar
  
  df2$White <- factor(df2$White)
  df2$Black <- factor(df2$Black)
  
  games      <- nrow(df2) / 2
  players    <- levels(df2$White)
  numplayers <- length(players)
  
  X <- Matrix(0, nrow = games + 1, ncol = numplayers,
              dimnames = list(NULL, players), sparse = TRUE)
  
  for (i in 1:games) {
    X[i, df2$White[i]] <-  1
    X[i, df2$Black[i]] <- -1
  }
  X[games + 1, ] <- 1
  
  y <- c(df2$Mar[1:games], 0)
  
  # Calculate initial b
  b <- as.numeric(ginv(as.matrix(crossprod(X))) %*% as.matrix(crossprod(X, y)))
  
  b0   <- rep(Inf, length(b))
  iter <- 0
  
  while(sum((b - b0)^2) > tol && iter < max_iter) {
    iter <- iter + 1
    b0 <- b
    
    closeness <- X[1:games, ] %*% b
    w <- abs(scale(closeness))
    
    w_inv_vec <- rep(1, games + 1)
    w_inv_vec[1:games] <- 1/(1 + exp(w))
    w_inv_vec[games + 1] <- 0
    
    XtWX <- crossprod(X, X * w_inv_vec)
    XtWy <- crossprod(X, y * w_inv_vec)
    
    b <- as.numeric(solve(XtWX, XtWy))
  }
  
  message("Converged in ", iter, " iterations.")
  
  Rankings_Table <- data.frame(
    Player = players,
    Rating = as.numeric(b),
    stringsAsFactors = FALSE
  ) %>%
    arrange(desc(Rating)) %>%
    mutate(Rnk = row_number())
  
  return(Rankings_Table)
}

Chess_Rankings_12_Weeks <- Final_Rankings(Games_12weeks)
print(head(Chess_Rankings_12_Weeks, 20))
write.csv(Chess_Rankings_12_Weeks,
          "rankings_12weeks.csv",
          row.names = FALSE)

# I want to filter out by the number of games played
# count games played 
games_played <- bind_rows(
  Games_12weeks %>% transmute(Player = White),
  Games_12weeks %>% transmute(Player = Black)
) %>%
  count(Player, name = "GamesPlayed")

Rankings_With_Counts <- Chess_Rankings_12_Weeks %>%
  left_join(games_played, by = "Player")

Final_Leaderboard <- Rankings_With_Counts %>%
  filter(GamesPlayed >= 15) %>% 
  arrange(desc(Rating)) %>%
  mutate(Rnk = row_number()) # Re-calculate rank after filtering



print(head(Final_Leaderboard, 20))
write.csv(Final_Leaderboard,
          "12weeks_filtered.csv",
          row.names = FALSE)


Rankings_12weeks_filtered <- read.csv("Ali_work/12weeks_filtered.csv")
