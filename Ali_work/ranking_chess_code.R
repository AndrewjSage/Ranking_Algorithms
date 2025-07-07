### I will be doing most of the work in this file.
# Current questions and brainstorm:
# If I upload 52 weeks of data into the .csv, it will have about 400.000 observations.
# That's a lot, but I don't think I will have issues with working 
# with such a dataset.
# The dataset doesn't count for gender.
# Should I do filtering by ELO? It depends on the task I assign. 
### What could be confounding variables? Number of games played, elo, no show up. 


# First test results:
# I am happy with the first results because I can observe some correlation between
# the rankings I made and the official FIDE rankings. However, there are still many
# outliers and inaccurate ratings. I think I should account for Elo and different
# events to make more accurate rankings. After that, I should try working with
# a 52-week dataset instead of a 3-week dataset. (It might take hours to run
# a ranking algorithm for a 52-week dataset)



# run this to see the chess games dataset
fide_games <- read.csv("Ali_work/fide_games_weekly.csv")

# run this to see the rankings
fide_rankings <- read.csv("Ali_work/chess_rankings_3weeks.csv")





## code for ranking chess players
# it takes 5-10 minutes to run the code
library(dplyr)
library(MASS)


# 1) Read the dataset
Games <- read.csv("Ali_work/fide_games_weekly.csv", stringsAsFactors = FALSE)

# 2) Encode results as +1 / 0 / –1
Games <- Games %>%
  mutate(
    Mar = case_when(
      Result == "1-0"       ~  1,
      Result == "0-1"       ~ -1,
      Result %in% c("½-½","1/2-1/2","1/2–1/2") ~  0,
      TRUE                  ~ NA_real_
    )
  ) %>%
  filter(!is.na(Mar))

# 3) Build design matrix
players <- sort(unique(c(Games$White, Games$Black)))
n       <- length(players)
g       <- nrow(Games)

X <- matrix(0, g+1, n, dimnames = list(NULL, players))
for (i in seq_len(g)) {
  X[i, Games$White[i]] <-  1
  X[i, Games$Black[i]] <- -1
}
X[g+1, ] <- 1  # sum-to-zero constraint

# 4) Response vector
y <- c(Games$Mar, 0)

# 5) Solve for ratings
b_hat <- ginv(t(X) %*% X) %*% t(X) %*% y

# 6) Assemble & rank
Ratings <- data.frame(
  Player = players,
  Rating = as.numeric(b_hat[1:n]),
  stringsAsFactors = FALSE
) %>%
  arrange(desc(Rating)) %>%
  mutate(Rank = row_number())

# 7) Inspect and save
print(Ratings)
# write.csv(Ratings, "chess_rankings_3weeks.csv", row.names = FALSE)


###### Filtering by number of games played
library(dplyr)

# 1) Count games per player
games_per_player <- Games %>%
  # stack White and Black into one column
  transmute(Player = White) %>%
  bind_rows(Games %>% transmute(Player = Black)) %>%
  count(Player, name = "GamesPlayed")

Ratings <- read.csv("Ali_work/chess_rankings_3weeks.csv")
# 2) Join to your Ratings table
Ratings2 <- Ratings %>%
  left_join(games_per_player, by = "Player")

# 3) Inspect the bottom & top of the distribution
Ratings2 %>% arrange(GamesPlayed)     %>% head(20)   # fewest games
Ratings2 %>% arrange(desc(GamesPlayed)) %>% head(20) # most games


# 5) (Optional) Filter out players with fewer than N games
N <- 10
Ratings_filtered <- Ratings2 %>%
  filter(GamesPlayed >= N) %>%
  arrange(desc(Rating))

# average games played per player
avg_games <- mean(games_per_player$GamesPlayed)
print(avg_games)
######


##### Including elo in our model (didn't work)

# library(dplyr)
# library(MASS)
# library(lubridate)
# # 1) Read in your weekly FIDE games
# Games <- read.csv("Ali_work/fide_games_weekly.csv", stringsAsFactors = FALSE)
# 
# # 2) Encode outcomes as +1 / 0 / -1
# Games <- Games %>%
#   mutate(
#     Mar = case_when(
#       Result == "1-0"           ~  1,
#       Result == "0-1"           ~ -1,
#       Result %in% c("½-½","1/2-1/2","1/2–1/2") ~ 0,
#       TRUE                      ~ NA_real_
#     ),
#     EloDiff = WhiteElo - BlackElo                # NEW: Elo difference per game
#   ) %>%
#   filter(!is.na(Mar))
# 
# # 3) Build the design matrix
# players <- sort(unique(c(Games$White, Games$Black)))
# n       <- length(players)
# g       <- nrow(Games)
# 
# # Now X has (n players) + 1 Elo‐diff column
# X <- matrix(0, nrow = g + 1, ncol = n + 1,
#             dimnames = list(NULL, c(players, "EloDiffCoef")))
# 
# # fill game rows
# for(i in seq_len(g)) {
#   X[i, Games$White[i]] <-  1
#   X[i, Games$Black[i]] <- -1
#   X[i, "EloDiffCoef"] <- Games$EloDiff[i]
# }
# 
# # constraint: sum of s_j = 0, but no constraint on beta_Elo  
# X[g+1, players]     <- 1    # last row, ones for each player  
# X[g+1, "EloDiffCoef"] <- 0  # no constraint for EloCoef
# 
# # 4) Response vector (margins + zero for constraint)
# y <- c(Games$Mar, 0)
# 
# # 5) Solve for s_1…s_n and beta_Elo
# coef_hat <- ginv(t(X) %*% X) %*% t(X) %*% y
# player_str <- coef_hat[players, 1]        # your s_j estimates
# beta_elo   <- coef_hat["EloDiffCoef", 1]   # the Elo‐diff coefficient
# 
# # 6) Assemble & rank
# Ratings <- data.frame(
#   Player = players,
#   Rating = as.numeric(player_str),
#   stringsAsFactors = FALSE
# ) %>%
#   arrange(desc(Rating)) %>%
#   mutate(Rank = row_number())
# 
# # 7) Inspect results
# cat("Estimated coefficient on Elo‐difference:  ", round(beta_elo, 3), "\n\n")
# print(head(Ratings, 20))   # top 20 players
# 



## currently testing the importance of Events

library(dplyr)
library(MASS)

# 1) Read in your data
Games <- read.csv("Ali_work/fide_games_weekly.csv", stringsAsFactors = FALSE) %>%
  mutate(
    Mar = case_when(
      Result == "1-0"       ~  1,
      Result == "0-1"       ~ -1,
      Result %in% c("½-½","1/2-1/2","1/2–1/2") ~  0,
      TRUE                  ~ NA_real_
    )
  ) %>%
  filter(!is.na(Mar))

# 2) Build list of players and events
players <- sort(unique(c(Games$White, Games$Black)))
events  <- sort(unique(Games$Event))
n_p     <- length(players)
n_e     <- length(events)
g       <- nrow(Games)

# 3) Build design matrix X of size (g + 1) × (n_p + n_e)
#    Columns 1..n_p = player dummies, columns (n_p+1)..(n_p+n_e) = event dummies
X <- matrix(0, nrow = g + 1, ncol = n_p + n_e,
            dimnames = list(NULL, c(players, events)))

for(i in seq_len(g)) {
  # player effects
  X[i, Games$White[i]] <-  1
  X[i, Games$Black[i]] <- -1
  # event effect
  X[i, Games$Event[i]] <-  1
}
# constraint: sum of player strengths = 0; no constraint on events
X[g+1, players] <- 1   
# leave event columns in last row as 0

# 4) Response vector (game margins + zero for the constraint)
y <- c(Games$Mar, 0)

# 5) Solve normal equations faster
XtX  <- crossprod(X)
Xty  <- crossprod(X, y)
coef <- solve(XtX, Xty)

# 6) Extract and rank
player_str <- coef[players]
event_eff  <- coef[events]

Ratings <- data.frame(
  Player = players,
  Rating = as.numeric(player_str),
  stringsAsFactors = FALSE
) %>%
  arrange(desc(Rating)) %>%
  mutate(Rank = row_number())

# 7) Inspect
cat("Top 10 players:\n")
print(head(Ratings, 10))


print(head(tibble(Event = events, Effect = event_eff), 10))


write.csv(Ratings, "chess_player_ratings_withElo.csv", row.names = FALSE)

