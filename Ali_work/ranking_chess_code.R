# I will be doing most of the work in this file.
# Current questions and brainstorm:
# If I upload 52 weeks of data into the .csv, it will have about 400.000 observations.
# That's a lot, but I don't think I will have issues with working 
# with such a dataset.
# Dataset doesn't count for gender.
# Should I do filtering by ELO? It depends on the task I assign. 
# What could be confounding variables? Number of games played, elo, no show up. 

## it takes 5-10 minutes to run the code.

library(dplyr)
library(MASS)

fide_games <- read.csv("Ali_work/fide_games_weekly.csv")

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
write.csv(Ratings, "chess_rankings_3weeks.csv", row.names = FALSE)
