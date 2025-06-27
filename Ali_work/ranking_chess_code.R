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
