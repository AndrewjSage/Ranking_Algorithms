knitr::opts_chunk$set(echo = FALSE, message=FALSE, warning=FALSE)


library(MASS)
library(tidyverse)
library(rjags)

#source("weekly_res_gen.R")
select <- dplyr::select

# Setup Games data
Games <- International.Soccer.Games.2018.2022
Games <- Games %>% select(V2, V3, V4, V7, V9, V10)

Games$Date <- as.Date(Games$V2, format = "%m/%d/%Y")
Games <- Games %>% filter(!is.na(Date))

#Commenting out year filter for now

#Yr <- 2022

#Data <- Games %>% filter(year == Yr)

K <- 20   # K represents the week or time period we're predicting

# Setup teams and data
all_teams <- sort(unique(c(Games$V3, Games$V4)))
Games$Home <- match(Games$V3, all_teams)
Games$Away <- match(Games$V4, all_teams)

#Applying personalized test dataset

cup_games <- Games%>% 
  filter(!(Games$V7 %in% c("Friendly", "Friendly tournament", 
                     "African Nations Cup qualifier", "Arab Cup qualifier", 
                     "Asian Cup qualifier", "CONCACAF Champ qual", 
                     "CONCACAF Nations League q", "East Asian Championship qual",
                     "European Championship qual", "Southeast Asian Champ qual")))

Test_cup_subset <- cup_games[951:1748, ]

Test_indices <- which(duplicated(rbind(Games, Test_cup_subset))[(nrow(Games) + 1):nrow(rbind(Games, Test_cup_subset))])


Data_Test <- dplyr::semi_join(Games, Test_cup_subset, by = colnames(Games))

Data_Train <- dplyr::anti_join(Games, Data_Test, by = colnames(Games))
Data_Train$Date <- as.Date(Data_Train$V2, format = "%m/%d/%Y")
Data_Train <- Data_Train %>% filter(!is.na(Date))
Data_Train$period <- cut(Data_Train$Date, breaks = K, labels = FALSE) #divide into 20 intervals
Data_Test$period <- K+1

Games$period <- NA
Games$period[match(Data_Train$V2, Games$V2)] <- Data_Train$period
Games$period[is.na(Games$period)] <- K + 1




-------------------------------------------------------------------------------------
  ## Bayesian adaptable strengths model
  -------------------------------------------------------------------------------------

# Bayesian Model with jags
  model <- model <- "
model {
  for (i in 1:n_games) {
    y[i] ~ dnorm(
      strength[Home[i], period[i]] -
      strength[Away[i], period[i]] +
      alpha * h[i],
      prec_game
    )
  }

  for (j in 1:n_teams) {
    strength[j, 1] ~ dnorm(0, 0.01)

    for (t in 2:n_periods) {
      strength[j, t] ~ dnorm(strength[j, t - 1], prec_team)
    }
  }

  alpha ~ dnorm(0, 0.01)

  # Game-to-game variability
  sig_game ~ dt(0, 1, 1) T(0, )
  prec_game <- 1 / (sig_game * sig_game)

  # Team strength evolution variability
  sig_team ~ dt(0, 1, 1) T(0, )
  prec_team <- 1 / (sig_team * sig_team)
}
"

#Fit model on actual data

dat <- list(
  y = as.numeric(Games$V10),
  n_games = nrow(Games),
  Home = as.integer(Games$Home),
  Away = as.integer(Games$Away),
  h = as.numeric(Games$V9),
  period = as.integer(Games$period),
  n_teams = length(all_teams),
  n_periods = K + 1
)


m <- jags.model(textConnection(model), dat, n.chains=3)   #run model
parms <- c("strength", "alpha", "sig_game", "sig_team")
r <- coda.samples(m, parms, n.iter=10000, n.burnin=100, thin=1)



summary_df <- as.data.frame(summary(r)$statistics)
summary_df$param <- rownames(summary_df)

quant_df <- as.data.frame(summary(r)$quantiles)
quant_df$param <- rownames(quant_df)

strength_df <- summary_df %>%
  inner_join(quant_df, by = "param") %>%
  filter(grepl("^strength\\[", param)) %>%
  mutate(
    Team_index = as.integer(sub("strength\\[([0-9]+),([0-9]+)\\]", "\\1", param)),
    Period = as.integer(sub("strength\\[([0-9]+),([0-9]+)\\]", "\\2", param)),
    Team = all_teams[Team_index],
    Mean = Mean,           # from summary stats
    Median = `50%`,        # from quantiles
    LCL = `2.5%`,
    UCL = `97.5%`
  ) %>%
  select(Team_index, Period, Team, Mean, Median, LCL, UCL)

final_period <- max(strength_df$Period)
final_rankings <- strength_df %>%
  filter(Period == final_period) %>%
  arrange(desc(Mean)) %>%
  select(Team, Mean, Median, LCL, UCL)

print(final_rankings)

top_teams <- final_rankings %>%
  slice_max(order_by = Mean, n = 10) %>%
  pull(Team)
show(top_teams)

plot_df <- strength_df %>%
  filter(Team %in% top_teams)

Graph <- ggplot(plot_df, aes(x = Period, y = Mean, color = Team)) +
  geom_line(linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Top 10 Teams Strength Over Time",
    y = "Strength",
    x = "Period"
  )

print(Graph)
