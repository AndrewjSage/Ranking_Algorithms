knitr::opts_chunk$set(
  echo = FALSE,
  message = FALSE,
  warning = FALSE
)

library(MASS)
library(tidyverse)
library(rjags)
library(coda)

select <- dplyr::select


# Set up games data
Games <- International_Soccer_Games_2018_2022

Games <- Games %>%
  select(-country)

# Convert date
Games$Date <- as.Date(
  Games$date,
  format = "%m/%d/%Y"
)

# Remove games without valid dates
Games <- Games %>%
  filter(!is.na(Date))


# Classify game types
friendlies <- c(
  "Friendly",
  "Friendly tournament"
)

qualifiers <- c(
  "African Nations Cup qualifier",
  "Arab Cup qualifier",
  "Asian Cup qualifier",
  "CONCACAF Champ qual",
  "CONCACAF Nations League q",
  "East Asian Championship qual",
  "European Championship qual",
  "Southeast Asian Champ qual"
)

all_gametypes <- unique(Games$tournament)

noncup_games <- c(
  "Friendly",
  "Friendly tournament",
  "African Nations Cup qualifier",
  "Arab Cup qualifier",
  "Asian Cup qualifier",
  "CONCACAF Champ qual",
  "CONCACAF Nations League q",
  "East Asian Championship qual",
  "European Championship qual",
  "Southeast Asian Champ qual"
)

cup_games <- setdiff(
  all_gametypes,
  noncup_games
)

# Binary indicators
Games$friendly <- ifelse(
  Games$tournament %in% friendlies,
  1, 0
)

Games$qualifier <- ifelse(
  Games$tournament %in% qualifiers,
  1, 0
)

Games$cup <- ifelse(
  Games$tournament %in% cup_games,
  1, 0
)

# Set up teams
all_teams <- sort(
  unique(
    c(Games$home_team, Games$away_team)
  )
)

# Numeric team identifiers
Games$Home <- match(
  Games$home_team,
  all_teams
)

Games$Away <- match(
  Games$away_team,
  all_teams
)


# Create team periods
K <- 20

Games$period <- cut(
  Games$Date,
  breaks = K,
  labels = FALSE
)

# Make sure period is numeric
Games$period <- as.integer(Games$period)


# Normal-Normal Bayesian model
model <- "

model {

  # Games model
  for (i in 1:n_games) {

    y[i] ~ dnorm(
      mu[i],
      tau_game
    )

    mu[i] <-
      strength[Home[i], period[i]]
      -
      strength[Away[i], period[i]]
      +
      alpha * h[i]
      +
      beta_qual * qualifier[i]
      +
      beta_cup * cup[i]
  }


  # Initial team strength
  for (j in 1:n_teams) {

    strength[j, 1] ~ dnorm(
      0,
      tau_prior
    )

    # Strength evolves over time
    for (t in 2:n_periods) {

      strength[j, t] ~ dnorm(
        strength[j, t - 1],
        tau_team
      )

    }
  }


 # Home advantage
  alpha ~ dnorm(
    0,
    0.01
  )


 # Qualifier games
  beta_qual ~ dnorm(
    0,
    0.01
  )


 # Cup games
  beta_cup ~ dnorm(
    0,
    0.01
  )


  # Priors for variability
  # Game-level standard deviation
  sig_game ~ dunif(
    0,
    10
  )

  tau_game <- 1 / pow(
    sig_game,
    2
  )


  # Team-strength evolution standard deviation
  sig_team ~ dunif(
    0,
    2
  )

  tau_team <- 1 / pow(
    sig_team,
    2
  )


  # Initial-strength precision
  tau_prior <- 0.01

}

"


# Data for JAGS
dat <- list(
  
  y = as.numeric(
    Games$margin
  ),
  
  n_games = as.integer(
    nrow(Games)
  ),
  
  Home = as.integer(
    Games$Home
  ),
  
  Away = as.integer(
    Games$Away
  ),
  
  h = as.numeric(
    Games$home
  ),
  
  period = as.integer(
    Games$period
  ),
  
  n_teams = as.integer(
    length(all_teams)
  ),
  
  n_periods = as.integer(
    K
  ),
  
  qualifier = as.integer(
    Games$qualifier
  ),
  
  cup = as.integer(
    Games$cup
  )
)


# Fit the Bayesian model
m <- jags.model(
  textConnection(model),
  data = dat,
  n.chains = 3
)


# Burn-in
update(
  m,
  n.iter = 1000
)


# Parameters to monitor
parms <- c(
  "strength",
  "alpha",
  "sig_game",
  "sig_team",
  "beta_qual",
  "beta_cup"
)


# MCMC sampling
r <- coda.samples(
  model = m,
  variable.names = parms,
  n.iter = 10000,
  thin = 1
)


# Extract posterior results
summary_stats <- summary(r)

summary_df <- as.data.frame(
  summary_stats$statistics
)

summary_df$param <- rownames(
  summary_df
)


quant_df <- as.data.frame(
  summary_stats$quantiles
)

quant_df$param <- rownames(
  quant_df
)


# Extract team strength parameters
strength_df <- summary_df %>%
  
  inner_join(
    quant_df,
    by = "param"
  ) %>%
  
  filter(
    grepl("^strength\\[", param)
  ) %>%
  
  mutate(
    
    Team_index = as.integer(
      sub(
        "strength\\[([0-9]+),([0-9]+)\\]",
        "\\1",
        param
      )
    ),
    
    Period = as.integer(
      sub(
        "strength\\[([0-9]+),([0-9]+)\\]",
        "\\2",
        param
      )
    ),
    
    Team = all_teams[Team_index],
    
    Mean = Mean,
    
    Median = `50%`,
    
    LCL = `2.5%`,
    
    UCL = `97.5%`
    
  ) %>%
  
  select(
    Team_index,
    Period,
    Team,
    Mean,
    Median,
    LCL,
    UCL
  )


# Final Period Rankings
final_period <- max(
  strength_df$Period
)

final_rankings <- strength_df %>%
  
  filter(
    Period == final_period
  ) %>%
  
  arrange(
    desc(Mean)
  ) %>%
  
  mutate(
    Rank = row_number()
  ) %>%
  
  select(
    Rank,
    Team,
    Mean,
    Median,
    LCL,
    UCL
  )


print(final_rankings)


# Top teams
top_teams <- final_rankings %>%
  
  slice_head(
    n = 10
  ) %>%
  
  pull(
    Team
  )

print(top_teams)


# Time series plot data
secondary_top_teams <- c(
  "France",
  "Uruguay",
  "Colombia",
  "Switzerland",
  "Serbia",
  "Croatia",
  "Poland",
  "Japan",
  "United States",
  "Canada"
)


plot_df <- strength_df %>%
  
  filter(
    Team %in% secondary_top_teams
  )


# Time series plot
Graph <- ggplot(
  plot_df,
  aes(
    x = Period,
    y = Mean,
    color = Team
  )
) +
  
  geom_line(
    linewidth = 1
  ) +
  
  theme_minimal() +
  
  labs(
    title = "International Soccer Team Strength Over Time",
    subtitle = "Normal-Normal Bayesian Model",
    y = "Estimated Team Strength",
    x = "Period",
    color = "Team"
  )

print(Graph)


ranking_plot_df <- final_rankings %>%
  
  slice_head(
    n = 20
  ) %>%
  
  mutate(
    Team = reorder(
      Team,
      Mean
    )
  )


RankingGraph <- ggplot(
  ranking_plot_df,
  aes(
    x = Mean,
    y = Team
  )
) +
  
  geom_point(
    size = 3
  ) +
  
  geom_errorbarh(
    aes(
      xmin = LCL,
      xmax = UCL
    ),
    height = 0.2
  ) +
  
  theme_minimal() +
  
  labs(
    title = "Final Bayesian Soccer Strength Rankings",
    subtitle = "Posterior means with 95% credible intervals",
    x = "Estimated Strength",
    y = "Team"
  )

print(RankingGraph)


# Posterior estimates
model_parameters <- summary_stats$statistics %>%
  
  as.data.frame() %>%
  
  mutate(
    Parameter = rownames(.)
  ) %>%
  
  filter(
    Parameter %in% c(
      "alpha",
      "beta_qual",
      "beta_cup",
      "sig_game",
      "sig_team"
    )
  ) %>%
  
  select(
    Parameter,
    Mean,
    SD
  )

print(model_parameters)