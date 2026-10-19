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

# Initial data
Games <- International_Soccer_Games_2018_2022

Games <- Games %>% 
  select(-country)

# Retain games with valid dates, as in the original workflow
 Games$Date <- as.Date(
   Games$date,
   format = "%m/%d/%Y"
 )

Games <- Games %>%
   filter(!is.na(Date))

# Games <- games

# Remove games with missing goal differentials or team names
Games <- Games %>%
  filter(
    !is.na(margin),
    !is.na(home_team),
    !is.na(away_team)
  )

# Numeric goal differential
Games$GoalDiff <- as.numeric(Games$margin)

# Remove any non-finite goal differentials
Games <- Games %>%
  filter(is.finite(GoalDiff))


# Identify and define all teams

all_teams <- sort(
  unique(
    c(
      Games$home_team,
      Games$away_team
    )
  )
)

Games$Home <- match(
  Games$home_team,
  all_teams
)

Games$Away <- match(
  Games$away_team,
  all_teams
)


# Model definition to be used in JAGS

model <- "

model {

  # Goal differential depends only on team strengths
  for (i in 1:n_games) {

    GoalDiff[i] ~ dnorm(
      strength[Home[i]] - strength[Away[i]],
      tau_game
    )

  }


  # Prior for each team's strength
  for (j in 1:n_teams) {

    strength[j] ~ dnorm(
      0,
      tau_prior
    )

  }


  # Game-level standard deviation
  sig_game ~ dunif(
    0,
    10
  )

  tau_game <- 1 / pow(
    sig_game,
    2
  )


  # Fixed prior variance for team strength
  # Precision = 1 / variance
  tau_prior <- 0.01

}

"


# Store relevant data into JAGS

dat <- list(
  
  GoalDiff = as.numeric(
    Games$GoalDiff
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
  
  n_teams = as.integer(
    length(all_teams)
  )
  
)

# Establish Normal-Normal Bayesian model

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
  "sig_game"
)


# MCMC sampling
r <- coda.samples(
  model = m,
  variable.names = parms,
  n.iter = 10000,
  thin = 1
)


# Extract posterior estimates
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
  summary_stats$quantiles
)


# Team strength estimates
strength_df <- summary_df %>%
  
  inner_join(
    quant_df,
    by = "param"
  ) %>%
  
  filter(
    grepl("^strength\\[[0-9]+\\]$", param)
  ) %>%
  
  mutate(
    
    Team_index = as.integer(
      sub(
        "strength\\[([0-9]+)\\]",
        "\\1",
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
    Team,
    Mean,
    Median,
    LCL,
    UCL
  )


# Establish final rankings
final_rankings <- strength_df %>%
  
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
print(tail(final_rankings))


# Top 10 teams
top_teams <- final_rankings %>%
  
  slice_head(
    n = 10
  ) %>%
  
  pull(
    Team
  )

print(top_teams)


# Team strength rankings
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
  
  geom_errorbar(
    aes(
      xmin = LCL,
      xmax = UCL
    ),
    orientation = "y",
    height = 0.2
  ) +
  
  theme_minimal() +
  
  labs(
    title = "Bayesian Soccer Team Strength Rankings",
    subtitle = "Posterior means with 95% credible intervals",
    x = "Estimated Team Strength",
    y = "Team"
  )

print(RankingGraph)


# Distribution of final team strengths

MeanDistribution <- ggplot(
  final_rankings,
  aes(x = Mean)
) +
  
  geom_histogram(
    bins = 30,
    color = "black",
    fill = "steelblue",
    alpha = 0.8
  ) +
  
  geom_vline(
    xintercept = mean(final_rankings$Mean),
    linetype = "dashed",
    linewidth = 1
  ) +
  
  theme_minimal() +
  
  labs(
    title = "Distribution of Bayesian Team Strength Ratings",
    subtitle = "Posterior mean estimates across all national teams",
    x = "Estimated Team Strength (Goal Differential)",
    y = "Number of Teams"
  )

print(MeanDistribution)


# Posterior estimates
model_parameters <- summary_df %>%
  
  filter(
    param == "sig_game"
  ) %>%
  
  transmute(
    Parameter = param,
    Mean = Mean,
    SD = SD
  )

print(model_parameters)