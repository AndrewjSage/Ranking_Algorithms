library(tidyverse)
library(R2jags)
library(lubridate)


Games_Data <- read.csv("Ali_work/fide_games_12weeks.csv") %>% 
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

# Games_Data <- read.csv("Ali_work/fide_games_weekly.csv") %>% 
#   mutate(Date = as.Date(Date, format = "%Y-%m-%d"))
# Games_Data <- Games_Data %>%
#   slice_sample(n = 1000, replace = FALSE) %>% 
#   mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

K <- 12 
df <- Games_Data %>%
  transmute(
    Date, White, Black,
    Mar = case_when(
      Result == "1-0" ~ 1, Result == "0-1" ~ -1,
      Result %in% c("½-½", "1/2-1/2", "1/2–1/2") ~ 0, TRUE ~ NA_real_
    ),
    white_dummy = 1
  ) %>%
  filter(!is.na(Mar)) %>%
  mutate(period = as.numeric(cut(Date, breaks = K, labels = 1:K)))


# model
jags_model_new <- "model {
  for(i in 1:n_games) {
    Mar[i] ~ dnorm(strength[White[i], period[i]] - strength[Black[i], period[i]] + white_advantage*white_dummy[i], prec_game)
  }
  sw ~ dnorm(0.995, 100)              # prior on shrinkage parameter s_w
  for(j in 1:n_players) {
    strength[j, 1] ~ dnorm(0, prec_player)
    for(w in 2:n_periods) {
      strength[j, w] = sw * strength[j, w-1] + delta[j, w] 
      
      CauchyDraw[j, w] ~ dt(0, 1, nu)
      delta[j, w] = gamma * CauchyDraw[j, w]
    }
  }

  prec_game <- 1 / (sig_game*sig_game)
  sig_game ~  dunif(0, 100)
  prec_player <- 1 / (sig_player*sig_player)
  sig_player ~  dunif(0, 100)
  gamma ~ dunif(0, 1)
  nu ~ dt(0, 1, 1)T(1,)
  white_advantage ~ dnorm(0, 1/100)
}
"


# running the model
all_players <- sort(unique(c(df$White, df$Black)))

dat <- list(
  Mar = df$Mar, n_games = nrow(df),
  White = as.numeric(factor(df$White, levels = all_players)),
  Black = as.numeric(factor(df$Black, levels = all_players)),
  period = df$period, n_periods = max(df$period, na.rm = TRUE),
  n_players = length(all_players), white_dummy = df$white_dummy
)

parms <- c("strength", "delta", "gamma", "nu", "white_advantage", "sig_game", "sig_player", "sw")

m <- jags(data = dat,parameters.to.save = parms, model.file = textConnection(jags_model_new),
          n.chains = 1,
          n.iter = 500,
          n.burnin = 100,
          n.thin = 1
)



# results (I used some AI to draw results because I was getting stuck here)
results_summary <- m$BUGSoutput$summary
strength_rows_dyn <- grep("strength", rownames(results_summary))   # alternative to srange because there is a large number of players

df_tr <- data.frame(
  Mean_Rating = results_summary[strength_rows_dyn, "mean"],
  Player_ID = as.numeric(gsub("strength\\[(\\d+),(\\d+)\\]", "\\1", rownames(results_summary)[strength_rows_dyn])),
  Period = as.numeric(gsub("strength\\[(\\d+),(\\d+)\\]", "\\2", rownames(results_summary)[strength_rows_dyn]))
) %>%
  mutate(Player = all_players[Player_ID])

plot <- ggplot(df_tr, aes(x = Period, y = Mean_Rating, color = Player)) +
  geom_line(alpha = 0.5) +
  geom_point(alpha = 0.5) +
  theme_bw() +
  labs(x = "Period", y = "Mean") +
  theme(legend.position = "none")

plot

StrengthEst <- results_summary[strength_rows_dyn, "mean"]
StrengthMat <- matrix(StrengthEst, 
                      nrow = dat$n_players, 
                      ncol = dat$n_periods, 
                      byrow = FALSE)

sims <- m$BUGSoutput$sims.matrix
strength_cols_period1 <- grep(paste0("strength\\[\\d+,1\\]"), colnames(sims))
strength_cols_final_period <- grep(paste0("strength\\[\\d+,", dat$n_periods, "\\]"), colnames(sims))
total_change_sims <- sims[, strength_cols_final_period] - sims[, strength_cols_period1]
EstChange <- apply(total_change_sims, 2, mean)
q.05 <- apply(total_change_sims, 2, quantile, 0.05)
q.95 <- apply(total_change_sims, 2, quantile, 0.95)
ImpProb <- apply(total_change_sims, 2, function(x) { mean(x > 0) })
Change_Summary <- data.frame(EstChange, q.05, q.95, ImpProb)


AvgStrength <- rowMeans(StrengthMat)
period_names <- paste0("P", 1:dat$n_periods)
colnames(StrengthMat) <- period_names
Strengths_Table <- data.frame(Player = all_players, AvgStrength) %>%
  bind_cols(as.data.frame(StrengthMat)) %>%
  bind_cols(Change_Summary) %>%
  arrange(desc(!!sym(paste0("P", dat$n_periods)))) %>%
  mutate_if(is.numeric, round, 2)


Strengths_Table %>% select(Player, AvgStrength, P1, !!sym(paste0("P", dat$n_periods)), EstChange, ImpProb)



delta_rows_dyn <- grep("delta", rownames(results_summary))
ChangeEst <- results_summary[delta_rows_dyn, c("mean", "sd")]
df_deltas <- data.frame(
  Mean_Change = ChangeEst[, "mean"], SD = ChangeEst[, "sd"],
  Player_ID = as.numeric(gsub("delta\\[(\\d+),(\\d+)\\]", "\\1", rownames(ChangeEst))),
  Period = as.numeric(gsub("delta\\[(\\d+),(\\d+)\\]", "\\2", rownames(ChangeEst)))
) %>%
  mutate(Player = all_players[Player_ID]) %>%
  filter(abs(Mean_Change) > 0.5) %>%
  arrange(desc(Mean_Change)) %>%
  select(Player, Period, Mean_Change, SD)

df_deltas



Final_Rankings_Dynamic <- df_tr %>%
  filter(Period == max(Period)) %>%
  arrange(desc(Mean_Rating)) %>%
  mutate(Rnk = row_number()) %>%
  select(Rnk, Player, Mean_Rating)

print(head(Final_Rankings_Dynamic, 20))

write.csv(Final_Rankings_Dynamic,
          "rankings_Cauchy.csv",
          row.names = FALSE)


Cauchy_Rankings <- read.csv("Ali_work/rankings_Cauchy.csv")
