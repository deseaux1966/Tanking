library(tidyverse)
library(hoopR)

# Old formula parameters (original power-law timing factor)
alpha   <- 1.0
sigma   <- 6.41
p_old   <- 0.45
nThresh <- 10
beta_old  <- 1
gamma_old <- 1

# Present formula parameters, from curvesf2.nb / curvesf2_summary.tex
# lw(n,w) = 1 - f(n)*(1 - Min[w/p,1]^gamma), gamma applied directly to
# the win-rate ratio rather than to its complement
NthreshF <- 40
NmaxF    <- 83
AmpF     <- 0.25
kExpF    <- log(2 / AmpF) / log(NmaxF - NthreshF)
pF       <- 0.8222173759
gammaF   <- 0.9619825823

# Load 2025-26 regular season games
games <- load_nba_schedule(seasons = 2026)

non_nba_teams <- c("WORLD", "STARS", "STRIPES")

completed <- games |>
  filter(!is.na(home_score), season_type == 2,
         !(home_abbreviation %in% non_nba_teams),
         !(away_abbreviation %in% non_nba_teams))

home <- completed |>
  transmute(game_date = as.Date(game_date),
            team = home_abbreviation,
            loss = as.integer(home_score < away_score))

away <- completed |>
  transmute(game_date = as.Date(game_date),
            team = away_abbreviation,
            loss = as.integer(away_score < home_score))

team_games <- bind_rows(home, away) |>
  arrange(team, game_date) |>
  group_by(team) |>
  mutate(
    N   = row_number(),
    W_N = cumsum(1L - loss),
    l_N_old = if_else(N <= nThresh, 1,
                       1 - alpha * ((N + 2) / (84 - N))^(1 / sigma) *
                         (1 - beta_old * pmin((W_N / N) / p_old, 1))^gamma_old),
    f_N = if_else(N > NthreshF, AmpF * (N - NthreshF)^kExpF, 0),
    l_N_new = if_else(N <= NthreshF, 1,
                       1 - f_N * (1 - pmin((W_N / N) / pF, 1)^gammaF)),
    cumulative_weighted_loss_old = cumsum(loss * l_N_old),
    cumulative_weighted_loss_new = cumsum(loss * l_N_new),
    cumulative_losses            = cumsum(loss)
  ) |>
  ungroup()

# Season-end totals for every team
final_losses <- team_games |>
  group_by(team) |>
  slice_max(N, n = 1) |>
  ungroup() |>
  select(team,
         cumulative_losses,
         cumulative_weighted_loss_old,
         cumulative_weighted_loss_new)

# Rank each loss specification: rank 1 = highest cumulative loss
# (i.e., worst team / best draft position under that specification)
final_losses <- final_losses |>
  mutate(
    rank_losses     = rank(-cumulative_losses, ties.method = "min"),
    rank_old        = rank(-cumulative_weighted_loss_old, ties.method = "min"),
    rank_new        = rank(-cumulative_weighted_loss_new, ties.method = "min")
  ) |>
  arrange(rank_losses)

print(final_losses, n = Inf)

out_file <- "team_final_losses_ranked.csv"
write_csv(final_losses, out_file)
message("Saved ", out_file)
