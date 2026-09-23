library(tidyverse)
library(hoopR)
library(patchwork)

# Present formula parameters, from curvesf2.nb / curvesf2_summary.tex
# lw(n,w) = 1 - f(n)*(1 - Min[w/p,1]^gamma), gamma applied directly to
# the win-rate ratio rather than to its complement
NthreshF <- 40
NmaxF    <- 82
AmpF     <- 0.25
kExpF    <- log(2 / AmpF) / log(NmaxF - NthreshF)
pF       <- 0.8057604390
gammaF   <- 0.9897550708

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
    f_N = if_else(N > NthreshF, AmpF * (N - NthreshF)^kExpF, 0),
    l_N = if_else(N <= NthreshF, 1,
                  1 - f_N * (1 - pmin((W_N / N) / pF, 1)^gammaF)),
    cumulative_weighted_loss = cumsum(loss * l_N),
    cumulative_losses        = cumsum(loss)
  ) |>
  ungroup()

final_standings <- team_games |>
  group_by(team) |>
  summarise(total_losses = max(cumulative_losses), .groups = "drop")

worst_5 <- final_standings |> slice_max(total_losses, n = 5) |> pull(team)
best_2  <- final_standings |> slice_min(total_losses, n = 2) |> pull(team)
ten_seeds <- c("MIA", "CHI")

selected_teams <- union(union(worst_5, best_2), ten_seeds)

# Group labels for consistent coloring/legend ordering
group_of <- function(team) {
  case_when(
    team %in% worst_5    ~ "5 Worst",
    team %in% best_2     ~ "2 Best",
    team %in% ten_seeds  ~ "10 Seeds",
    TRUE ~ "Other"
  )
}

plot_data <- team_games |>
  filter(team %in% selected_teams) |>
  mutate(group = group_of(team),
         team_label = paste0(team, " (", group, ")"))

last_pts <- plot_data |> group_by(team) |> slice_max(N, n = 1)

# Order legend: worst to best
team_order <- c(worst_5[order(-final_standings$total_losses[match(worst_5, final_standings$team)])],
                 ten_seeds, best_2)
plot_data$team <- factor(plot_data$team, levels = team_order)
last_pts$team  <- factor(last_pts$team, levels = team_order)

team_colors <- setNames(
  c("#d73027", "#f46d43", "#fdae61", "#fee090", "#abd9e9",
    "#74add1", "#4575b4", "#313695", "#762a83"),
  team_order
)

base_theme <- theme_minimal(base_size = 12) +
  theme(plot.background  = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))

p_left <- ggplot(plot_data, aes(x = N, y = cumulative_losses, color = team)) +
  geom_line(linewidth = 1.1) +
  geom_point(data = last_pts, size = 2.5) +
  scale_color_manual(values = team_colors, name = "Team") +
  labs(x = expression(italic(n)), y = "Cumulative Losses") +
  base_theme +
  theme(legend.position = "none")

p_right <- ggplot(plot_data, aes(x = N, y = cumulative_weighted_loss, color = team)) +
  geom_line(linewidth = 1.1) +
  geom_point(data = last_pts, size = 2.5) +
  scale_color_manual(values = team_colors, name = "Team") +
  labs(x = expression(italic(n)), y = "Cumulative Weighted Losses") +
  base_theme +
  theme(legend.position = "right")

combined <- p_left + p_right +
  plot_annotation(
    theme = theme(plot.background = element_rect(fill = "white", color = NA))
  )

out_file <- "graphics/tanking_lines_2panel_selectteams.png"
ggsave(out_file, plot = combined, width = 15, height = 6, dpi = 150)
message("Saved ", out_file)

print(final_standings |> arrange(total_losses), n = Inf)
