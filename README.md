# Tanking

A weighted-loss anti-tanking mechanism for NBA draft order: instead of an unweighted-loss lottery, teams draft in order of *cumulative weighted losses*, where each loss is penalized more heavily as the season progresses and less heavily for teams closer to playoff contention. The goal is to remove the incentive to tank while avoiding the "cliffs" (large jumps in draft odds between adjacent standings positions) created by lottery-based systems.

## Contents

- **`sloan_abstract.tex` / `sloan_abstract.pdf`** — abstract submitted to the MIT Sloan Sports Analytics Conference describing the formula, its calibration, and results on the 2025-26 NBA season.
- **`curvesf2.nb`** — Mathematica notebook that solves for the formula's calibration parameters (`gamma`, `k`, `p`) from a set of target conditions (e.g., a break-even win rate with no tanking incentive, matching cumulative weighted losses across two win rates).
- **`tanking_lines_2panel_selectteams.R`** — R script that pulls 2025-26 NBA schedule/results data and produces the two-panel figure (unweighted vs. weighted cumulative losses) used in the abstract.
- **`team_final_losses_ranked.R`** — R script that computes season-end unweighted losses, weighted losses, and the resulting draft-order rank change (`ΔRank`) for every team, underlying the abstract's table.
- **`graphics/tanking_lines_2panel_selectteams.png`** — the figure itself.
