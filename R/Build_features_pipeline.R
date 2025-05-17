################################################################################
# R/build_features_all.R
#
# - Sources custom Elo functions
# - Loads NBA data via hoopR for seasons 2002-2024
# - Computes per-season Elo and continuous all-time Elo
# - Pivots team stats to home_/away_
# - Assembles one master features table for modeling
################################################################################

# Dependencies
library(dplyr)
library(hoopR)
library(tidyr)

#-------------------------------------------------------------------------------
# Custom Elo functions are defined in R/compute_elo.R and R/compute_alltime_elo.R
# They will be loaded automatically by devtools::load_all(), so no need to source here.

build_features_all <- function(
    seasons    = 2002:2024,
    k          = 20,
    baseline   = 1500,
    hfa        = 100
) {
  # 1) Load and combine schedules ----------------------------------------------
  sched_all <- lapply(seasons, function(y) {
    hoopR::load_nba_schedule(y) %>%
      filter(season_type == 2) %>%              # 2 = Regular season
      transmute(
        game_id,
        game_date = game_date,
        season    = y,
        home_score = home_score,
        away_score = away_score,
        home_team  = home_display_name,
        away_team  = away_display_name,
        Win_Home   = home_score > away_score
      )
  }) %>% bind_rows()

  # 2) Compute continuous all-time Elo -----------------------------------------
  alltime_elo <- compute_alltime_elo(
    res      = sched_all,
    k        = k,
    baseline = baseline,
    hfa      = hfa
  )

  # 3) Define stats columns and helper to pivot team stats --------------------
  stats_cols <- c(
    "team_score", "assists", "blocks", "defensive_rebounds",
    "offensive_rebounds", "total_rebounds", "steals",
    "team_turnovers", "field_goal_pct",
    "three_point_field_goal_pct", "free_throw_pct",
    "fast_break_points", "points_in_paint",
    "opponent_team_score"
  )

  pivot_stats <- function(season) {
    hoopR::load_nba_team_box(seasons = season) %>%
      filter(season_type == 2) %>%
      select(
        game_id,
        side = team_home_away,
        all_of(stats_cols)
      ) %>%
      pivot_wider(
        id_cols     = game_id,
        names_from  = side,
        values_from = all_of(stats_cols),
        names_sep   = "_"
      )
  }

  # 4) Build per-season feature tables ----------------------------------------
  feats_list <- lapply(seasons, function(y) {
    # season-specific schedule
    sched_y <- sched_all %>% filter(season == y)

    # per-season Elo
    elo_y <- compute_elo(
      res      = sched_y,
      k        = k,
      baseline = baseline,
      hfa      = hfa
    )

    # team stats wide
    stats_y <- pivot_stats(y)

    # assemble
    sched_y %>%
      left_join(elo_y,      by = "game_id") %>%
      left_join(stats_y,    by = "game_id")
  })

  # 5) Combine all seasons and add continuous Elo & Target --------------------
  features <- bind_rows(feats_list) %>%
    left_join(alltime_elo, by = "game_id") %>%
    mutate(
      Target = as.integer(Win_Home)
    ) %>%
    # Reorder so all-time Elo follows regular Elo
    select(
      game_id, game_date, season,
      home_score, away_score, home_team, away_team,
      Win_Home, Target,
      Elo_H, Elo_A, Elo_Diff,
      AllTimeElo_H, AllTimeElo_A, AllTimeElo_Diff,
      everything()
    )

  features
}

# Example usage:
# source("R/build_features_all.R")
# train_feats <- build_features_all(2002:2024)
# test_feats  <- build_features_all(2025)
# dim(train_feats)  # ~ total games from 2002-2024
# dim(test_feats)   # ~ games in 2025
