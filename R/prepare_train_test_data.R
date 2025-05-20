#-------------------------------------------------------------------------------
# Convenience wrapper: prepare both train and test feature sets
#-------------------------------------------------------------------------------
#' Prepare train and test feature sets
#'
#' @importFrom magrittr %>%
#' @param train_seasons Integer vector of seasons to train on (default 2002:2024)
#' @param test_season Integer season to hold out for testing (default 2025)
#' @param k Numeric, Elo K-factor
#' @param baseline Numeric, starting Elo
#' @param hfa Numeric, home-court advantage
#' @return A list with elements `train_feats` and `test_feats`, each a tibble of features
#' @export
prepare_train_test <- function(
    train_seasons = 2002:2024,
    test_season   = 2025,
    k             = 20,
    baseline      = 1500,
    hfa           = 100
) {
  # 1) Raw feature sets including placeholder all-time Elo
  train_raw <- build_features_all(
    seasons  = train_seasons,
    k        = k,
    baseline = baseline,
    hfa      = hfa
  )
  test_raw <- build_features_all(
    seasons  = test_season,
    k        = k,
    baseline = baseline,
    hfa      = hfa
  )

  # 2) Compute continuous all-time Elo across train + test seasons
  all_seasons <- c(train_seasons, test_season)
  sched_all <- lapply(all_seasons, function(y) {
    hoopR::load_nba_schedule(y) %>%
      filter(season_type == 2) %>%
      transmute(
        game_id,
        game_date,
        home_team = home_display_name,
        away_team = away_display_name,
        Win_Home  = home_score > away_score
      )
  }) %>%
    bind_rows()

  combined_alltime <- compute_alltime_elo(
    res      = sched_all,
    k        = k,
    baseline = baseline,
    hfa      = hfa
  )

  # 3) Drop old AllTimeElo and re-join combined continuous values
  train_feats <- train_raw %>%
    select(-AllTimeElo_H, -AllTimeElo_A, -AllTimeElo_Diff) %>%
    left_join(combined_alltime, by = "game_id") %>%
    select(
      game_id, game_date, season,
      home_score, away_score, home_team, away_team,
      Win_Home, Target,
      Elo_H, Elo_A, Elo_Diff,
      AllTimeElo_H, AllTimeElo_A, AllTimeElo_Diff,
      everything()
    )

  test_feats <- test_raw %>%
    select(-AllTimeElo_H, -AllTimeElo_A, -AllTimeElo_Diff) %>%
    left_join(combined_alltime, by = "game_id") %>%
    select(
      game_id, game_date, season,
      home_score, away_score, home_team, away_team,
      Win_Home, Target,
      Elo_H, Elo_A, Elo_Diff,
      AllTimeElo_H, AllTimeElo_A, AllTimeElo_Diff,
      everything()
    )

  recent <- compute_recent_stats(
    seasons = c(train_seasons, test_season),
    window = 5
  )
  train_feats <- left_join(train_feats, recent, by = "game_id")
  test_feats  <- left_join(test_feats,  recent, by = "game_id")

  train_feats <- train_feats %>%
    mutate(
      net_diff5 = roll5_net_home - roll5_net_away,
      efg_diff5 = roll5_efg_home - roll5_efg_away
    )

  test_feats <- test_feats %>%
    mutate(
      net_diff5 = roll5_net_home - roll5_net_away,
      efg_diff5 = roll5_efg_home - roll5_efg_away
    )

  # 4) Clean missing stats
  train_feats <- tidyr::drop_na(train_feats)
  test_feats  <- tidyr::drop_na(test_feats)

  # Return both sets
  list(
    train_feats = train_feats,
    test_feats  = test_feats
  )
}

# Example usage in your R console:
# library(devtools); load_all()
# res <- prepare_train_test()
# train_feats <- res$train_feats
# test_feats  <- res$test_feats
#
# Run these lines of code below in the R console to get data features
# train_feats <- prepare_train_test()$train_feats
# test_feats  <- prepare_train_test()$test_feats

