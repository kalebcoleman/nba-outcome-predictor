################################################################################
# R/compute_recent_stats.R
#
# Compute 5-game rolling net rating and eFG% per team, per game,
# using only prior games (pre-game stats), with first-game default = 0.
################################################################################

library(dplyr)
library(slider)
library(hoopR)
library(tidyr)

#' Compute 5-game pre-game rolling net rating & eFG% over seasons
#'
#' @param seasons Integer vector of seasons (e.g. 2002:2025)
#' @param window  Integer, rolling window size (default 5)
#' @return Tibble with columns:
#'   game_id,
#'   roll5_net_home, roll5_net_away,
#'   roll5_efg_home, roll5_efg_away
#' @export
compute_recent_stats <- function(seasons = 2002:2025,
                                 window  = 5) {
  # 1) Load team-box into long form
  tb <- lapply(seasons, function(y) {
    hoopR::load_nba_team_box(seasons = y) %>%
      filter(season_type == 2) %>%
      transmute(
        game_id,
        game_date,
        team        = team_display_name,
        side        = team_home_away,       # "home"/"away"
        pts_for     = team_score,
        pts_against = opponent_team_score,
        efg         = (field_goals_made +
                         0.5 * three_point_field_goals_made) /
          field_goals_attempted
      )
  }) %>% bind_rows()

  # 2) Compute **pre-game** rolling stats
  tb_roll <- tb %>%
    arrange(team, game_date) %>%
    group_by(team) %>%
    mutate(
      # lag the raw series so current game isn't included
      lag_pts_for   = lag(pts_for,   default = NA_real_),
      lag_pts_again = lag(pts_against, default = NA_real_),
      lag_efg       = lag(efg,       default = NA_real_),

      # slide over up to `window` prior games
      roll_for   = slide_dbl(lag_pts_for,   mean, .before = window-1, .complete = FALSE),
      roll_again = slide_dbl(lag_pts_again, mean, .before = window-1, .complete = FALSE),
      roll_net   = roll_for - roll_again,
      roll_efg   = slide_dbl(lag_efg,       mean, .before = window-1, .complete = FALSE)
    ) %>%
    ungroup() %>%
    # replace initial NAs (first game) with 0
    mutate(
      roll_net = if_else(is.na(roll_net), 0, roll_net),
      roll_efg = if_else(is.na(roll_efg), 0, roll_efg)
    ) %>%
    select(game_id, side, roll_net, roll_efg)

  # 3) Pivot to wide
  recent_wide <- tb_roll %>%
    pivot_wider(
      id_cols     = game_id,
      names_from  = side,
      values_from = c(roll_net, roll_efg),
      names_sep   = "_"
    ) %>%
    rename(
      roll5_net_home = roll_net_home,
      roll5_net_away = roll_net_away,
      roll5_efg_home = roll_efg_home,
      roll5_efg_away = roll_efg_away
    )

  recent_wide
}
