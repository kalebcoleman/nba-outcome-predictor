################################################################################
# R/compute_elo.R
#
# Compute continuous Elo ratings across an entire schedule (multi‑season or single).
################################################################################

#' Compute Elo ratings for every game
#'
#' @param res      A tibble with columns:
#'                 - game_id    (unique identifier)
#'                 - game_date  (Date)
#'                 - home_team  (string)
#'                 - away_team  (string)
#'                 - Win_Home   (logical; TRUE if home team won)
#' @param k        Numeric. Elo K‑factor (default 20).
#' @param baseline Numeric. Starting Elo for every team on their first game (default 1500).
#' @param hfa      Numeric. Home‑court advantage boost (default 100).
#' @return A tibble with columns:
#'         game_id, Elo_H (pre‑game), Elo_A (pre‑game), Elo_Diff = Elo_H − Elo_A.
#' @importFrom dplyr select arrange mutate
#' @export
compute_elo <- function(res,
                        k = 20,
                        baseline = 1500,
                        hfa = 100) {
  df <- res %>%
    select(game_id, game_date, home_team, away_team, Win_Home) %>%
    arrange(game_date)

  teams   <- unique(c(df$home_team, df$away_team))
  ratings <- setNames(rep(baseline, length(teams)), teams)

  out <- df %>% mutate(Elo_H = NA_real_, Elo_A = NA_real_)

  for (i in seq_len(nrow(df))) {
    h     <- df$home_team[i]
    a     <- df$away_team[i]
    win_h <- df$Win_Home[i]

    # pre‐game ratings
    out$Elo_H[i] <- ratings[h]
    out$Elo_A[i] <- ratings[a]

    # apply home‐court advantage
    r_h <- ratings[h] + hfa
    r_a <- ratings[a]

    # expected score and update
    e_h <- 1 / (1 + 10^((r_a - r_h) / 400))
    s_h <- if (win_h) 1 else 0

    ratings[h] <- ratings[h] + k * (s_h - e_h)
    ratings[a] <- ratings[a] + k * ((1 - s_h) - (1 - e_h))
  }

  out %>%
    mutate(Elo_Diff = Elo_H - Elo_A) %>%
    select(game_id, Elo_H, Elo_A, Elo_Diff)
}


compute_alltime_elo <- function(res,
                                k = 20,
                                baseline = 1500,
                                hfa = 100) {
  df <- res %>%
    select(game_id, game_date, home_team, away_team, Win_Home) %>%
    arrange(game_date)

  teams   <- unique(c(df$home_team, df$away_team))
  ratings <- setNames(rep(baseline, length(teams)), teams)

  out <- df %>% mutate(AllTimeElo_H = NA_real_, AllTimeElo_A = NA_real_)

  for (i in seq_len(nrow(df))) {
    h     <- df$home_team[i]
    a     <- df$away_team[i]
    win_h <- df$Win_Home[i]

    out$AllTimeElo_H[i] <- ratings[h]
    out$AllTimeElo_A[i] <- ratings[a]

    r_h <- ratings[h] + hfa
    r_a <- ratings[a]

    e_h <- 1 / (1 + 10^((r_a - r_h) / 400))
    s_h <- if (win_h) 1 else 0

    ratings[h] <- ratings[h] + k * (s_h - e_h)
    ratings[a] <- ratings[a] + k * ((1 - s_h) - (1 - e_h))
  }

  out %>%
    mutate(AllTimeElo_Diff = AllTimeElo_H - AllTimeElo_A) %>%
    select(game_id, AllTimeElo_H, AllTimeElo_A, AllTimeElo_Diff)
}
