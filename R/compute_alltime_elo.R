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
