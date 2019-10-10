#' Get plus-minus for the time a player is on the court
#'
#' @param pbp Play-by-play data frame
#' @param players Character vector with the players on the court
#'
#' @return
#' @export
#'
#' @examples
getPm <- function(pbp, players) {
    stats <- getStintStats(pbp, players)
    # if (nrow(stats) == 0) return()
    # Change when actual possesions are computed
    poss_mean <- mean(stats$poss)
    stats %>%
        dplyr::select(off_def, pts) %>%
        tidyr::spread(off_def, pts) %>%
        dplyr::transmute(
            pts_scored = Offense,
            pts_allowed = Defense,
            poss = poss_mean,
            ppp_scored = pts_scored / poss,
            ppp_allowed = pts_allowed / poss,
            pm = pts_scored - pts_allowed,
            pm_pp = ppp_scored - ppp_allowed
        )
}

#' Get stats for a single stint
#'
#' @param pbp Play-by-play data frame
#' @param players Character vector with the players on the court
#'
#' @return Data frame with stats on offense and defense
#' @export
#'
#' @examples
getStintStats <- function(pbp, players) {
    team <- unique(pbp$team_code[pbp$player_name %in% players])
    which_player <- purrr::map_dfc(
        players,
        function(x) stringr::str_detect(pbp$lineups, x)
    )
    players_together <- apply(which_player, 1, sum) == ncol(which_player)
    pbp_players <- pbp %>%
        dplyr::filter(players_together)

    pbp_players %>%
        dplyr::mutate(
            off_def = ifelse(team_code == team, "Offense", "Defense")
            ) %>%
        dplyr::group_by(off_def) %>%
        dplyr::summarise(
            m_2fg = sum(play_type == "2FGM"),
            a_2fg = sum(play_type == "2FGA") + m_2fg,
            m_3fg = sum(play_type == "3FGM"),
            a_3fg = sum(play_type == "3FGA") + m_3fg,
            m_ft = sum(play_type == "FTM"),
            a_ft = sum(play_type == "FTA") + m_ft,
            orb = sum(play_type == "O"),
            tov = sum(play_type == "TO"),
            # TODO: Need a way to count actual possesions, not an estimate
            poss = a_2fg + a_3fg - orb + tov + 0.4 * a_ft,
            pts = 2 * m_2fg + 3 * m_3fg + m_ft
        )
}

get_comb_pm <- function(pbp, n) {
    home_players <- pbp$player_name[pbp$home == TRUE] %>%
        .[!is.na(.)] %>%
        unique()
    away_players <- pbp$player_name[pbp$home == FALSE] %>%
        .[!is.na(.)] %>%
        unique()

    comb_home <- combn(home_players, n, simplify = FALSE)
    comb_home_names <- sapply(comb_home, paste, collapse = " - ")
    names(comb_home) <- comb_home_names

    comb_away <- combn(away_players, n, simplify = FALSE)
    comb_away_names <- sapply(comb_away, paste, collapse = " - ")
    names(comb_away) <- comb_away_names
    purrr::map_df(comb_home,
                  function(x) get_pm(pbp, x),
                  .id = "combination")
    combh
}


