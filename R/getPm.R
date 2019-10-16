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
    offense_row <- which(stats$off_def == "offense")
    defense_row <- which(stats$off_def == "defense")

    offensive_poss <- stats$poss[offense_row]
    defensive_poss <- stats$poss[defense_row]
    pts_scored <- stats$pts[offense_row]
    pts_allowed <- stats$pts[defense_row]

    tibble::tibble(
        offensive_poss,
        pts_scored,
        pts_scored_pp = pts_scored / offensive_poss,
        defensive_poss,
        pts_allowed,
        pts_allowed_pp = pts_allowed / defensive_poss,
        pm = pts_scored - pts_allowed,
        pm_pp = pts_scored_pp - pts_allowed_pp
    )
}



#' Get stats for a single stint
#'
#' \code{getStintStats} parses a play-by-play data frame and returns the statistics while the players were on the court
#'
#' @param pbp Play-by-play data frame
#' @param players Character vector with the players on the court
#'
#' @return Data frame with stats on offense and defense when players were on the court
#' @export
#'
#' @examples
getStintStats <- function(pbp, players) {
    teams <- unique(pbp$team_code)
    teams <- teams[teams != ""]
    team <- unique(pbp$team_code[pbp$player_name %in% players])
    # TODO: assert_that(team is scalar)
    opp_team <- teams[teams != team]
    which_player <- purrr::map_dfc(
        players,
        function(x) stringr::str_detect(pbp$lineups, x)
    )
    players_together <- apply(which_player, 1, sum) == ncol(which_player)
    pbp_players <- pbp[players_together,]

    stint_stats <- pbp_players %>%
        dplyr::mutate(
            off_def = ifelse(team_code == team, "offense", "defense")
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
            pts = 2 * m_2fg + 3 * m_3fg + m_ft
            # The estimated possesions would be
            # poss = a_2fg + a_3fg - orb + tov + 0.4 * a_ft,
        )

    # Add the number of offensive and defensive possessions
    offensive_poss <- getPossessions(pbp_players, team_code = team)
    defensive_poss <- getPossessions(pbp_players, opp_team)
    offense_row <- which(stint_stats$off_def == "offense")
    defense_row <- which(stint_stats$off_def == "defense")
    stint_stats$poss <- NA
    stint_stats$poss[offense_row] <- offensive_poss
    stint_stats$poss[defense_row] <- defensive_poss

    stint_stats %>%
        dplyr::select(.data$off_def, .data$poss, .data$pts, dplyr::everything())
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


