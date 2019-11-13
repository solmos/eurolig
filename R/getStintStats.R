#' Get stats for stints that players were on the floor
#'
#' \code{getStintStats} parses a play-by-play data frame and returns the
#' statistics while the players were on the court.
#'
#'
#' @param pbp Play-by-play data frame.
#' @param players Character vector with the players on the court.
#' @param format_long Logical scalar indicating how output data frame should
#'   be returned (defaults to TRUE).
#'
#' @return Data frame with stats on offense and defense when players were
#'   on the court. If \code{format_long = TRUE} there will be two rows per
#'   game: one for offense and one for defense. If \code{format_long = FALSE}
#'   offense and defense stats for each game will be on the same row.
#' @export
#'
#' @examples
getStintStats <- function(pbp, players, format_long = TRUE) {
    pbp_players <- pbp[isOnCourt(pbp, players),]
    pbp_stats <- getPbpStats(pbp_players)
    players_var <- paste(players, collapse = " - ")

    pbp_by_game <- split(pbp_players,
                         list(pbp_players$season, pbp_players$game_code))

    poss_by_game <- purrr::map_df(pbp_by_game, getPbpPoss)
    poss_by_game

    team <- unique(pbp$team_code[pbp$player_name %in% players])

    stint_stats_long <- pbp_stats %>%
        dplyr::mutate(
            players = players_var,
            type = ifelse(.data$team_code == team, "offense", "defense")
        ) %>%
        dplyr::select(
            season,
            game_code,
            players,
            type,
            dplyr::everything()
        )

    if (format_long == FALSE) {
        stats_offense <- stint_stats_long %>%
            dplyr::filter(.data$type == "offense") %>%
            dplyr::select(-.data$type)
        stats_defense <- stint_stats_long %>%
            dplyr::filter(.data$type == "defense") %>%
            dplyr::select(-.data$type)
        stint_stats_wide <- dplyr::left_join(
            stats_offense,
            stats_defense,
            by = c("season", "game_code", "players"),
            suffix = c("", "_opp")
        )

        return(stint_stats_wide)
    }

    stint_stats_long
}

# Pbp data seem to underestimate minutes
getStintMin <- function(pbp, players) {
    pbp_by_lineup <- split(pbp, list(pbp$season, pbp$game_code, pbp$lineups))
    lapply(players, function(x) stringr::str_detect(names(pbp_by_lineup), x))
    players_on <- purrr::map_dfc(
        players,
        function(x) stringr::str_detect(names(pbp_by_lineup), x)
    )
    colnames(players_on) <- players
    stints_idx <- apply(players_on, 1, sum) == ncol(players_on)
    players_stints <- pbp_by_lineup[stints_idx]
    min_per_stint <- purrr::map_dbl(
        players_stints,
        function(x) max(x$seconds) - min(x$seconds)
    )

    sum(min_per_stint)
}
