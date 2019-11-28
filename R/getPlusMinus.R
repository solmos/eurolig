#' Get plus-minus for the time a player is on the court
#'
#' \code{getPlusMinus} returns a data frame with the players' plus/minus stats
#' for each game.
#'
#' @param pbp Play-by-play data frame
#' @param players Character vector with the players on the court
#'
#' @return A data frame with plus/minus information for each game
#' @export
#'
#' @examples
getPlusMinus <- function(pbp, players) {
    stats_players_wide <- getStintStats(pbp, players, format_long = FALSE)
    stats_players_wide %>%
        dplyr::mutate(plus_minus = .data$pts - .data$pts_opp) %>%
        dplyr::select(
            .data$season,
            .data$game_code,
            .data$players,
            .data$team_code,
            .data$team_code_opp,
            .data$home,
            .data$poss,
            .data$poss_opp,
            .data$pts,
            .data$pts_opp,
            .data$plus_minus)
}

