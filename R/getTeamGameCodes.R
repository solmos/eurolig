#' Get team results for a single season
#'
#' @param team Team code
#' @param season Integer scalar
#'
#' @return
#' @export
#'
#' @examples
getTeamResults <- function(team, season) {
    filter_idx <- allgames$season == season &
        (allgames$team_code_home == team | allgames$team_code_away == team)
    team_results <- allgames[filter_idx,] %>%
        dplyr::mutate(
            home = .data$team_code_home == team,
            team = team,
            opp = dplyr::case_when(
                .data$home == TRUE ~ .data$team_code_away,
                .data$home == FALSE ~ .data$team_code_home,
            ),
            pts = dplyr::case_when(
                .data$home == TRUE ~ .data$points_home,
                .data$home == FALSE ~ .data$points_away
            ),
            pts_opp = dplyr::case_when(
                .data$home == TRUE ~ .data$points_away,
                .data$home == FALSE ~ .data$points_home
            ),
            win = .data$pts > .data$pts_opp
        ) %>%
        dplyr::select(
            .data$season,
            .data$game_code,
            .data$team,
            .data$opp,
            .data$pts,
            .data$pts_opp,
            .data$win,
            .data$home,
            .data$date
        )

    team_results
}

#' Get game codes for a team
#'
#' \code{getGameCodes} returns the game codes for all the games played by
#' the specified team in a season.
#'
#' @param team Team code. Each team has a three letter code that uniquely
#'   identifies it. See \code{\link{teaminfo}}.
#' @param season Integer scalar identifying the season.
#'
#' @return
#' @export
#'
#' @examples
getTeamGameCodes <- function(team, season) {
    filter_idx <- allgames$season == season &
        (allgames$team_code_home == team | allgames$team_code_away == team)

    allgames$game_code[filter_idx]
}
