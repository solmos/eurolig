#' Get game codes for a team
#'
#' \code{getGameCodes} returns the game codes for all the games played by
#' the specified team in a season.
#'
#' @param team
#' @param season
#'
#' @return
#' @export
#'
#' @examples
getTeamGameCodes <- function(team, season) {
    filter_idx <- gameresults$season == season &
        (gameresults$team_home == team | gameresults$team_away == team)

    gameresults$game_code[filter_idx]
}

getTeamResults <- function(team, season) {
    filter_idx <- gameresults$season == season &
        (gameresults$team_home == team | gameresults$team_away == team)
    team_results <- gameresults[filter_idx,]
    team_results
}

# team <- "Real Madrid"
# season <- 2018
