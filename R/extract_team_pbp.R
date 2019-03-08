#' Extract play-by-play data for a given team
#'
#' Extracts play-by-play data for all games that a given team has played in the specified season
#'
#' @param team
#' @param season
#'
#' @return
#' @export
#'
#' @examples
extract_team_pbp <- function(team, season) {
    game_results <- scrape_team_results(team, season)
    game_codes <- game_results$game_code
    pbp <- extract_pbp(game_codes, season)
    pbp
}
