#' Extract play-by-play data for a given team
#'
#' Downloads play-by-play data of all games that a given team has played
#' in the specified season
#'
#' @param team A three letter string specifying the team code
#' @param season An integer specifying the starting year of the desired season
#'
#' @return A tibble
#' @export
#'
#' @examples
extract_team_pbp <- function(team, season) {
    game_results <- scrape_team_results(team, season)
    game_codes <- game_results$game_code
    pbp <- extract_pbp(game_codes, season)
    pbp
}
