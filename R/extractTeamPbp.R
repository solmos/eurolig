#' Extract play-by-play data for a given team
#'
#' Downloads play-by-play data of all games that a given team has played
#' in the specified season
#'
#' @section Warning:
#' This functions may take over a minute to return the large output data frame
#'
#' @param team A three letter string specifying the team code
#' @param season An integer specifying the starting year of the desired season
#'
#' @return A tibble
#' @export
#'
#' @examples
extractTeamPbp <- function(team, season) {
    game_results <- extractTeamResults(team, season)
    game_codes <- game_results$game_code
    pbp <- purrr::map_df(game_codes, extractPbp, season = season)
    pbp
}
