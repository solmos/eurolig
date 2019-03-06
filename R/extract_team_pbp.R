extract_team_pbp <- function(team, season) {
    game_results <- scrape_team_results(team, season)
    game_codes <- game_results$game_code
    season_code <- paste0("E", season)
    pbp <- extract_pbp(game_codes, season_code)
    pbp
}
