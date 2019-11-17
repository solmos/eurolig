#' Extract results for a team
#'
#' Get results of the games of a given team and season
#'
#' @param team A three letter string specifying the team code
#' @param season An integer specifying the starting year of the desired season
#'
#'
#' @return A tibble
#' @export
#'
#' @examples
#' extractTeamResults(team = "BAR", season = 2017)
extractTeamResults <- function(team, season) {
    base_url <- "https://www.euroleague.net/competition/teams/showteam?"
    query <- paste0("clubcode=", team,"&seasoncode=E", season, "#!games")
    team_url <- paste0(base_url, query)
    team_page <- xml2::read_html(team_url)

    game_urls <- team_page %>%
        rvest::html_nodes(".VersusContainer a") %>%
        rvest::html_attr("href")
    game_urls <- paste0("https://www.euroleague.net",
                        game_urls)

    game_scores <- team_page %>%
        rvest::html_nodes(".TeamPhaseGameScoreContainer span") %>%
        rvest::html_text()

    # We get the text inside the column of opponent team names
    opponents_text <- team_page %>%
        rvest::html_nodes(".VersusContainer span") %>%
        rvest::html_text()
    # Each cell containing the opponent name has also "vs" or "at" in front
    #  so all odd indexes contain vs/at and all even the team name
    home_idx <- seq(1, length(opponents_text) - 1, 2)
    game_home <- opponents_text[home_idx]
    game_home[which(game_home == "vs")] <- TRUE
    game_home[which(game_home == "at")] <- FALSE

    team_idx <- seq(2, length(opponents_text), 2)
    game_opponents <- opponents_text[team_idx]

    game_codes <- stringr::str_extract(game_urls, "[0-9]+") %>%
        as.numeric()
    # Games not played have the date of the game (March 22) instead of score
    finished_games <- !stringr::str_detect(game_scores, "[A-z]")
    if (sum(finished_games) != 0) {
        game_urls <- game_urls[finished_games]
        game_scores <- game_scores[finished_games]
        game_home <- game_home[finished_games]
        game_opponents <- game_opponents[finished_games]
        game_codes <- game_codes[finished_games]
    }

    df <- tibble::tibble(
        opponent = game_opponents,
        score = game_scores,
        home = game_home,
        game_code = game_codes,
        season = season,
        url = game_urls
    )

    df
}
