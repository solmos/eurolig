#' Extract game results from a given season
#'
#' \code{extractResults} returns a data frame with all game results for
#' a single season.
#'
#' \code{extractResults} is useful for the current season (2019).
#' For past seasons check the included data set \code{gameresults}.
#'
#'
#' @section Note:
#' \code{extractResults} takes considerable time since it parses all
#' web pages containing game results for the given season. Looping over
#' many seasons may take a long time.
#'
#'
#' @param season Integer indicating the season
#'
#' @return A data frame with all the games in the given season
#' @export
#'
#' @examples
extractResults <- function(season) {
    base_url <- "https://www.euroleague.net/main/results?seasoncode=E"
    year_url <- paste0(base_url, season)
    phase_urls <- extractPhaseUrls(year_url)

    round_urls <- lapply(as.list(phase_urls), extractRoundUrls) %>%
        unlist()
    results <- vector("list", length(round_urls))
    for (i in seq_along(results)) {
        results[[i]] <- try(extractGames(round_urls[i]), silent = TRUE)
    }

    # Detect games that were not retrieved
    results_correct <- vapply(results,
                              function(x) typeof(x) == "list",
                              logical(1))

    stopifnot(sum(results_correct) >= 1)

    results <- results[results_correct]

    results_df <- do.call(rbind, results)

    team_codes <- allteams[, c("season", "team_code", "team_name")]
    results_output <- results_df %>%
        dplyr::left_join(
            team_codes,
            by = c("season", team_home = "team_name")
        ) %>%
        dplyr::rename(team_code_home = .data$team_code) %>%
        dplyr::left_join(
            team_codes,
            by = c("season", team_away = "team_name")
        ) %>%
        dplyr::rename(team_code_away = .data$team_code)

    results_output
}

extractYearUrls <- function() {
    main_url <- "https://www.euroleague.net/main/results"
    main_html <- xml2::read_html(main_url)
    select_nodes <- main_html %>%
        html_nodes("div.game-center-selector") %>%
        html_nodes("div.styled-select")

    year_urls <- select_nodes[[1]] %>%
        html_nodes("option") %>%
        html_attr("value")

    base_url <- "https://www.euroleague.net"
    paste0(base_url, year_urls)
}

extractPhaseUrls <- function(year_url) {
    page_html <- xml2::read_html(year_url)

    select_nodes <- page_html %>%
        html_nodes("div.game-center-selector") %>%
        html_nodes("div.styled-select")

    phases_urls <- select_nodes[[2]] %>%
        html_nodes("option") %>%
        html_attr("value")

    main_url <- "https://www.euroleague.net"
    paste0(main_url, phases_urls)
}

extractRoundUrls <- function(phase_url) {
    phase_html <- xml2::read_html(phase_url)

    select_nodes <- phase_html %>%
        html_nodes("div.game-center-selector") %>%
        html_nodes("div.styled-select")

    main_url <- "https://www.euroleague.net"
    round_urls <- select_nodes[[3]] %>%
        html_nodes("option") %>%
        html_attr("value") %>%
        paste0(main_url, .)

    round_urls
}


extractGames <- function(round_url) {
    round_html <- xml2::read_html(round_url)

    # First we want to find the information for these games

    ## The name of the round for all the games
    select_nodes <- round_html %>%
        html_nodes("div.game-center-selector") %>%
        html_nodes("div.styled-select")

    round_urls <- select_nodes[[3]] %>%
        html_nodes("option") %>%
        html_attr("value")

    relative_url <- stringr::str_remove(round_url, "https://www.euroleague.net")
    round_idx <- which(round_urls == relative_url)
    round_name <- select_nodes[[3]] %>%
        html_nodes("option") %>%
        html_text() %>%
        .[round_idx]

    round_info <- getRoundInfo(round_url)

    # The following nodes correspond to the game boxes in the website
    game_nodes <- round_html %>%
        html_node("div#main-one") %>%
        html_nodes("div.game.played")

    # Stop if there are no games played (i.e. games have not been played yet)
    stopifnot(length(game_nodes) > 0)

    main_url <- "https://www.euroleague.net"
    game_urls <- game_nodes %>%
        html_node("a") %>%
        html_attr("href") %>%
        paste0(main_url, .)

    game_codes <- game_urls %>%
        stringr::str_extract("gamecode=\\d+") %>%
        stringr::str_remove("gamecode=")

    team_names <- game_nodes %>%
        html_nodes("div.club") %>%
        html_node("span.name") %>%
        html_text()
    home_idx <- seq(1, (length(team_names) - 1), by = 2)
    home_teams <- team_names[home_idx]
    away_teams <- team_names[-home_idx]


    # The tag where scores are located change depending on who wins
    # We select all tags in the games boxes
    # Each team has two span tags, the second of which is the score
    n_tags <- length(team_names) * 2
    score_tag_idx <- seq(2, n_tags, by = 2)
    points <- game_nodes %>%
        html_nodes("div.club") %>%
        html_nodes("span") %>%
        .[score_tag_idx] %>%
        html_text() %>%
        as.integer()
    # The html structure for Season 2019 is a bit different
    # The score is stored in attr data-score instead of as text
    if (sum(is.na(points)) == length(points)) {
        points <- game_nodes %>%
            html_nodes("div.club") %>%
            html_nodes("span") %>%
            .[score_tag_idx] %>%
            html_attr("data-score") %>%
            as.integer()
    }
    points_home <- points[home_idx]
    points_away <- points[-home_idx]

    game_dates <- game_nodes %>%
        html_node("div.info") %>%
        html_node("span.date") %>%
        html_text()

    tibble::tibble(
        season = as.integer(round_info$season),
        phase = round_info$phase,
        round_name = round_name,
        team_home = home_teams,
        points_home = as.integer(points_home),
        team_away = away_teams,
        points_away = as.integer(points_away),
        game_code = as.integer(game_codes),
        date = game_dates,
        round_code = as.integer(round_info$round_number),
        game_url = game_urls
    )
}

# Get the round number, phase and season from a round_url,
# such as "https://www.euroleague.net/main/results?gamenumber=31&phasetypecode=PO&seasoncode=E2017"
getRoundInfo <- function(round_url) {
    round_info_raw <- stringr::str_extract_all(round_url, "=[:alnum:]+")[[1]] %>%
        stringr::str_remove("=E*")
    round_number <- round_info_raw[1]
    phase <- round_info_raw[2]
    season <- round_info_raw[3]
    list(round_number = round_number,
         phase = phase,
         season = season)
}
