#' Extract game results from a given season
#'
#' @param season Integer indicating the season
#'
#' @return A data frame with all the games in the given season
#' @export
#'
#' @examples
extract_results <- function(season) {
    base_url <- "https://www.euroleague.net/main/results?seasoncode=E"
    year_url <- paste0(base_url, season)
    phase_urls <- extract_phase_urls(year_url)

    round_urls <- lapply(as.list(phase_urls), extract_round_urls) %>%
        unlist()
    results <- vector("list", length(round_urls))
    for (i in seq_along(results)) {
        results[[i]] <- extract_games(round_urls[i])
    }
    results_df <- do.call(rbind, results)

    results_df
}

extract_year_urls <- function() {
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

extract_phase_urls <- function(year_url) {
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

extract_round_urls <- function(phase_url) {
    phase_html <- read_html(phase_url)

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


extract_games <- function(round_url) {
    round_html <- xml2::read_html(round_url)

    # First we want to find the information for these games

    ## The name of the round for all the games
    select_nodes <- round_html %>%
        html_nodes("div.game-center-selector") %>%
        html_nodes("div.styled-select")

    round_urls <- select_nodes[[3]] %>%
        html_nodes("option") %>%
        html_attr("value")

    relative_url <- str_remove(round_url, "https://www.euroleague.net")
    round_idx <- which(round_urls == relative_url)
    round_name <- select_nodes[[3]] %>%
        html_nodes("option") %>%
        html_text() %>%
        .[round_idx]

    round_info <- get_round_info(round_url)

    # The following nodes correspond to the game boxes in the website
    game_nodes <- round_html %>%
        html_nodes("div.game.played")

    main_url <- "https://www.euroleague.net"
    game_urls <- game_nodes %>%
        html_node("a") %>%
        html_attr("href") %>%
        paste0(main_url, .)

    game_codes <- game_urls %>%
        str_extract("gamecode=\\d+") %>%
        str_remove("gamecode=")

    team_names <- game_nodes %>%
        html_nodes("div.club") %>%
        html_node("span.name") %>%
        html_text()
    home_idx <- seq(1, (length(team_names) - 1), by = 2)
    home_teams <- team_names[home_idx]
    away_teams <- team_names[-home_idx]

    points <- game_nodes %>%
        html_nodes("div.club") %>%
        html_text() %>%
        str_extract("\\d+")
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
get_round_info <- function(round_url) {
    round_info_raw <- str_extract_all(round_url, "=[:alnum:]+")[[1]] %>%
        str_remove("=E*")
    round_number <- round_info_raw[1]
    phase <- round_info_raw[2]
    season <- round_info_raw[3]
    list(round_number = round_number,
         phase = phase,
         season = season)
}
