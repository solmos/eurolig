scrapePbp <- function(game_code, season) {
    assertthat::assert_that(
        assertthat::is.scalar(game_code),
        assertthat::is.scalar(season)
        )
    base_api <- "https://live.euroleague.net/api/PlayByPlay"
    season_code <- paste0("E", season)

    api_request <- httr::GET(
        base_api,
        query = list(gamecode = game_code, seasoncode = season_code),
        httr::timeout(2)
        ) %>%
        httr::stop_for_status()

    # How to handle errors in requests
    error_message <- paste0("Unable to retrieve game ", game_code, ".")
    assertthat::assert_that(
        length(api_request$content) != 0,
        msg = error_message
        )

    json_data <- httr::content(api_request, as = "text", encoding = "UTF-8")

    raw_data <- jsonlite::fromJSON(json_data)

    raw_data$game_code <- game_code
    raw_data$season <- season

    raw_data
}
