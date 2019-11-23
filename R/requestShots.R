#' Request shot location data from Euroleague's API
#'
#' \code{requestShots} returns the raw data frame obtained when using
#' Euroleague's API.
#'
#' @keywords internal
#'
#' @param game_code Integer scalar
#' @param season Integer scalar
#'
#' @return
#' @export
#'
#' @examples
requestShots <- function(game_code, season) {
    assertthat::assert_that(
        assertthat::is.scalar(game_code),
        assertthat::is.scalar(season)
    )

    base_api <- "https://live.euroleague.net/api/Points"

    season_code <- paste0("E", season)

    api_request <- tryCatch(
        httr::GET(
            base_api,
            query = list(gamecode = game_code, seasoncode = season_code)
        ),
        message = "Try starting a new R session."
    ) %>%
        httr::stop_for_status()

    # How to handle errors in requests
    error_message <- paste0("Unable to retrieve game ", game_code, ".")
    assertthat::assert_that(
        length(api_request$content) != 0,
        msg = error_message
    )

    json_data <- httr::content(api_request, as = "text", encoding = "UTF-8")

    raw_data <- jsonlite::fromJSON(json_data)[[1]]

    raw_data
}
