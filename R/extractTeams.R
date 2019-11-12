#' Extract info of the teams for a given season
#'
#' @param season Integer value indicating the season
#'
#' @return A data frame with information of all the teams in the given season
#' @export
#'
#' @examples
extractTeams <- function(season) {
    base_url <- "https://www.euroleague.net/competition/teams?seasoncode=E"
    year_url <- paste0(base_url, season)

    year_html <- xml2::read_html(year_url)

    team_nodes <- year_html %>%
        html_nodes("div.item")
    team_names <- team_nodes %>%
        html_nodes("div.RoasterName") %>%
        html_node("a") %>%
        html_text()

    main_url <- "https://www.euroleague.net"
    team_links <- team_nodes %>%
        html_nodes("div.RoasterName") %>%
        html_node("a") %>%
        html_attr("href") %>%
        paste0(main_url, .)

    team_codes <- team_links %>%
        stringr::str_extract("clubcode=[A-Z]+") %>%
        stringr::str_remove("clubcode=")

    team_imgs <- team_nodes %>%
        html_nodes("div.RoasterImage") %>%
        html_node("a") %>%
        html_node("img") %>%
        html_attr("src")

    tibble::tibble(
        season = season,
        team_code = team_codes,
        team_name = team_names,
        team_link = team_links,
        team_img = team_imgs
    )
}
