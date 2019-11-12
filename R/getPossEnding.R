getPossessions <- function(pbp, team_code) {
    possesions <- dplyr::case_when(
        pbp$play_type == "DRB" & pbp$team_code != team_code ~ 1,
        pbp$play_type == "2FGM" & pbp$team_code == team_code ~ 1,
        pbp$play_type == "3FGM" & pbp$team_code == team_code ~ 1,
        pbp$play_type == "FTM" & pbp$last_ft == TRUE &
            pbp$team_code == team_code & pbp$and1 == FALSE ~ 1,
        pbp$play_type == "TOV" & pbp$team_code == team_code ~ 1,
        TRUE ~ 0
    )

    sum(possesions)
}

#' Get number of possessions from play-by-play data
#'
#' \code{getPbpPoss} returns the number of possesions of each team from
#' the play-by-play data of a single game
#'
#' @param pbp A play-by-play data frame
#'
#' @return A data frame with number of offensive and defensive possesions
#' @export
#'
#' @examples
getPbpPoss <- function(pbp) {

    assertthat::assert_that(
        length(unique(pbp$game_code)) == 1,
        msg = "Only a single game allowed"
    )

    team_codes <- unique(pbp$team_code) %>%
        .[!is.na(.)]

    possesions_team1 <- dplyr::case_when(
        pbp$play_type == "DRB" & pbp$team_code != team_codes[1] ~ 1,
        pbp$play_type == "2FGM" & pbp$team_code == team_codes[1] ~ 1,
        pbp$play_type == "3FGM" & pbp$team_code == team_codes[1] ~ 1,
        pbp$play_type == "FTM" & pbp$last_ft == TRUE &
            pbp$team_code == team_codes[1] & pbp$and1 == FALSE ~ 1,
        pbp$play_type == "TOV" & pbp$team_code == team_codes[1] ~ 1,
        TRUE ~ 0
    ) %>%
        sum()

    possesions_team2 <- dplyr::case_when(
        pbp$play_type == "DRB" & pbp$team_code != team_codes[2] ~ 1,
        pbp$play_type == "2FGM" & pbp$team_code == team_codes[2] ~ 1,
        pbp$play_type == "3FGM" & pbp$team_code == team_codes[2] ~ 1,
        pbp$play_type == "FTM" & pbp$last_ft == TRUE &
            pbp$team_code == team_codes[2] & pbp$and1 == FALSE ~ 1,
        pbp$play_type == "TOV" & pbp$team_code == team_codes[2] ~ 1,
        TRUE ~ 0
    ) %>%
        sum()

    tibble::tibble(
        season = unique(pbp$season),
        game_code = unique(pbp$game_code),
        team_code = team_codes,
        poss = c(possesions_team1, possesions_team2)
    )

}

#' Find when a pbp event ends a possession
#'
#' @param pbp Play-by-play data frame
#'
#' @return
#' @export
#'
#' @examples
getPossEnding <- function(pbp) {
    dplyr::case_when(
        pbp$play_type == "D" & pbp$home == FALSE ~ 1,
        pbp$play_type == "2FGM" & pbp$home == TRUE ~ 1,
        pbp$play_type == "3FGM" & pbp$home == TRUE ~ 1,
        pbp$play_type == "FTM" & pbp$last_ft == TRUE & pbp$home == TRUE ~ 1,
        pbp$play_type == "TO" & pbp$home == TRUE ~ 1,
        # should the end of periods be included?
        # play_type == "EP" & home == TRUE ~ 1,
        pbp$play_type == "D" & pbp$home == TRUE ~ 1,
        pbp$play_type == "2FGM" & pbp$home == FALSE ~ 1,
        pbp$play_type == "3FGM" & pbp$home == FALSE ~ 1,
        pbp$play_type == "FTM" & pbp$last_ft == TRUE & pbp$home == FALSE ~ 1,
        pbp$play_type == "TO" & pbp$home == FALSE ~ 1,
        # play_type == "EP" & home == FALSE ~ 1,
        TRUE ~ 0
    ) %>%
        as.logical()
}
