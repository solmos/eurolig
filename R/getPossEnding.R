getPossessions <- function(pbp, team_code) {
    possesions <- dplyr::case_when(
        pbp$play_type == "D" & pbp$team_code != team_code ~ 1,
        pbp$play_type == "2FGM" & pbp$team_code == team_code ~ 1,
        pbp$play_type == "3FGM" & pbp$team_code == team_code ~ 1,
        pbp$play_type == "FTM" & pbp$last_ft == TRUE &
            pbp$team_code == team_code & pbp$and1 == FALSE ~ 1,
        pbp$play_type == "TO" & pbp$team_code == team_code ~ 1,
        TRUE ~ 0
    )

    sum(possesions)
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
