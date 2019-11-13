#' Get team statistics from play-by-play data
#'
#' \code{getPbpStats} returns boxscore statistics from play-by-play data from
#' one or more games.
#'
#' @param pbp A play-by-play data frame
#'
#' @return
#' @export
#'
#' @examples
getPbpStats <- function(pbp) {

    pbp_by_game <- split(pbp, list(pbp$season, pbp$game_code))

    poss_by_game <- purrr::map_df(pbp_by_game, getPbpPoss)

    pbp %>%
        dplyr::group_by(
            .data$season,
            .data$game_code,
            .data$team_code,
            .data$home
        ) %>%
        dplyr::summarise(
            fg2a = sum(.data$play_type == "2FGM" | .data$play_type == "2FGA"),
            fg2m = sum(.data$play_type == "2FGM"),
            fg2_pct = .data$fg2m / .data$fg2a,
            fg3a = sum(.data$play_type == "3FGM" | .data$play_type == "3FGA"),
            fg3m = sum(.data$play_type == "3FGM"),
            fg3_pct = .data$fg3m / .data$fg3a,
            fga = .data$fg2a + .data$fg3a,
            fgm = .data$fg2m + .data$fg3m,
            fta = sum(.data$play_type == "FTM" | .data$play_type == "FTA"),
            ftm = sum(.data$play_type == "FTM"),
            ft_pct = .data$ftm / .data$fta,
            orb = sum(.data$play_type == "ORB"),
            drb = sum(.data$play_type == "DRB"),
            tov = sum(.data$play_type == "TOV"),
            ast = sum(.data$play_type == "AST"),
            stl = sum(.data$play_type == "STL"),
            cpf = sum(.data$play_type == "CPF"),
            rpf = sum(.data$play_type == "RPF"),
            blk = sum(.data$play_type == "BLK"),
            pts = 2 * .data$fg2m + 3 * .data$fg3m + .data$ftm
        ) %>%
        dplyr::ungroup() %>%
        tidyr::drop_na() %>%
        dplyr::left_join(
            poss_by_game,
            by = c("season", "game_code", "team_code")
        )
}
