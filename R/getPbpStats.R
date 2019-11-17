#' Get team statistics from play-by-play data
#'
#' \code{getPbpStats} returns boxscore team statistics from play-by-play data.
#'
#' @param pbp A play-by-play data frame
#'
#' @return A data frame with 2 rows per game and 25 variables:
#'   @return A data frame with 4 rows per game and 28 variables:
#'   \describe{
#'     \item{season}{Starting year of the season}
#'     \item{game_code}{Game code}
#'     \item{team_code}{Team code.}
#'     \item{home}{Whether the team was playing at home or not}
#'     \item{fg2a}{2PA}
#'     \item{fg2m}{2PM}
#'     \item{fg2_pct}{2P\%}
#'     \item{fg3a}{3PA}
#'     \item{fg3m}{3PM}
#'     \item{fg3_pct}{3P\%}
#'     \item{fga}{FGA}
#'     \item{fgm}{FGM}
#'     \item{fg_pct}{FG\%}
#'     \item{fta}{FTA}
#'     \item{ftm}{FTM}
#'     \item{ft_pct}{FT\%}
#'     \item{orb}{ORB}
#'     \item{drb}{DRB}
#'     \item{tov}{TOV}
#'     \item{ast}{AST}
#'     \item{stl}{STL}
#'     \item{cpf}{Comitted personal fouls}
#'     \item{blk}{BLK}
#'     \item{pts}{PTS}
#'     \item{poss}{Possessions}
#'   }
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
            fg2_pct = ifelse(.data$fg2a == 0, 0, .data$fg2m / .data$fg2a),
            fg3a = sum(.data$play_type == "3FGM" | .data$play_type == "3FGA"),
            fg3m = sum(.data$play_type == "3FGM"),
            fg3_pct = ifelse(.data$fg3a == 0, 0, .data$fg3m / .data$fg3a),
            fga = .data$fg2a + .data$fg3a,
            fgm = .data$fg2m + .data$fg3m,
            fg_pct = ifelse(.data$fga == 0, 0, .data$fgm / .data$fga),
            fta = sum(.data$play_type == "FTM" | .data$play_type == "FTA"),
            ftm = sum(.data$play_type == "FTM"),
            ft_pct = ifelse(.data$fta == 0, 0, .data$ftm / .data$fta),
            orb = sum(.data$play_type == "ORB"),
            drb = sum(.data$play_type == "DRB"),
            tov = sum(.data$play_type == "TOV"),
            ast = sum(.data$play_type == "AST"),
            stl = sum(.data$play_type == "STL"),
            cpf = sum(.data$play_type == "CPF"),
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
