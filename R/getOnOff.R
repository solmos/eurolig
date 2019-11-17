#' Get on/off statistics from play-by-play data
#'
#' \code{getOnOffStats} returns team statistics on offense and defense
#' when the specified players were on the court together and when they
#' were on the bench.
#'
#' @param pbp A play-by-play data frame
#' @param players Character vector of full player names
#'
#' @return A data frame with 4 rows per game and 28 variables:
#'   \describe{
#'     \item{season}{Starting year of the season}
#'     \item{game_code}{Game code}
#'     \item{players}{Players that were on/off the court}
#'     \item{on}{Whether players were on the court}
#'     \item{type}{Factor indicating whether the stats correspond to offense/defense}
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
#' data(samplepbp)
#' madrid_players <- c("FERNANDEZ, RUDY", "AYON, GUSTAVO")
#' getOnOffStats(pbp = samplepbp, players = madrid_players)
getOnOffStats <- function(pbp, players) {
    # Need to filter only games where players appear
    game_codes <- unique(pbp$game_code[isOnCourt(pbp, players)])

    assertthat::assert_that(
        length(game_codes) > 0,
        msg = "Players do not appear in pbp.")

    pbp <- pbp %>%
        dplyr::filter(.data$game_code %in% game_codes)

    pbp_on <- pbp[isOnCourt(pbp, players),]
    pbp_off <- pbp[!isOnCourt(pbp, players),]

    team <- unique(pbp$team_code[pbp$player_name %in% players])
    players_var <- paste(players, collapse = " - ")
    stats_on <- getPbpStats(pbp_on) %>%
        dplyr::mutate(
            on = TRUE,
            players = players_var,
            type = ifelse(.data$team_code == team, "offense", "defense")
        )
    stats_off <- getPbpStats(pbp_off) %>%
        dplyr::mutate(
            on = FALSE,
            players = players_var,
            type = ifelse(.data$team_code == team, "offense", "defense")
        )

    on_off_stats <- dplyr::bind_rows(stats_on, stats_off) %>%
        dplyr::mutate(
            players = players_var,
            type = ifelse(.data$team_code == team, "offense", "defense")
        ) %>%
        dplyr::select(
            .data$season,
            .data$game_code,
            .data$players,
            .data$on,
            .data$type,
            .data$team_code,
            .data$home,
            dplyr::everything()
        )

    on_off_stats
}
