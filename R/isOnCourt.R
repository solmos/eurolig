#' Find if players were on the court
#'
#' \code{isOnCourt} finds if a player or a set of players is on the court
#' for each event of play-by-play data with lineup information.
#'
#' @param pbp A play-by-play data frame
#' @param players A character vector with the nemes of the players
#'
#' @return Logical vector indicating whether the specified players were
#'   on the court at each event of the play-by-play data frame.
#' @export
#'
#' @examples
#' data("samplepbp")
#' isOnCourt(samplepbp, players = c("FERNANDEZ, RUDY", "AYON, GUSTAVO"))
isOnCourt <- function(pbp, players) {
    players_on <- purrr::map_dfc(
        players,
        function(x) stringr::str_detect(pbp$lineups, x)
    )
    colnames(players_on) <- players
    players_together <- apply(players_on, 1, sum) == ncol(players_on)
    players_together
}
