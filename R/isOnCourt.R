#' Find if players were on the court
#'
#' \code{isOnCourt} finds if a player or a set of players is on the court
#' for each event of play-by-play data with lineup information.
#'
#' @param pbp A play-by-play data frame
#' @param players A character vector with the nemes of the players
#'
#' @return
#' @export
#'
#' @examples
isOnCourt <- function(pbp, players) {
    players_on <- purrr::map_dfc(
        players,
        function(x) stringr::str_detect(pbp$lineups, x)
    )
    colnames(players_on) <- players
    players_together <- apply(players_on, 1, sum) == ncol(players_on)
    players_together
}
