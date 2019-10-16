#' Correct lineups in play-by-play data
#'
#' \code{fixLineups} corrects the lineups in free throw stints so that free trhows are attributed to the lineups that were on the court when the foul occured
#'
#' @param pbp
#'
#' @return
#' @export
#'
#' @examples
fixLineups <- function(pbp) {
    # Find the time when fts are being shot
    ft_secs <- pbp$seconds[pbp$play_type == "FTA" | pbp$play_type == "FTM"]
    # Filter only the events during those times
    ft_events <- pbp[pbp$seconds %in% ft_secs,]

    ft_stints <- split(ft_events, ft_events$seconds)

    ft_lineups <- purrr::map_df(ft_stints, getFtLineup)

    pbp2 <- pbp %>%
        dplyr::left_join(ft_lineups, by = c("season", "game_code", "play_number"))

    # TODO: Perhaps a more elegant solution by replacing as a whole matrix?
    idx <- which(pbp2$lineups.x != pbp2$lineups.y)
    pbp2$home_player1.x[idx] <- pbp2$home_player1.y[idx]
    pbp2$home_player2.x[idx] <- pbp2$home_player2.y[idx]
    pbp2$home_player3.x[idx] <- pbp2$home_player3.y[idx]
    pbp2$home_player4.x[idx] <- pbp2$home_player4.y[idx]
    pbp2$home_player5.x[idx] <- pbp2$home_player5.y[idx]
    pbp2$away_player1.x[idx] <- pbp2$away_player1.y[idx]
    pbp2$away_player2.x[idx] <- pbp2$away_player2.y[idx]
    pbp2$away_player3.x[idx] <- pbp2$away_player3.y[idx]
    pbp2$away_player4.x[idx] <- pbp2$away_player5.y[idx]
    pbp2$away_player5.x[idx] <- pbp2$away_player5.y[idx]
    pbp2$lineups.x[idx] <- pbp2$lineups.y[idx]

    # Remove the .x that resulted when we merged the two data frames
    col_names <- stringr::str_remove(colnames(pbp2), ".x")
    colnames(pbp2) <- col_names
    pbp_final <- pbp2 %>%
        dplyr::select(-dplyr::ends_with(".y"))

    pbp_final
}


#' Get the lineup on the court at the start of a free throw stint
#'
#' @param ft_stint A data frame
#'
#' @return A data frame with the corrected lineups
#' @export
#'
#' @examples
getFtLineup <- function(ft_stint) {
    # Filter lineup columns with identifying season, game_code and play_number
    player_col_names <- c(paste0("home_player", 1:5),
                          paste0("away_player", 1:5))
    id_col_names <- c("season", "game_code", "play_number")
    col_names <- c(id_col_names, player_col_names, "lineups")
    lineup_df <- ft_stint[, col_names]

    # Get the lineup when the free throw stint started
    initial_lineup <- lineup_df[1, -(1:3)]
    # Place the initial lineup in all events of the free throw stint
    lineup_df[, -(1:3)] <- initial_lineup[rep(1, nrow(ft_stint)),]

    lineup_df
}


