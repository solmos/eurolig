#' Extracts and attachs lineups at all events of a play-by-play data frame
#'
#' @param pbp Play-by-play data frame.
#'
#' @keywords internal
#'
#' @return The original play-by-play data with 11 extra variables identifying
#'   the players that were on the floor at each event.
#' @export
#'
#' @examples
attachLineups <- function(pbp) {
    game_code <- unique(pbp$game_code)
    season <- unique(pbp$season)
    starters_names <- extractStarters(game_code, season)

    players_in <- pbp$player_name[pbp$play_type == "IN"]
    players_out <- pbp$player_name[pbp$play_type == "OUT"]

    lineups <- vector("list", length(players_in) + 1)
    lineups[[1]] <- starters_names
    for (i in 2:length(lineups)) {
        starters_names[starters_names == players_out[i - 1]] <- players_in[i - 1]
        lineups[[i]] <- starters_names
    }
    lineups_as_list <- lapply(
        lineups,
        function(x) as.data.frame(t(x), stringsAsFactors = FALSE)
    )

    # Find number of times a lineup should be repeated in pbp
    in_idx <- c(which(pbp$play_type == "IN"), nrow(pbp))
    n_times <- c(in_idx[1], dplyr::lead(in_idx) - in_idx)
    n_times <- n_times[-length(n_times)]


    lineups_df <- purrr::map2_df(lineups_as_list, as.list(n_times),
                          function(df, n) df[rep(1, n),]) %>%
        tibble::as_tibble()
    col_names <- c(paste0("home_player", 1:5),
                   paste0("away_player", 1:5))
    colnames(lineups_df) <- col_names

    # Add column with all players on the court
    lineups_df$lineups <- purrr::pmap_chr(lineups_df, paste, sep = " - ")

    dplyr::bind_cols(pbp, lineups_df) %>%
        fixLineups()
}

#fixed the function extractStarters
extractStarters <- function(game_code, season) {
    pbp<-extractPbp(game_code = game_code, season = season,lineups=FALSE)
    
    players_in1 <- pbp$player_name[pbp$play_type == "IN" & pbp$team_name==unique(pbp$home_team)]
    players_in2 <- pbp$player_name[pbp$play_type == "IN" & pbp$team_name==unique(pbp$away_team)]
    
    players_out1 <- pbp$player_name[pbp$play_type == "OUT" & pbp$team_name==unique(pbp$home_team)]
    players_out2 <- pbp$player_name[pbp$play_type == "OUT" & pbp$team_name==unique(pbp$away_team)]
    
    ind<-rep(1:length(players_out1)) #indicator variable
    for (i in 1:length(players_out1)){
      ind[i]<-isTRUE(!(players_out1[i] %in% players_in1[1:(i-1)]))
    }
    
    startersH<-players_out1[ind==1] #starters of home team
    
    ind<-rep(1:length(players_out2))
    for (i in 1:length(players_out2)){
      ind[i]<-isTRUE(!(players_out2[i] %in% players_in2[1:(i-1)]))
    }
    
    startersA<-players_out2[ind==1] #starters of away team
    
    starters<-c(startersH,startersA)
    starters}
}

#' Correct lineups in play-by-play data
#'
#' \code{fixLineups} corrects the lineups in free throw stints so that free
#' trhows are attributed to the lineups that were on the court when
#' the foul occured.
#'
#' @param pbp A play-by-play data frame.
#'
#' @keywords internal
#'
#' @return A data frame with the corrected lineups
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
#' @keywords internal
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
