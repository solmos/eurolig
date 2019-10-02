
#' Get lineups at all events of a play-by-play data frame
#'
#' @param pbp Data frame as obtained by extract_pbp()
#'
#' @return Data frame with play-by-play data and lineups attached
#' @export
#'
#' @examples

get_lineups <- function(pbp) {
    game_code <- as.integer(levels(pbp$game_code))
    season <- unique(pbp$season)
    starters_names <- extract_starters(game_code, season)

    players_in <- as.character(pbp$player_name[pbp$play_type == "IN"])
    players_out <- as.character(pbp$player_name[pbp$play_type == "OUT"])

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
    ## TODO: Need a way to get the free throws right after a player subs out
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

    ## Remove this perhaps?? And just give lineups_df?
    dplyr::bind_cols(pbp, lineups_df)
}


extract_starters <- function(game_code, season) {
    base_url <- "https://www.euroleague.net/main/results/showgame?gamecode="
    path_url <- paste0(game_code, "&seasoncode=E", season, "#!boxscore")
    boxscore_url <- paste0(base_url, path_url)
    boxscore_html <- xml2::read_html(boxscore_url)

    starters_names <- boxscore_html %>%
        rvest::html_nodes(".PlayerStartFive") %>%
        rvest::html_text()

    starters_names
}
