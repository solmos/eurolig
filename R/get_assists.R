#' Get who assisted who from play-by-play data
#'
#' @param df Play-by-play data frame as produced by extract_pbp()
#'
#' @return A data frame
#' @export
#'
#' @examples
get_assists <- function(df, team, and1 = FALSE) {
    if (!missing(team)) {
        df <- df %>%
            dplyr::filter(team_code == team,
                          play_type != "IN",
                          play_type != "OUT")
    }
    df <- tibble::rowid_to_column(df, "row_number")
    # Assisted FGM are recorded in the row above the assist row
    assists_idx <- which(df$play_type == "AS")
    fg_idx <- assists_idx - 1
    assists_df <- data.frame(
        passer = df$player_name[assists_idx],
        shooter = df$player_name[fg_idx],
        play_type = as.character(df$play_type[fg_idx]),
        points = 0,
        team_code = df$team_code[fg_idx],
        seconds = df$seconds[fg_idx],
        game_code = df$game_code[fg_idx],
        season = df$season[fg_idx],
        play_number = df$play_number[assists_idx],
        time_remaining = df$time_remaining[fg_idx],
        quarter = df$quarter[fg_idx],
        row_number = df$row_number[assists_idx],
        stringsAsFactors = FALSE
    )

    # We need to find out how many assisted free throws were made
    # We get a df with the rows above and below the assisted FT
    assisted_ft_idx <- assists_df$df_idx[assists_df$play_type == "FTM"]
    assisted_ft <- data.frame(
        three_above = df$play_type[assisted_ft_idx - 3],
        two_above = df$play_type[assisted_ft_idx - 2],
        one_above = df$play_type[assisted_ft_idx - 1],
        assist = df$play_type[assisted_ft_idx],
        one_below = df$play_type[assisted_ft_idx + 1],
        two_below = df$play_type[assisted_ft_idx + 2],
        df_idx = assisted_ft_idx)
    assisted_ft_points <- apply(assisted_ft, 1, function(x) sum(x == "FTM"))

    # Dealing with "and 1s"
    ## TODO: SIMPLIFY/VECTORIZE THIS CODE AT SOME POINT
    and1_key <- assists_df %>%
        dplyr::filter(play_type == "RV" | play_type == "CV") %>%
        dplyr::select(play_type, time_remaining, quarter, game_code)

    and1_df <- df %>%
        dplyr::filter(time_remaining %in% and1_key$time_remaining &
                          quarter %in% and1_key$quarter &
                          game_code %in% and1_key$game_code) %>%
        dplyr::group_by(game_code, quarter, time_remaining) %>%
        dplyr::summarise(fg2 = sum(play_type == "2FGM"),
                         fg3 = sum(play_type == "3FGM"),
                         ft = sum(play_type == "FTM"))

    # The FGM is either a two or a three
    # so from just the 2fg column we can find out the shot type
    fg2 <- and1_df$fg2
    and1_play_type <- dplyr::case_when(fg2 == 1 ~ "2FGM",
                                       fg2 == 0 ~ "3FGM")

    # Should we specify wheter the shot was an and1 in this type of analysis?
    # Perhaps as an option of the function?
    if (and1 == TRUE) {
        and1_play_type <- paste0(and1_play_type, "+1")
        and1_ft <- and1_df$ft
        and1_points <- integer(nrow(and1_df))
        and1_points[and1_play_type == "2FGM+1"] <- 2
        and1_points[and1_play_type == "3FGM+1"] <- 3
        and1_points <- and1_points + and1_ft
    }

    and1_idx <- which(assists_df$play_type == "RV" |
                          assists_df$play_type == "CV")
    assists_df[and1_idx, "play_type"] <- and1_play_type

    # Assign corresponding points to each play type
    assists_df$points[assists_df$play_type == "2FGM"] <- 2
    assists_df$points[assists_df$play_type == "3FGM"] <- 3
    assists_df$points[assists_df$play_type == "FTM"] <- assisted_ft_points
    assists_df$points[and1_idx] <- and1_points

    tibble::as_tibble(assists_df)
}
