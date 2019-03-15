#' Get who assisted who from play-by-play data
#'
#' @param df Play-by-play data frame as produced by extract_pbp()
#'
#' @return A data frame
#' @export
#'
#' @examples
get_assists <- function(df, team) {
    df <- tibble::rowid_to_column(df, "row_number")
    if (!missing(team)) {
        df <- df %>%
            dplyr::filter(team_code == team,
                          play_type != "IN",
                          play_type != "OUT")
    }
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
        season_code = df$season[fg_idx],
        play_number = df$play_number[assists_idx],
        df_idx = assists_idx,
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
    and1_df <- assists_df %>%
        dplyr::filter(play_type == "RV" | play_type == "CV") %>%
        dplyr::select(df_idx, play_number, game_code, play_type)
    and1_playtype <- character(nrow(and1_df))
    and1_ft <- character(nrow(and1_df))
    for (i in 1:nrow(and1_df)) {
        play_no <- and1_df[i, "play_number"]
        game_no <- and1_df[i, "game_code"]
        # FG is three plays before the assist (after RV and CV)
        fg_idx <- which(df$play_number == (play_no - 3) &
                         df$game_code == game_no)
        and1_playtype[i] <- as.character(df$play_type[fg_idx])
        # FT is one play after the assist
        ft_idx <- fg_idx + 3
        and1_ft[i] <- as.character(df$play_type[ft_idx])
    }
    and1_playtype <- paste0(and1_playtype, "+1")
    and1_idx <- which(assists_df$play_type == "RV" |
                          assists_df$play_type == "CV")
    assists_df[and1_idx, "play_type"] <- and1_playtype
    assists_df$play_type <- factor(assists_df$play_type)

    # Assign corresponding points to each play type
    assists_df$points[assists_df$play_type == "2FGM"] <- 2
    assists_df$points[assists_df$play_type == "3FGM"] <- 3
    assists_df$points[assists_df$play_type == "FTM"] <- assisted_ft_points

    tibble::as_tibble(assists_df)
}

x <- get_assists(pbp, "MAD")
levels(x$play_type)
df[5114:5119,]
which(df$play_number == 487 & df$game_code == 166)
df[4225:4229,]
