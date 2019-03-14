#' Get who assisted who from play-by-play data
#'
#' @param df Play-by-play data frame as produced by extract_pbp()
#'
#' @return A data frame
#' @export
#'
#' @examples
get_assists <- function(df, team) {
    if (!missing(team)) {
        df <- dplyr::filter(df, team_code == team)
    }
    # Assisted FGM are recorded in the row above the assist row
    assists_idx <- which(df$play_type == "AS")
    fg_idx <- assists_idx - 1
    assists_df <- data.frame(
        passer = df$player_name[assists_idx],
        shooter = df$player_name[fg_idx],
        play_type = df$play_type[fg_idx],
        points = 0,
        team_code = df$team_code[fg_idx],
        seconds = df$seconds[fg_idx],
        game_code = df$game_code[fg_idx],
        season_code = df$season[fg_idx],
        df_idx = assists_idx
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

    # Assign corresponding points to each play type
    assists_df$points[assists_df$play_type == "2FGM"] <- 2
    assists_df$points[assists_df$play_type == "3FGM"] <- 3
    assists_df$points[assists_df$play_type == "FTM"] <- assisted_ft_points

    tibble::as_tibble(assists_df)
}
