#' Get who assisted who from play-by-play data
#'
#' @param df Play-by-play data frame as produced by extract_pbp()
#'
#' @return A data frame
#' @export
#'
#' @examples
get_assists <- function(df) {
    # Assisted FGM are recorded in the row above the assist row
    assists_idx <- which(df$PLAYTYPE == "AS")
    fg_idx <- assists_idx - 1
    assists_df <- data.frame(
        PASSER = df$PLAYER[assists_idx],
        SHOOTER = df$PLAYER[fg_idx],
        PLAYTYPE = df$PLAYTYPE[fg_idx],
        POINTS = 0,
        TEAM = df$TEAM[fg_idx],
        TIME = df$ELAPSEDTIME[fg_idx],
        GAMECODE = df$GAMECODE[fg_idx],
        SEASONCODE = df$SEASONCODE[fg_idx],
        df_idx = assists_idx
    )

    # We need to find out how many assisted free throws were made
    # We get a df with the rows above and below the assisted FT
    assisted_ft_idx <- assists_df$df_idx[assists_df$PLAYTYPE == "FTM"]
    assisted_ft <- data.frame(
        three_above = df$PLAYTYPE[assisted_ft_idx - 3],
        two_above = df$PLAYTYPE[assisted_ft_idx - 2],
        one_above = df$PLAYTYPE[assisted_ft_idx - 1],
        assist = df$PLAYTYPE[assisted_ft_idx],
        one_below = df$PLAYTYPE[assisted_ft_idx + 1],
        two_below = df$PLAYTYPE[assisted_ft_idx + 2],
        df_idx = assisted_ft_idx)
    assisted_ft_points <- apply(assisted_ft, 1, function(x) sum(x == "FTM"))

    # Assign corresponding points to each play type
    assists_df$POINTS[assists_df$PLAYTYPE == "2FGM"] <- 2
    assists_df$POINTS[assists_df$PLAYTYPE == "3FGM"] <- 3
    assists_df$POINTS[assists_df$PLAYTYPE == "FTM"] <- assisted_ft_points
    assists_df
}
