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
        TEAM = df$TEAM[fg_idx],
        TIME = df$ELAPSEDTIME[fg_idx],
        GAMECODE = df$GAMECODE[fg_idx],
        SEASONCODE = df$SEASONCODE[fg_idx]
    )
    assists_df
}
