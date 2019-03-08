#' Plot score of a game
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
plot_score <- function(df) {
    ggplot(df) +
        geom_line(aes(ELAPSEDTIME, POINTS_HOME), color = "skyblue") +
        geom_line(aes(ELAPSEDTIME, POINTS_AWAY, color = "coral2"))
        labs(x = "Time (s)", y = "Points") +
        theme_minimal()
}
