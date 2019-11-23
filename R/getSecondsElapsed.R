#' Get seconds elapsed since the start of the game
#'
#' @param quarters Integer vector indicating the quarter
#' @param secs Character vector indicating minute and
#'   seconds left in the quarter (MM:SS)
#'
#' @keywords internal
#'
#' @return
#' @export
#'
#' @examples
getSecondsElapsed <- function(quarters, secs) {
    quarters_idx <- which(quarters <= 4)
    ot_idx <- which(quarters > 4)
    # Create vector with seconds elapsed after each quarter and overtime
    seconds_per_quarter <- 600 * quarters[quarters_idx]
    seconds_per_ot <- 2400 + 300 * (quarters[ot_idx] - 4)
    seconds_var <- c(seconds_per_quarter, seconds_per_ot)
    seconds_remaining_in_q <- as.numeric(lubridate::ms(secs))
    seconds_elapsed <- seconds_var - seconds_remaining_in_q

    seconds_elapsed
}

# Create a variable specifying the seconds elapsed since start of game:
#   Each quarter has 600 seconds
#   Each over time has 300 seconds
# If we have the time remaining in the quarter/overtime
# we need to substract the remaining time in the quarter
# to the total time after each quarter
# That is, if there is 8:20 left in Q2, we transform 8:20 to seconds (500s)
# Then we substract 500 to the seconds after Q2 (1200)
# 1200 - 500 = 700 seconds, which is 11:40
