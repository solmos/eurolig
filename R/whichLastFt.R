#' Find all last free throws in play-by-play data
#'
#' @param pbp Play-by-play data frame
#'
#' @return Integer vector with the indexes of the last free throws
#' @export
#'
#' @examples
whichLastFt <- function(pbp) {
    # Find the time when fts are being shot
    ft_secs <- pbp$seconds[pbp$play_type == "FTA" | pbp$play_type == "FTM"]
    # Filter only the events during those times
    ft_events <- pbp[pbp$seconds %in% ft_secs,]

    ft_stints <- split(ft_events, ft_events$seconds)
    last_ft_play_number <- vapply(ft_stints, whichLastFtInStint, numeric(1))
    which(pbp$play_number %in% last_ft_play_number)
}

#' Find last free throw of a single trip to the foul line
#'
#' @param ft_stint Data frame with all events in a single trip to the line
#'
#' @return Integer scalar indicating the play_number of the last free throw in the stint
#' @export
#'
#' @examples
whichLastFtInStint <- function(ft_stint) {
    ft_idx <- which(ft_stint$play_type == "FTA" | ft_stint$play_type == "FTM")
    last_ft_play_number <- ft_stint$play_number[ft_idx][length(ft_idx)]
    last_ft_play_number
}

whichAnd1InStint <- function(ft_stint) {
    ft_idx <- which(ft_stint$play_type == "FTA" | ft_stint$play_type == "FTM")
    ifelse(length(ft_idx) == 1, ft_stint$play_number[ft_idx], NA)
}

whichAnd1 <- function(pbp) {
    # Find the time when fts are being shot
    ft_secs <- pbp$seconds[pbp$play_type == "FTA" | pbp$play_type == "FTM"]
    # Filter only the events during those times
    ft_events <- pbp[pbp$seconds %in% ft_secs,]

    ft_stints <- split(ft_events, ft_events$seconds)
    last_ft_play_number <- vapply(ft_stints, whichAnd1InStint, numeric(1))
    which(pbp$play_number %in% last_ft_play_number)
}

# pbp %>%
#     filter(play_number %in% find_last_ft(pbp))
#
# pbp$play_type[pbp$play_number %in% find_last_ft(pbp)]
#
#
# pbp$last_ft <- FALSE
# pbp$last_ft[pbp$play_number[find_last_ft(pbp)]] <- TRUE
# pbp[pbp$play_number[find_last_ft(pbp)],]
# x <- split(ft_events, ft_events$seconds)
#
# # bad recorded sequence
# ft_stint <- x$`1673`
#
# idx <- which(y$play_type == "FTA" | y$play_type == "FTM")
# last_ft_play_number <- y$play_number[idx][length(idx)]
# pbp$play_type[pbp$play_number == last_ft_play_number]
#
# y$last_ft <- FALSE
# y$last_ft[y$play_number == last_ft_play_number] <- TRUE
# sum(n_fts$n_fts)
