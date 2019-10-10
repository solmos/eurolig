# library(tidyverse)
#
# pbp <- extract_pbp(4, 2019) %>%
#     get_lineups()


which_last_ft <- function(pbp) {
    # Find the time when fts are being shot
    ft_secs <- pbp$seconds[pbp$play_type == "FTA" | pbp$play_type == "FTM"]
    # Filter only the events during those times
    ft_events <- pbp[pbp$seconds %in% ft_secs,]

    ft_stints <- split(ft_events, ft_events$seconds)
    last_ft_idx <- vapply(ft_stints, which_last_ft_in_stint, numeric(1))
    as.integer(last_ft_idx)
}

which_last_ft_in_stint <- function(ft_stint) {
    ft_idx <- which(ft_stint$play_type == "FTA" | ft_stint$play_type == "FTM")
    last_ft_play_number <- ft_stint$play_number[ft_idx][length(ft_idx)]
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
