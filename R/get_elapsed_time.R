get_elapsed_time <- function(df) {
    t_substract <- paste0(as.numeric(pbp_df$QUARTER) * 10, ":00") %>%
        lubridate::ms() %>%
        lubridate::as.duration()
    t_remaining_in_q <- lubridate::ms(pbp_df$MARKERTIME) %>%
        lubridate::as.duration()
    t_elapsed <- lubridate::as.period(t_substract - t_remaining_in_q)
    as.numeric(t_elapsed)
}
