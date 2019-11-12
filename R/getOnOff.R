getOnOff <- function(pbp, players) {
    teams <- unique(pbp$team_code)
    teams <- teams[teams != ""]
    team <- unique(pbp$team_code[pbp$player_name %in% players])
    # TODO: assert_that(team is scalar)
    opp_team <- teams[teams != team]
    on_court_idx <- isOnCourt(pbp, players)
    pbp_on <- pbp[on_court_idx,]
    pbp_off <- pbp[!on_court_idx,]
    stats_on <- getStintStats(pbp_on, team = team) %>%
        dplyr::mutate(
            players = paste(players, collapse = " - "),
            on_court = TRUE
        )
    stats_off <- getStintStats(pbp_off, team = team) %>%
        dplyr::mutate(
            players = paste(players, collapse = " - "),
            on_court = FALSE
        )

    dplyr::bind_rows(stats_on, stats_off) %>%
        dplyr::select(
            .data$season,
            .data$game_code,
            .data$players,
            .data$on_court,
            .data$off_def,
            .data$team_code,
            dplyr::everything()
        )
}

getOnOffStats <- function(pbp, players) {
    pbp_on <- pbp[isOnCourt(pbp, players),]
    pbp_off <- pbp[!isOnCourt(pbp, players),]
    team <- unique(pbp$team_code[pbp$player_name %in% players])
    players_var <- paste(players, collapse = " - ")
    stats_on <- getPbpStats(pbp_on) %>%
        dplyr::mutate(
            on = TRUE,
            players = players_var,
            type = ifelse(.data$team_code == team, "offense", "defense")
        )
    # stats_on$on_off <- "on"
    stats_off <- getPbpStats(pbp_off) %>%
        dplyr::mutate(
            on = FALSE,
            players = players_var,
            type = ifelse(.data$team_code == team, "offense", "defense")
        )
    # stats_off$on_off <- "off"
    on_off_stats <- dplyr::bind_rows(stats_on, stats_off) %>%
        dplyr::mutate(
            players = players_var,
            type = ifelse(.data$team_code == team, "offense", "defense")
        ) %>%
        dplyr::select(
            .data$season,
            .data$game_code,
            .data$players,
            .data$on,
            .data$type,
            .data$team_code,
            .data$home,
            dplyr::everything()
        )

    on_off_stats
}
