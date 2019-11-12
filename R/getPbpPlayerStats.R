getPbpPlayerStats <- function(pbp, player) {
    players <- unique(pbp$player_name)
    players <- players[!is.na(players)]

    pbp_by_player <- purrr::map(players, function(x) pbp[isOnCourt(pbp, x),])
    purrr::map2(pbp_by_player, players, function(x, y) dplyr::bind_cols(x, y))


    purrr::map(pbp_by_player, getPbpPoss)
    pbp %>%
        dplyr::group_by(season, game_code, team_code, player_name) %>%
        dplyr::summarise(
            fg2a = sum(play_type == "2FGA" | play_type == "2FGM"),
            fg2m = sum(play_type == "2FGM"),
            fg2_pct = fg2m / fg2a,
            fg3a = sum(play_type == "3FGA" | play_type == "3FGM"),
            fg3m = sum(play_type == "3FGM"),
            fg3_pct = fg3m / fg3a,
            fga = fg2a + fg3a,
            fgm = fg2m + fg3m,
            fta = sum(play_type == "FTA" | play_type == "FTM"),
            ftm = sum(play_type == "FTM"),
            ft_pct = ftm / fta,
            orb = sum(play_type == "ORB"),
            drb = sum(play_type == "DRB"),
            stl = sum(play_type == "STL"),
            tov = sum(play_type == "TOV"),
            ast = sum(play_type == "AST"),
            cpf = sum(play_type == "CPF"),
            rpf = sum(play_type == "RPF"),
            blk = sum(play_type == "BLK")
        )
}
