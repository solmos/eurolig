extractShots <- function(game_code, season) {
    shots_raw <- requestShots(game_code, season) %>%
        tibble::as_tibble()
    colnames(shots_raw) <- tolower(colnames(shots_raw))
    shots <- shots_raw %>%
        dplyr::rename(
            team_code = .data$team,
            player_id = .data$id_player,
            player_name = .data$player,
            action_id = .data$id_action
        ) %>%
        dplyr::mutate(
            coord_x = .data$coord_x / 100,
            coord_y = .data$coord_y / 100 + 1.575,
            team_code = trimws(.data$team_code),
            player_id = trimws(.data$player_id)
        ) %>%
        dplyr::filter(.data$action_id != "FTM")

    # TODO: Sometimes regulation ends with 41?
    quarters <- cut(shots$minute, c(0, 10, 20, 30, 40), labels = 1:4)

    # TODO: How to deal with OTs
    if (max(shots$minute > 40)) {
        cuts <- seq(40, max(shots$minute), by = 5)
        # how to deal with OT
        quarters <- quarters
    }
    shots$quarter <- quarters
}
