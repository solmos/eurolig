
get_stint_stats <- function(pbp, player) {
    team <- pbp$team_code[which(pbp$player_name == player)[1]]
    pbp %>%
        dplyr::filter(stringr::str_detect(lineups, player)) %>%
        dplyr::mutate(
            off_def = ifelse(team_code == team, "Offense", "Defense")
            ) %>%
        dplyr::group_by(off_def) %>%
        dplyr::summarise(
            m_2fg = sum(play_type == "2FGM"),
            a_2fg = sum(play_type == "2FGA") + m_2fg,
            m_3fg = sum(play_type == "3FGM"),
            a_3fg = sum(play_type == "3FGA") + m_3fg,
            m_ft = sum(play_type == "FTM"),
            a_ft = sum(play_type == "FTA") + m_ft,
            orb = sum(play_type == "O"),
            tov = sum(play_type == "TO"),
            # TODO: Need a way to count actual possesions, not an estimate
            poss = a_2fg + a_3fg - orb + tov + 0.4 * a_ft,
            pts = 2 * m_2fg + 3 * m_3fg + m_ft
        )
}

get_pm <- function(pbp, player) {
    stats <- get_stint_stats(pbp, player)
    # Change when actual possesions are computed
    poss_mean <- mean(stats$poss)
    stats %>%
        dplyr::select(off_def, pts) %>%
        tidyr::spread(off_def, pts) %>%
        dplyr::transmute(
            pts_scored = Offense,
            pts_allowed = Defense,
            poss = poss_mean,
            ppp_scored = pts_scored / poss,
            ppp_allowed = pts_allowed / poss,
            pm = pts_scored - pts_allowed,
            pm_pp = ppp_scored - ppp_allowed
        )
}


