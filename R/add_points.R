add_points <- function(df) {
    points_home <- numeric(nrow(df))
    points_away <- numeric(nrow(df))
    home_team <- levels(df$HOME_TEAM)
    away_team <- levels(df$AWAY_TEAM)
    points_home[df$PLAYTYPE == "2FGM" & df$TEAM == home_team] <- 2
    points_home[df$PLAYTYPE == "3FGM" & df$TEAM == home_team] <- 3
    points_home[df$PLAYTYPE == "FTM" & df$TEAM == home_team] <- 1
    POINTS_HOME <- cumsum(points_home)

    points_away[df$PLAYTYPE == "2FGM" & df$TEAM == away_team] <- 2
    points_away[df$PLAYTYPE == "3FGM" & df$TEAM == away_team] <- 3
    points_away[df$PLAYTYPE == "FTM" & df$TEAM == away_team] <- 1
    POINTS_AWAY <- cumsum(points_away)

    cbind(df, POINTS_HOME, POINTS_AWAY)
}
