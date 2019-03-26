#' Get who assisted who from play-by-play data
#'
#' @param df Play-by-play data frame as produced by extract_pbp()
#'
#' @return A data frame
#' @export
#'
#' @examples
get_assists <- function(df, team) {
    ignored_plays <- c("IN", "OUT", "TOUT", "TOUT_TV")

    df <- df %>%
        dplyr::filter(!(play_type %in% ignored_plays))
    if (!missing(team)) {
        df <- df %>%
            dplyr::filter(team_code == team)
    }
    # Assisted FGM are recorded in the row above the assist row
    assists_idx <- which(df$play_type == "AS")
    fg_idx <- assists_idx - 1
    assists_df <- data.frame(
        passer = df$player_name[assists_idx],
        shooter = df$player_name[fg_idx],
        play_type = as.character(df$play_type[fg_idx]),
        points = 0,
        team_code = df$team_code[fg_idx],
        seconds = df$seconds[fg_idx],
        game_code = df$game_code[fg_idx],
        season = df$season[fg_idx],
        play_number = df$play_number[assists_idx],
        time_remaining = df$time_remaining[fg_idx],
        quarter = df$quarter[fg_idx],
        stringsAsFactors = FALSE
    )

    # We need to find out how many assisted free throws were made
    # We get a df with the rows above and below the assisted FT
    ft_plays <- c("FTM", "RV", "CM")
    ft_key <- assists_df %>%
        dplyr::filter(play_type %in% ft_plays) %>%
        dplyr::select(play_type, seconds, game_code, season)

    game_split <- split(df, list(df$game_code, df$season))

    ft_key_list <- split(ft_key,
                         f = list(ft_key$game_code, ft_key$season),
                         drop = TRUE)
    assisted_ft_games <- game_split[names(game_split) %in% names(ft_key_list)]

    ft_list <- Map(function(pbp, a) pbp[pbp$seconds %in% a$seconds,],
                   pbp = assisted_ft_games, a = ft_key_list)
    ft_df <- do.call("rbind", ft_list) %>%
        dplyr::mutate(seconds = factor(seconds)) %>%
        dplyr::group_by(season, game_code, seconds) %>%
        dplyr::summarise(fta = sum(play_type == "FTM" |
                                       play_type == "FTA"),
                         ftm = sum(play_type == "FTM"),
                         and1 = fta == 1,
                         fg2 = sum(play_type == "2FGM"),
                         fg3 = sum(play_type == "3FGM"),
                         foul = 1) %>%
        dplyr::mutate(shot_type = dplyr::case_when(
            fg2 == 1 ~ "2FG",
            fg3 == 1 ~ "3FG",
            fta == 2 ~ "2FG",
            fta == 3 ~ "3FG"
            )
        )
    assists_df$seconds <- as.character(assists_df$seconds)
    ft_df$seconds <- as.character(ft_df$seconds)
    assists_df <- left_join(assists_df, ft_df,
                            by = c("season", "game_code", "seconds"))

    # NOTE: Any non 2FG or 3FG will be recorded as NA
    assists_df$shot_type[assists_df$play_type == "2FGM"] <- "2FG"
    assists_df$shot_type[assists_df$play_type == "3FGM"] <- "3FG"

    assists_df$foul[is.na(assists_df$foul)] <- 0
    assists_df$ftm[is.na(assists_df$ftm)] <- 0
    assists_df$and1[is.na(assists_df$and1)] <- 0

    assists_df <- assists_df %>%
        dplyr::mutate(points = dplyr::case_when(
            foul == 1 & and1 == 0 ~ ftm,
            and1 == 1 & shot_type == "2FG" ~ 2 + ftm,
            and1 == 1 & shot_type == "3FG" ~ 3 + ftm,
            play_type == "2FGM" ~ 2,
            play_type == "3FGM" ~ 3
        ))

    assists <- assists_df %>%
        dplyr::mutate(
            seconds = as.integer(seconds),
            season = factor(season),
            foul = as.logical(foul),
            shot_type = factor(shot_type)
            ) %>%
        dplyr::select(game_code,
                      season,
                      passer,
                      shooter,
                      shot_type,
                      points,
                      time_remaining,
                      quarter,
                      seconds,
                      foul,
                      and1,
                      ftm)

    tibble::as_tibble(assists)
}

