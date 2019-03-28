#' Get who assisted who from play-by-play data
#'
#' @param df Play-by-play data frame as produced by extract_pbp()
#' @param team A three letter string specifying the team code
#'
#' @return A data frame
#' @export
#'
#' @examples
get_assists <- function(df, team) {
    ignored_plays <- c("IN", "OUT", "TOUT", "TOUT_TV")

    df <- df %>%
        dplyr::filter(!(.data$play_type %in% ignored_plays))
    if (!missing(team)) {
        df <- df %>%
            dplyr::filter(.data$team_code == team)
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
        dplyr::filter(.data$play_type %in% ft_plays) %>%
        dplyr::select(.data$play_type, .data$seconds,
                      .data$game_code, .data$season)

    game_split <- split(df, list(df$game_code, df$season))

    ft_key_list <- split(ft_key,
                         f = list(ft_key$game_code, ft_key$season),
                         drop = TRUE)
    assisted_ft_games <- game_split[names(game_split) %in% names(ft_key_list)]

    ft_list <- Map(function(pbp, a) pbp[pbp$seconds %in% a$seconds,],
                   pbp = assisted_ft_games, a = ft_key_list)
    ft_df <- do.call("rbind", ft_list) %>%
        dplyr::mutate(seconds = factor(.data$seconds)) %>%
        dplyr::group_by(.data$season, .data$game_code, .data$seconds) %>%
        dplyr::summarise(fta = sum(.data$play_type == "FTM" |
                                       .data$play_type == "FTA"),
                         ftm = sum(.data$play_type == "FTM"),
                         and1 = .data$fta == 1,
                         fg2 = sum(.data$play_type == "2FGM"),
                         fg3 = sum(.data$play_type == "3FGM"),
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
    assists_df <- dplyr::left_join(assists_df, ft_df,
                                   by = c("season", "game_code", "seconds"))

    # NOTE: Any non 2FG or 3FG will be recorded as NA
    assists_df$shot_type[assists_df$play_type == "2FGM"] <- "2FG"
    assists_df$shot_type[assists_df$play_type == "3FGM"] <- "3FG"

    assists_df$foul[is.na(assists_df$foul)] <- 0
    assists_df$ftm[is.na(assists_df$ftm)] <- 0
    assists_df$and1[is.na(assists_df$and1)] <- 0

    assists_df <- assists_df %>%
        dplyr::mutate(points = dplyr::case_when(
            .data$foul == 1 & .data$and1 == 0 ~ ftm,
            .data$and1 == 1 & .data$shot_type == "2FG" ~ 2 + ftm,
            .data$and1 == 1 & .data$shot_type == "3FG" ~ 3 + ftm,
            .data$play_type == "2FGM" ~ 2,
            .data$play_type == "3FGM" ~ 3
        ))

    assists <- assists_df %>%
        dplyr::mutate(
            seconds = as.integer(.data$seconds),
            season = factor(.data$season),
            foul = as.logical(.data$foul),
            shot_type = factor(.data$shot_type),
            passer = droplevels(.data$passer),
            shooter = droplevels(.data$shooter)
            ) %>%
        dplyr::select(.data$game_code,
                      .data$season,
                      .data$passer,
                      .data$shooter,
                      .data$shot_type,
                      .data$points,
                      .data$time_remaining,
                      .data$quarter,
                      .data$seconds,
                      .data$foul,
                      .data$and1,
                      .data$ftm)

    tibble::as_tibble(assists)
}

