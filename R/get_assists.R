#' Get who assisted who from play-by-play data
#'
#' @param df Play-by-play data frame as produced by extract_pbp()
#'
#' @return A data frame
#' @export
#'
#' @examples
get_assists <- function(df, team) {
    df <- df %>%
        dplyr::filter(play_type != "IN" & play_type != "OUT")
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
        foul = 0,
        and1 = 0,
        and1_made = 0,
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
    ft_key <- assists_df %>%
        dplyr::filter(play_type == "FTM") %>%
        dplyr::select(play_type, seconds, game_code, season)

    if (nrow(ft_key) > 0) {
        ft_df <- df %>%
            dplyr::filter(
                season %in% ft_key$season &
                    game_code %in% ft_key$game_code &
                    seconds %in% ft_key$seconds
            ) %>%
            dplyr::mutate(seconds = factor(seconds)) %>%
            dplyr::group_by(season, game_code, seconds) %>%
            dplyr::summarise(n_ft = sum(play_type == "FTM" |
                                            play_type == "FTA"),
                             points = sum(play_type == "FTM"))
        ft_play_type <- dplyr::case_when(
            ft_df$n_ft == 2 ~ "2FG",
            ft_df$n_ft == 3 ~ "3FG"
        )

        ft_idx <- which(assists_df$play_type == "FTM")
        assists_df[ft_idx, "play_type"] <- ft_play_type
        assists_df[ft_idx, "foul"] <- 1
        assists_df[ft_idx, "points"] <- ft_df$points
    }

    # Dealing with "and 1s"
    and1_key <- assists_df %>%
        dplyr::filter(play_type == "RV" | play_type == "CM") %>%
        dplyr::select(play_type, seconds, game_code, season)

    if (nrow(and1_key) > 0) {
        and1_df <- df %>%
            dplyr::filter(
                seconds %in% and1_key$seconds &
                    game_code %in% and1_key$game_code &
                    season %in% and1_key$season
            ) %>%
            dplyr::mutate(seconds = factor(seconds)) %>%
            dplyr::group_by(season, game_code, seconds) %>%
            dplyr::summarise(fg2 = sum(play_type == "2FGM"),
                             fg3 = sum(play_type == "3FGM"),
                             ft_made = sum(play_type == "FTM"))

        # The FGM is either a two or a three
        # so from just the 2fg column we can find out the shot type
        fg2 <- and1_df$fg2
        and1_play_type <- dplyr::case_when(fg2 == 1 ~ "2FGM",
                                           fg2 == 0 ~ "3FGM")
        and1_made <- and1_df$ft_made

        and1_idx <- which(assists_df$play_type == "RV" |
                              assists_df$play_type == "CM")
        assists_df[and1_idx, "play_type"] <- and1_play_type
        assists_df[and1_idx, "foul"] <- 1
        assists_df[and1_idx, "and1"] <- 1
        assists_df[and1_idx, "and1_made"] <- and1_made
    }

    # Assign corresponding points to each play type
    assists_df$points[assists_df$play_type == "2FGM"] <- 2
    assists_df$points[assists_df$play_type == "3FGM"] <- 3

    assists_df <- tibble::as_tibble(assists_df) %>%
        dplyr::mutate(
            foul = as.logical(foul),
            and1 = as.logical(and1),
            points = points + and1_made,
            team_code = droplevels(team_code),
            season = factor(season)
            ) %>%
        dplyr::select(-and1_made)
    assists_df$play_type[assists_df$play_type == "2FGM"] <- "2FG"
    assists_df$play_type[assists_df$play_type == "3FGM"] <- "3FG"
    assists_df$play_type <- factor(assists_df$play_type)

    assists_df
}

