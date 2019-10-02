#' Extract play-by-play data
#'
#' \code{extract_pbp} downloads play-by-play data from Euroleague and returns
#' a tidy data frame.
#'
#'
#' @param game_code An integer specifying the game code
#' @param season An integer specifying the starting year of the desired season
#'
#'
#' @return A tibble
#'
#'
#' @export
#'
#' @examples
extract_pbp <- function(game_code, season) {
    # Scrape data and update game and season codes in case bad requests occurr
    all_data <- scrape_pbp(game_code, season)
    game_code <- attr(all_data, "game_code")
    season_code <- attr(all_data, "season")

    pbp_per_quarter <- lapply(all_data, function(x) x[7:11])
    pbp_raw_list <- lapply(pbp_per_quarter, function(x) do.call("rbind", x))

    # When binding the quarters the row names identify the observation
    #  within each quarter (i.e. FirstQuarter.23)
    quarters_variable <- lapply(pbp_raw_list, rownames)
    # We are only interested in the quarter, not the observation number (.23)
    quarter_alpha <- lapply(quarters_variable,
                             stringr::str_extract,
                             pattern = "[A-z]+")

    recode_quarters <- function(x) {
        v <- dplyr::recode(x,
                           FirstQuarter = 1,
                           SecondQuarter = 2,
                           ThirdQuarter = 3,
                           ForthQuarter = 4,
                           ExtraTime = 5)
        as.factor(v)
    }
    quarter <- lapply(quarter_alpha, recode_quarters)
    pbp_list <- Map(cbind, pbp_raw_list, QUARTER = quarter)
    delete_rownames <- function(x) {
        rownames(x) <- NULL
        x
    }
    pbp_list <- lapply(pbp_list, delete_rownames)
    pbp_list <- Map(cbind, pbp_list,
                    GAMECODE = game_code,
                    SEASONCODE = season_code)

    # We need a variable identifying the home team
    home_team_names <- sapply(all_data, function(x) x$TeamA)
    away_team_names <- sapply(all_data, function(x) x$TeamB)
    pbp_list <- Map(cbind, pbp_list,
                    HOME_TEAM = home_team_names,
                    AWAY_TEAM = away_team_names)

    # Specify the score at every play using helper function add_points
    pbp_list <- lapply(pbp_list, add_points)

    # Dealing with double (or more) overtimes
    recode_overtimes <- function(x) {
        x$QUARTER <- as.integer(x$QUARTER)
        over_times <- sum(x$PLAYTYPE == "BP") - 4
        if (over_times > 1) {
            t <- 45
            period <- 6
            for (i in 1:(over_times - 1)) {
                x$QUARTER[x$MINUTE > t] <- period
                t <- t + 5
                period <- period + 1
            }
        }
        x
    }
    pbp_list <- lapply(pbp_list, recode_overtimes)

    pbp_df <- do.call("rbind", pbp_list)

    # The time when a period/game begins or ends is missing
    begin_quarter_idx <- which(pbp_df$PLAYTYPE == "BP" &
                                   as.integer(pbp_df$QUARTER) <= 4)
    begin_ot_idx <- which(pbp_df$PLAYTYPE == "BP" &
                              as.integer(pbp_df$QUARTER) > 4)
    pbp_df[begin_quarter_idx, "MARKERTIME"] <- "10:00"
    pbp_df[begin_ot_idx, "MARKERTIME"] <- "05:00"
    pbp_df[which(pbp_df$PLAYTYPE == "EP"), "MARKERTIME"] <- "00:00"
    pbp_df[which(pbp_df$PLAYTYPE == "EG"), "MARKERTIME"] <- "00:00"

    # Create a variable specifying the seconds elapsed since start of game:
    #   Each quarter has 600 seconds
    #   Each over time has 300 seconds
    quarters_idx <- which(pbp_df$QUARTER <= 4)
    ot_idx <- which(pbp_df$QUARTER > 4)
    seconds_per_quarter <- 600 * pbp_df$QUARTER[quarters_idx]
    seconds_per_ot <- 2400 + 300 * (pbp_df$QUARTER[ot_idx] - 4)
    s <- c(seconds_per_quarter, seconds_per_ot)
    seconds_remaining_in_q <- as.numeric(lubridate::ms(pbp_df$MARKERTIME))
    seconds_elapsed <- s - seconds_remaining_in_q


    pbp <- pbp_df %>%
        dplyr::transmute(
            game_code = factor(.data$GAMECODE),
            play_number = .data$NUMBEROFPLAY,
            team_code = as.character(trimws(.data$CODETEAM)),
            player_name = as.character(.data$PLAYER),
            play_type = factor(.data$PLAYTYPE),
            time_remaining = .data$MARKERTIME,
            quarter = factor(.data$QUARTER),
            points_home = .data$POINTS_HOME,
            points_away = .data$POINTS_AWAY,
            team_name = as.character(.data$TEAM),
            player_id = factor(trimws(.data$PLAYER_ID), exclude = ""),
            player_dorsal = as.numeric(.data$DORSAL),
            play_info = .data$PLAYINFO,
            seconds = seconds_elapsed,
            home_team = .data$HOME_TEAM,
            away_team = .data$AWAY_TEAM,
            home = as.character(.data$HOME_TEAM) == as.character(.data$TEAM),
            season = as.integer(.data$SEASONCODE)
            )
    tibble::as_tibble(pbp)
}
