#' Extracts play-by-play data for a single game
#'
#' \code{extractPbp} returns a data frame with the play-by-play data for
#' the specified game.
#'
#' Each game is uniquely identified by the game code and the season. Game codes
#' can be found in the included data set \code{\link{gameresults}} or using
#' \code{\link{extractResults}}.
#'
#' Columns indicating the 10 players that were on the court at each event
#' of the play-by-play data are included by default.
#'
#' @section Warning:
#' Euroleague's API documentation sets a rate limit of 15 secods per request.
#' Take this into consideration when requesting a large number of games.
#'
#' @param game_code Integer scalar specifying the game code
#' @param season Integer scalar specifying the season
#' @param lineups Logical scalar indicating whether to include
#'   lineup information. Defaults to TRUE.
#'
#' @return A play-by-play data frame with the following variables:
#'   \describe{
#'     \item{season}{Starting year of the season}
#'     \item{game_code}{}
#'   }
#' @export
#'
#' @examples
extractPbp <- function(game_code, season, lineups = TRUE) {

    assertthat::assert_that(
        assertthat::is.scalar(game_code),
        assertthat::is.scalar(season),
        msg = "game_code and season must be single integer values.")

    all_data <- scrapePbp(game_code, season)
    # game_code <- all_data$game_code
    # season_code <- all_data$season

    pbp_per_quarter <- all_data[7:11]
    pbp <- do.call(rbind, pbp_per_quarter)

    # When binding the quarters the row names identify the observation
    #  within each quarter (i.e. FirstQuarter.23)
    quarter_ids <- rownames(pbp)
    # We are only interested in the quarter, not the observation number (.23)
    quarters <- quarter_ids %>%
        stringr::str_extract(pattern = "[A-z]+") %>%
        dplyr::recode(
            FirstQuarter = 1,
            SecondQuarter = 2,
            ThirdQuarter = 3,
            ForthQuarter = 4,
            ExtraTime = 5
        )

    pbp$quarter <- quarters
    rownames(pbp) <- NULL

    pbp$game_code <- game_code
    pbp$season <- season

    # We need a variable identifying the home team
    home_team_names <- all_data$TeamA
    away_team_names <- all_data$TeamB
    pbp$home_team <- home_team_names
    pbp$away_team <- away_team_names


    # Specify the score at every play using helper function add_points
    score_df <- makePointsVar(pbp)
    pbp <- cbind(pbp, score_df)

    # We use helper function recodeOvertimes() to properly
    # code double or more overtimes
    over_times <- sum(pbp$PLAYTYPE == "BP") - 4
    if (over_times > 1) {
        # Set minutes and period to start after first over time
        minutes <- 45
        period <- 6
        # Set quarter values after the first overtime (i.e. minutes > 45)
        # to 6, 7, ...
        for (i in 1:(over_times - 1)) {
            pbp$quarter[pbp$MINUTE > minutes] <- period
            minutes <- minutes + 5
            period <- period + 1
        }
    }

    # The time when a period/game begins or ends is missing
    begin_quarter_idx <- which(pbp$PLAYTYPE == "BP" &
                                   as.integer(pbp$quarter) <= 4)
    begin_ot_idx <- which(pbp$PLAYTYPE == "BP" &
                              pbp$quarter > 4)
    pbp[begin_quarter_idx, "MARKERTIME"] <- "10:00"
    pbp[begin_ot_idx, "MARKERTIME"] <- "05:00"
    pbp[pbp$PLAYTYPE == "EP", "MARKERTIME"] <- "00:00"
    pbp[pbp$PLAYTYPE == "EG", "MARKERTIME"] <- "00:00"

    # Create a variable specifying the seconds elapsed since start of game:
    #   Each quarter has 600 seconds
    #   Each over time has 300 seconds
    # If we have the time remaining in the quarter/overtime
    # we need to substract the remaining time in the quarter
    # to the total time after each quarter
    # That is, if there is 8:20 left in Q2, we transform 8:20 to seconds (500s)
    # Then we substract 500 to the seconds after Q2 (1200)
    # 1200 - 500 = 700 seconds, which is 11:40
    quarters_idx <- which(pbp$quarter <= 4)
    ot_idx <- which(pbp$quarter > 4)
    # Create vector with seconds elapsed after each quarter and overtime
    seconds_per_quarter <- 600 * pbp$quarter[quarters_idx]
    seconds_per_ot <- 2400 + 300 * (pbp$quarter[ot_idx] - 4)
    seconds_var <- c(seconds_per_quarter, seconds_per_ot)
    seconds_remaining_in_q <- as.numeric(lubridate::ms(pbp$MARKERTIME))
    seconds_elapsed <- seconds_var - seconds_remaining_in_q

    # Recode PLAYTYPE since some codes are not intuitive
    play_type_recoded <- dplyr::recode(pbp$PLAYTYPE,
                                       TO = "TOV",
                                       ST = "STL",
                                       D  = "DRB",
                                       O  = "ORB",
                                       RV = "RPF",
                                       CM = "CPF",
                                       AS = "AST",
                                       FV = "BLK",
                                       AG = "RBLK")


    pbp_final <- pbp %>%
        tibble::as_tibble() %>%
        dplyr::transmute(
            season = as.integer(.data$season),
            game_code = .data$game_code,
            play_number = .data$NUMBEROFPLAY,
            team_code = trimws(.data$CODETEAM),
            player_name = as.character(.data$PLAYER),
            play_type = play_type_recoded,
            time_remaining = .data$MARKERTIME,
            quarter = .data$quarter,
            points_home = .data$points_home,
            points_away = .data$points_away,
            play_info = .data$PLAYINFO,
            seconds = seconds_elapsed,
            home_team = .data$home_team,
            away_team = .data$away_team,
            home = .data$home_team == .data$TEAM,
            team_name = .data$TEAM
            # Include PLAYER_ID and DORSAL?
        )

    # Replace "" values in team_code
    pbp_final$team_code[pbp_final$team_code == ""] <- NA

    # Add column identifying the last free throw in a trip to the line
    pbp_final$last_ft <- FALSE
    last_fts <- isLastFt(pbp_final)
    pbp_final$last_ft[last_fts] <- TRUE

    # Add column identifying if a free throw is from an "and 1"
    pbp_final$and1 <- FALSE
    and1_idx <- isAnd1(pbp_final)
    pbp_final$and1[and1_idx] <- TRUE

    # Add column indicating when a possesion ends??
    # pbp$possession_end <- getPossEnding(pbp)

    # Add columns indicating players on the floor
    if (lineups == TRUE) {
        pbp_final <- attachLineups(pbp_final)
    }

    pbp_final
}


#' Create home and away points variables
#'
#' Helper function to create points variables. Used to add these variables to output data frame in extract_pbp().
#'
#' @param df A data frame of play-by-play data
#'
#' @return A data frame with home and away points for each action of pbp
#' @export
#'
#' @examples
makePointsVar <- function(df) {
    points_home <- integer(nrow(df))
    points_away <- integer(nrow(df))
    home_team <- unique(df$home_team)
    away_team <- unique(df$away_team)
    points_home[df$PLAYTYPE == "2FGM" & df$TEAM == home_team] <- 2
    points_home[df$PLAYTYPE == "3FGM" & df$TEAM == home_team] <- 3
    points_home[df$PLAYTYPE == "FTM" & df$TEAM == home_team] <- 1
    points_home <- cumsum(points_home)

    points_away[df$PLAYTYPE == "2FGM" & df$TEAM == away_team] <- 2
    points_away[df$PLAYTYPE == "3FGM" & df$TEAM == away_team] <- 3
    points_away[df$PLAYTYPE == "FTM" & df$TEAM == away_team] <- 1
    points_away <- cumsum(points_away)

    data.frame(points_home, points_away)
}
