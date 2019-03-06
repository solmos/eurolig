#' Extract play-by-play data for Euroleague games
#'
#' @param game_code An integer as specified in the game url
#' @param season_code A string as specified in the game url
#'
#' @importFrom magrittr %>%
#'
#' @return A data frame
#' @export
#'
#' @examples
extract_pbp <- function(game_code, season) {
    # Scrape data and update game and season codes in case bad requests occurr
    all_data <- scrape_pbp(game_code, season)
    game_code <- attr(all_data, "game_code")
    season_code <- attr(all_data, "season_code")

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


    pbp_df <- do.call("rbind", pbp_list)

    # The time when a period/game begins or ends is missing
    pbp_df[which(pbp_df$PLAYTYPE == "BP"), "MARKERTIME"] <- "10:00"
    pbp_df[which(pbp_df$PLAYTYPE == "EP"), "MARKERTIME"] <- "00:00"
    pbp_df[which(pbp_df$PLAYTYPE == "EG"), "MARKERTIME"] <- "00:00"

    # Create a variable specifying the seconds elapsed since start of game:
    #   Each quarter has 600 seconds
    #   Each over time has 300 seconds

    seconds_per_quarter <- 600 * as.numeric(pbp_df$QUARTER)
    # TODO: Deal with over times
    seconds_remaining_in_q <- as.numeric(lubridate::ms(pbp_df$MARKERTIME))
    seconds_elapsed <- seconds_per_quarter - seconds_remaining_in_q

    pbp <- pbp_df %>%
        dplyr::mutate(CODETEAM = as.factor(trimws(CODETEAM)),
                      PLAYER_ID = as.factor(trimws(PLAYER_ID)),
                      PLAYTYPE = as.factor(PLAYTYPE),
                      PLAYER = as.factor(PLAYER),
                      TEAM = as.factor(TEAM),
                      DORSAL = as.factor(as.numeric(DORSAL)),
                      ELAPSEDTIME = seconds_elapsed,
                      HOME = as.character(HOME_TEAM) == as.character(TEAM)
                      )

    pbp
}
