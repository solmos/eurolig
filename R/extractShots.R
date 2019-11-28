#' Extract shot location data for a single game
#'
#' \code{extractShots} returns a data frame with coordinate location data on
#' every shot taken in the specified game.
#'
#' @param game_code Integer scalar
#' @param season Integer scalar
#'
#' @return Data frame with 25 variables:
#'   \describe{
#'     \item{season}{Starting year of the season}
#'     \item{game_code}{Game code}
#'     \item{num_anot}{Not sure!}
#'     \item{team_code}{Team code of player taking the shot}
#'     \item{player_id}{Player ID}
#'     \item{player_name}{Full player name (Last name, First name)}
#'     \item{action_id}{Shot type and outcome}
#'     \item{action}{Full description of action_id}
#'     \item{points}{Points added with the shot}
#'     \item{coord_x, coord_y}{x-y coordinates}
#'     \item{zone}{Zone on the court}
#'     \item{fastbreak}{Was the shot taken on a fastbreak?}
#'     \item{second_chance}{Was the shot taken after an offensive rebound?}
#'     \item{off_turnover}{Was the shot taken after a turnover from the other team?}
#'     \item{minute}{Minute in the game}
#'     \item{console}{Time remaining in the shot clock}
#'     \item{points_a}{Points scored by Team A after the shot was taken}
#'     \item{points_b}{Points scored by Team B after the shot was taken}
#'     \item{utc}{Datetime when the shot was logged}
#'     \item{make}{Did the shot go in?}
#'     \item{quarter}{Quarter}
#'     \item{seconds}{Seconds elapsed since the start of the game}
#'     \item{team_code_a}{Team A code}
#'     \item{team_code_b}{Team B code}
#'   }
#' @export
#'
#' @examples
extractShots <- function(game_code, season) {
    shots_raw <- requestShots(game_code, season) %>%
        tibble::as_tibble()
    colnames(shots_raw) <- tolower(colnames(shots_raw))
    shots <- shots_raw %>%
        dplyr::rename(
            team_code = .data$team,
            player_id = .data$id_player,
            player_name = .data$player,
            action_id = .data$id_action,
            off_turnover = .data$points_off_turnover
        ) %>%
        dplyr::mutate(
            # Transform coordinates to fit into court plot
            # Find the right denominator (98 perhaps?)
            coord_x = .data$coord_x / 98,
            coord_y = .data$coord_y / 98 + 1.575,
            team_code = trimws(.data$team_code),
            player_id = trimws(.data$player_id),
            make = .data$points != 0,
            fastbreak = as.logical(as.integer(.data$fastbreak)),
            second_chance = as.logical(as.integer(.data$second_chance)),
            off_turnover = as.logical(as.integer(.data$off_turnover))
        ) %>%
        dplyr::filter(.data$action_id != "FTM")

    cuts_regular <- seq(0, 40, by = 10)
    quarters <- cut(shots$minute, c(0, 10, 20, 30, 40), labels = 1:4)

    # How to deal with OTs
    minute_max <- max(shots$minute)
    if (max(shots$minute > 40)) {
        cuts_ot <- seq(45, max(shots$minute), by = 5)
        cuts_all <- c(cuts_regular, cuts_ot)
        quarters <- cut(
            shots$minute,
            cuts_all,
            labels = 1:(length(cuts_all) - 1)
        )
    }

    # Quarter as integer or factor??
    quarters <- as.integer(quarters)
    shots$quarter <- quarters

    seconds <- getSecondsElapsed(shots$quarter, shots$console)
    shots$seconds <- seconds

    # Find which team is A and B
    team_a_idx <- which(shots$points_a != 0)[1]
    team_b_idx <- which(shots$points_b != 0)[1]
    team_code_a <- shots$team_code[team_a_idx]
    team_code_b <- shots$team_code[team_b_idx]
    shots$team_code_a <- team_code_a
    shots$team_code_b <- team_code_b

    shots <- shots %>%
        dplyr::mutate(
            season = season,
            game_code = game_code
        ) %>%
        dplyr::select(.data$season, .data$game_code, dplyr::everything())

    shots
}
