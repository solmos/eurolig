#' Euroleague game results (2001-2018)
#'
#' Each game is uniquely idendified by the season and game code.
#'
#' @format A data frame with 4,173 rows and 13 variables:
#' \describe{
#'   \item{season}{Year the season started.}
#'   \item{phase}{Phase of the season: regular season (RS), top 16 (TS),
#'     Playoffs (PO), and Final Four (FF).}
#'   \item{team_home}{Full team name of the home team.}
#'   \item{points_home}{Points scored by the home team.}
#'   \item{team_away}{Full team name of the away team.}
#'   \item{points_away}{Points scored by the away team.}
#'   \item{game_code}{Game code partially identifying the game.}
#'   \item{date}{Date of the game.}
#'   \item{round_name}{The round within a phase.}
#'   \item{round_code}{Round code.}
#'   \item{game_url}{Game url.}
#'   \item{team_code_home}{Team code of home team.}
#'   \item{team_code_away}{Team code of away team.}
#' }
#' @source \url{https://www.euroleague.net/}
"gameresults"

#' Euroleague teams (2001-2019)
#'
#' @format A data frame with 434 rows and 5 variables:
#' \describe{
#'   \item{season}{Year the season started.}
#'   \item{team_code}{Team code.}
#'   \item{team_name}{Full team name.}
#'   \item{team_link}{Team URL in Euroleague's site.}
#'   \item{team_img}{Link to image file of team logo.}
#' }
#' @source \url{https://www.euroleague.net/}
"teaminfo"

#' Euroleague 2018/2019 Final Four play-by-play dataset
#'
#' @format A play-by-play data frame with 2,121 rows and 29 variables:
#'   \describe{
#'     \item{season}{Starting year of the season}
#'     \item{game_code}{Game code}
#'     \item{play_number}{Identifying event number}
#'     \item{team_code}{Team code of involved player}
#'     \item{player_name}{Full player name (Last name, First name)}
#'     \item{play_type}{Play type. See Details.}
#'     \item{time_remaining}{Time remaining in the quarter}
#'     \item{quarter}{Quarter}
#'     \item{points_home}{Home team's score}
#'     \item{points_away}{Away team's score}
#'     \item{play_info}{Event description}
#'     \item{seconds}{Seconds elapsed since the start of the game}
#'     \item{home_team}{Full name of home team}
#'     \item{away_team}{Full name of away team}
#'     \item{home}{Whether player involved plays at home}
#'     \item{team_name}{Full team name of involved player}
#'     \item{last_ft}{Whether event corresponds to a last free throw in a trip to the line}
#'     \item{and1}{Whether event corresponds to an additional free throw}
#'     \item{home_player1:home_player5}{Home players on the floor}
#'     \item{away_player1:away_player5}{Away players on the floor}
#'     \item{lineups}{All 10 players on the floor}
#'   }
#'
#' @source \url{https://www.euroleague.net/}
"samplepbp"

#' Euroleague 2018/2019 Final Four shot location dataset
#'
#' @format A shot location data frame with 490 rows and 25 variables:
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
#'     \item{points_off_turnover}{Was the shot taken after a turnover from the other team?}
#'     \item{minute}{Minute in the game}
#'     \item{console}{Time remaining in the shot clock}
#'     \item{points_a}{Points scored by Team A after the shot was taken}
#'     \item{points_b}{Points scored by Team B after the shot was taken}
#'     \item{utc}{Datetime when the shot was logged}
#'     \item{quarter}{Quarter}
#'     \item{seconds}{Seconds elapsed since the start of the game}
#'     \item{make}{Did the shot go in?}
#'     \item{team_code_a}{Team A code}
#'     \item{team_code_b}{Team B code}
#'   }
#'
#' @source \url{https://www.euroleague.net/}
"sampleshots"
