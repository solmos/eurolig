#' Euroleague game results (2001-2018).
#'
#' Each game is uniquely idendified by the season and game code.
#'
#' @format A data frame with 4173 rows and 13 variables:
#' \describe{
#'   \item{season}{Year the season started.}
#'   \item{phase}{Phase of the season: regular season (RS), top 16 (TS),
#'     Playoffs (PO), and Final Four (FF).}
#'   \item{round_name}{The round within a phase.}
#'   \item{team_home}{Full team name of the home team.}
#'   \item{points_home}{Points scored by the home team.}
#'   \item{team_away}{Full team name of the away team.}
#'   \item{points_away}{Points scored by the away team.}
#'   \item{game_code}{Game code partially identifying the game.}
#'   \item{date}{Date of the game.}
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
