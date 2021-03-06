% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extractTeamResults.R
\name{extractTeamResults}
\alias{extractTeamResults}
\title{Extract results for a team}
\usage{
extractTeamResults(team, season)
}
\arguments{
\item{team}{A three letter string specifying the team code}

\item{season}{An integer specifying the starting year of the desired season}
}
\value{
A tibble
}
\description{
Get results of the games of a given team and season
}
\examples{
extractTeamResults(team = "BAR", season = 2017)
}
