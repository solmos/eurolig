# Data sets used by functions in the package

# library(eurolig)
library(dplyr)
library(readr)

data("gameresults")
data("teaminfo")

allgames <- gameresults %>%
    select(season,
           game_code,
           team_code_home,
           team_code_away,
           points_home,
           points_away,
           date)

allteams <- teaminfo

usethis::use_data(allgames, allteams, internal = TRUE, overwrite = TRUE)
