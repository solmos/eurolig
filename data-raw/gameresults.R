# Scrape all the results for games between 2001 and 2018
# and save the data in the package

# library(eurolig)
library(dplyr, warn.conflicts = FALSE)
library(purrr)
library(readr)

seasons <- 2001:2018

gameresults <- purrr::map_df(seasons, extractResults)

# team_codes <- teaminfo %>%
#     select(season, team_code, team_name)
# gameresults2 <- gameresults %>%
#     left_join(team_codes, by = c("season", team_home = "team_name")) %>%
#     rename(team_code_home = team_code) %>%
#     left_join(team_codes, by = c("season", team_away = "team_name")) %>%
#     rename(team_code_away = team_code)

write_csv(gameresults, "data-raw/gameresults.csv")

usethis::use_data(gameresults, overwrite = TRUE)
