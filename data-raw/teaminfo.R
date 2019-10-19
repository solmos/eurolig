## Scrape information of all teams for all seasons (2001-2019)
## and save the data in the package

library(purrr)
library(readr)

seasons <- 2001:2019

teaminfo <- map_df(seasons, extractTeams)

write_csv(teaminfo, "data-raw/teaminfo.csv")
usethis::use_data(teaminfo, overwrite = TRUE)
