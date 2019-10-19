# Scrape all the results for games between 2001 and 2018
# and save the data in the package

library(dplyr, warn.conflicts = FALSE)
library(purrr)
library(readr)

seasons <- 2001:2018

gameresults <- purrr::map_df(seasons, extractResults)

write_csv(gameresults, "data-raw/gameresults.csv")

usethis::use_data(gameresults, overwrite = TRUE)
