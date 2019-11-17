# Sample play-by-play dataset

library(eurolig)
library(dplyr)
library(purrr)
library(readr)

# 2018/2019 Final Four games
game_codes <- 257:260
season <- 2018

samplepbp <- map_df(game_codes, extractPbp, season = season)

write_csv(samplepbp, "data-raw/samplepbp.csv")

usethis::use_data(samplepbp, overwrite = TRUE)
