# Sample shot location data frame
library(eurolig)
library(dplyr)
library(purrr)
library(readr)

# 2018/2019 Final Four games
game_codes <- 257:260
season <- 2018

sampleshots <- map_df(game_codes, extractShots, season = season)

write_csv(sampleshots, "data-raw/sampleshots.csv")
usethis::use_data(sampleshots, overwrite = TRUE)
