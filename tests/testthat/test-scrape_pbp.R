context("test-scrape_pbp")

test_that("a wrong game_code throws an error", {
  expect_warning(scrape_pbp(1200, 2018),
                 "Unable to get data for games 1200")
})

test_that("a single game_code works", {
    expect_equal(length(scrape_pbp(172, 2018)), 1)
})

test_that("only good requests are obtained", {
    bad_game_code <- 1200
    good_game_code <- 172
    game_codes <- c(good_game_code, bad_game_code)
    expect_warning(scrape_pbp(game_codes, 2018),
                   "Unable to get data for games 1200")
    expect_equal(length(scrape_pbp(game_codes, 2018)), 1)
})
