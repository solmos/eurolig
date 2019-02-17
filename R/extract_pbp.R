extract_pbp <- function(game_code, season_code) {
    base_api <- "https://live.euroleague.net/api/PlayByPlay"
    n_games <- length(game_code)
    output <- vector("list", n_games)
    for (i in 1:n_games) {
        output[[i]] <- httr::GET(base_api,
                                 query = list(gamecode = game_code[i],
                                              seasoncode = season_code[i]))
        cat(paste("Obtaining data for game", game_code[i]))
        Sys.sleep(15)
        }

}

game_code <- 172:173
season_code <- rep("E2018", 2)
