scrape_pbp <- function(game_code, season_code) {
    base_api <- "https://live.euroleague.net/api/PlayByPlay"
    n_games <- length(game_code)

    if (length(season_code) == 1) {
        season_code <- rep(season_code, n_games)
    } else if (length(season_code) != n_games) {
        warning("season_code must be length 1 or same length as game_code \n")
    }

    api_requests <- vector("list", n_games)

    # euroleague.net/robots.txt specifies a crawl-delay of 15s
    # If the number of requests is small specify a shorter delay
    delay <- ifelse(n_games < 40, 5, 15)

    # ERROR: Sometimes it fails to request when calling curl_fetch_memory()
    if (n_games == 1) {
        api_requests[[1]] <- httr::GET(base_api,
                                       query = list(gamecode = game_code,
                                                    seasoncode = season_code))
    } else {
        for (i in 1:(n_games - 1)) {
            query_list <- list(gamecode = game_code[i],
                               seasoncode = season_code[i])
            cat("Obtaining data for game", game_code[i], "\n")
            api_requests[[i]] <- httr::GET(base_api,
                                           query = query_list)
            Sys.sleep(delay)
        }
        cat("Obtaining data for game", game_code[n_games], "\n")
        query_list <- list(gamecode = game_code[n_games],
                           seasoncode = season_code[n_games])
        api_requests[[n_games]] <- httr::GET(base_api,
                                             query = query_list)
    }

    # Dealing with request errors ====
    # ?For some reason, invalid game codes do not give bad request codes
    errors <- unlist(lapply(api_requests, function(x) length(x$content) == 0))
    good_game_codes <- game_code[!errors]
    if (sum(errors != 0)) {
        bad_game_codes <- paste(game_code[errors], collapse = ", ")
        error_message <- paste("Unable to get data for games", bad_game_codes)
        api_requests <- api_requests[!errors]
        warning(error_message)
    }

    json_data <- lapply(api_requests, httr::content,
                        as = "text", encoding = "UTF-8")
    all_data <- lapply(json_data, jsonlite::fromJSON)
    attr(all_data, "game_code") <- good_game_codes
    attr(all_data, "season_code") <- season_code[!errors]
    all_data
}
