extract_pbp <- function(game_code, season_code) {
    # Data scraping ----
    base_api <- "https://live.euroleague.net/api/PlayByPlay"
    n_games <- length(game_code)
    api_requests <- vector("list", n_games)
    for (i in 1:(n_games - 1)) {
        api_requests[[i]] <- httr::GET(base_api,
                                       query = list(gamecode = game_code[i],
                                                    seasoncode = season_code[i]
                                                    )
                                       )
        cat(paste("Obtaining data for game", game_code[i], "\n"))
        Sys.sleep(15)
    }
    cat(paste("Obtaining data for game", game_code[n_games], "\n"))
    api_requests[[n_games]] <- httr::GET(base_api,
                                         query = list(
                                             gamecode = game_code[n_games],
                                             seasoncode = season_code[n_games]
                                             )
                                         )

    json_data <- lapply(api_requests, httr::content,
                           as = "text", encoding = "UTF-8")
    all_data <- lapply(json_data, jsonlite::fromJSON)

    # Data wrangling ----
    pbp_per_quarter <- lapply(all_data, function(x) x[7:11])
    pbp_raw_list <- lapply(pbp_per_quarter, function(x) do.call("rbind", x))

    # When binding the quarters the row names identify the observation
    #  within each quarter (i.e. FirstQuarter.23)
    quarters_variable <- lapply(pbp_raw_list, rownames)
    # We are only interested in the quarter, not the observation number (.23)
    quarter_alpha <- lapply(quarters_variable,
                             stringr::str_extract,
                             pattern = "[A-z]+")
    recode_quarters <- function(x) {
        v <- dplyr::recode(x,
                           FirstQuarter = 1,
                           SecondQuarter = 2,
                           ThirdQuarter = 3,
                           ForthQuarter = 4,
                           ExtraTime = 5)
        as.factor(v)
    }
    quarter <- lapply(quarter_alpha, recode_quarters)
    pbp_list <- Map(cbind, pbp_raw_list, QUARTER = quarter)
    delete_rownames <- function(x) {
        rownames(x) <- NULL
        x
    }
    pbp_list <- lapply(pbp_list, delete_rownames)
    pbp_list <- Map(cbind, pbp_list, GAMECODE = game_code, SEASONCODE = season_code)
    pbp_df <- do.call("rbind", pbp_list)

    # The time when a period/game begins or ends is missing
    pbp_df[which(pbp_df$PLAYTYPE == "BP"), "MARKERTIME"] <- "10:00"
    pbp_df[which(pbp_df$PLAYTYPE == "EP"), "MARKERTIME"] <- "00:00"
    pbp_df[which(pbp_df$PLAYTYPE == "EG"), "MARKERTIME"] <- "00:00"

    pbp <- pbp_df %>%
        dplyr::mutate(CODETEAM = as.factor(trimws(CODETEAM)),
                      PLAYER_ID = as.factor(trimws(PLAYER_ID)),
                      PLAYTYPE = as.factor(PLAYTYPE),
                      PLAYER = as.factor(PLAYER),
                      TEAM = as.factor(TEAM),
                      DORSAL = as.factor(as.numeric(DORSAL))
                      )
    pbp_df
}

library(tidyverse)
game_code <- 172:173
season_code <- rep("E2018", 2)
x <- extract_pbp(game_code, season_code)
data_tibble <- as_tibble(x)
data_tibble
data_tibble$CODETEAM
str(x)
pbp <- x %>%
    dplyr::mutate(CODETEAM = factor(trimws(CODETEAM), exclude = ""),
                  PLAYER_ID = factor(trimws(PLAYER_ID), exclude = ""),
                  PLAYTYPE = factor(PLAYTYPE),
                  PLAYER = factor(PLAYER),
                  TEAM = factor(TEAM),
                  DORSAL = as.numeric(DORSAL)
    )
str(pbp)
levels(pbp$PLAYER_ID)
filter(pbp, PLAYER_ID == "CO_B")
pbp[is.na(pbp$PLAYER),]
is.na(pbp$PLAYER_ID)
pbp$PLAYER_ID
