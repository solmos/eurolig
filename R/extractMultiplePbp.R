#' Title
#'
#' @param game_codes Numeric vector of game codes
#' @param season Scalar
#'
#' @keywords internal
#'
#' @return
#' @export
#'
#' @examples
extractMultiplePbp <- function(game_codes, season) {
    pbp_list <- vector("list", length(game_codes))
    for (i in seq_along(game_codes)) {
        pbp_list[[i]] <- try(extractPbp(game_codes[i], season))
        msg <- paste0("Game ", game_codes[i], ", season ", season, ".\n")
        cat(msg)
        Sys.sleep(2)
    }
    pbp <- do.call(rbind, pbp_list)

    pbp
}
