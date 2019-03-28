#' Plot assist heatmap
#'
#' \code{plot_heatmap} creates a heatmap indicating who assists who
#'
#' @param pbp Play-by-play data frame
#' @param team A three letter string specifying the team code
#'
#' @return A ggplot heatmap
#' @export
#'
#' @examples
plot_heatmap <- function(pbp, team) {
    assists <- get_assists(pbp, team)

    # Make passer and shooter factors have the same levels so that
    # we get all combinations with tidyr::complete()
    player_levels <- unique(c(levels(assists$passer), levels(assists$shooter)))
    levels(assists$passer) <- player_levels
    levels(assists$shooter) <- player_levels

    # We want to include number of assists in the diagonal of our heatmap
    n_assists <- assists %>%
        dplyr::group_by(.data$passer) %>%
        dplyr::summarise(assists = dplyr::n())
    levels(n_assists$passer) <- player_levels

    coordinates <- tibble::tibble(
        passer = factor(player_levels, levels = player_levels),
        x = seq_along(player_levels),
        y = seq_along(player_levels)
    ) %>%
        dplyr::left_join(n_assists, by = "passer") %>%
        dplyr::mutate(assists = tidyr::replace_na(.data$assists, 0))

    assists_comb <- assists %>%
        dplyr::group_by(.data$passer, .data$shooter) %>%
        dplyr::count() %>%
        tidyr::complete(.data$passer, .data$shooter) %>%
        dplyr::mutate(n = tidyr::replace_na(.data$n, 0)) %>%
        dplyr::left_join(n_assists, by = "passer") %>%
        dplyr::mutate(
            passer_short = stringr::str_extract(.data$passer, "[A-z]+"),
            shooter_short = stringr::str_extract(.data$shooter, "[A-z]+"))
    assists_comb$n[assists_comb$passer == assists_comb$shooter] <- NA
    assists_comb$assists[assists_comb$passer != assists_comb$shooter] <- NA

    ggplot(assists_comb) +
        geom_tile(aes_(~shooter_short, ~passer_short, fill = ~n)) +
        scale_fill_gradient(na.value = "whitesmoke",
                            guide = guide_colorbar(title = "Assists")) +
        annotate("text", coordinates$x, coordinates$y,
                 label = coordinates$assists) +
        coord_fixed() +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            plot.background = element_rect(fill = "whitesmoke"),
            legend.background = element_rect(fill = "whitesmoke"),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            panel.background = element_blank(),
            panel.border = element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_blank()
        ) +
        ggplot2::labs(x = "Shooter",
                      y = "Passer")
}
