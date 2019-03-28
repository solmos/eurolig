#' Plot assists network
#'
#' Uses igpraph to plot a network graph showing who assisted who from play-by-play data.
#'
#' @param pbp_df Data frame with play-by-play data
#' @param team A three letter string specifying the team code
#'
#' @return An igraph network plot
#' @export
#'
#' @examples
plot_network <- function(pbp_df, team, n_min) {
    assist_df <- get_assists(pbp_df, team)
    n_assists <- assist_df %>%
        dplyr::group_by(.data$passer) %>%
        dplyr::count()

    edges <- assist_df %>%
        dplyr::group_by(.data$passer, .data$shooter) %>%
        dplyr::count() %>%
        dplyr::arrange(dplyr::desc(.data$n))

    if (!missing(n_min)) {
        edges <- dplyr::filter(edges, .data$n >= n_min)
    }

    nodes <- assist_df %>%
        dplyr::group_by(.data$passer) %>%
        dplyr::summarise(pts_generated = sum(.data$points, na.rm = TRUE)) %>%
        dplyr::filter(.data$passer %in% unique(edges$passer))

    # We need to add the players with no assists to nodes
    all_node_ids <- unique(c(as.character(edges$passer),
                             as.character(edges$shooter)))
    non_passers <- all_node_ids[!all_node_ids %in% edges$passer]
    non_passers_df <- data.frame(passer = non_passers,
                                 pts_generated = 0)
    nodes <- rbind(nodes, non_passers_df)

    # Shorten names to just the last name of the player
    node_labels <- stringr::str_extract(all_node_ids, "[A-z]+")
    node_labels_df <- data.frame(id = all_node_ids,
                                 label = node_labels,
                                 stringsAsFactors = FALSE)

    nodes <- nodes %>%
        dplyr::transmute(id = as.character(.data$passer),
                         value = .data$pts_generated,
                         tittle = .data$passer) %>%
        dplyr::left_join(node_labels_df, by = "id")

    # Igraph
    net <- igraph::graph_from_data_frame(d = edges,
                                         vertices = nodes,
                                         directed = TRUE)

    igraph::V(net)$size <- nodes$value
    igraph::V(net)$color <- "slategray2"
    igraph::V(net)$frame.color <- "white"
    igraph::V(net)$label.color <- "black"
    igraph::V(net)$label.family <- "Mono"
    igraph::E(net)$width <- edges$n * 2

    plot(net, edge.arrow.size = .3, edge.curved = 0.4)
}
